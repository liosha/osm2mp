#!/usr/bin/perl -w

use strict;

use LWP::UserAgent;
use Getopt::Long;
use XML::Simple;
use List::Util qw{ min max sum };
use List::MoreUtils qw{ first_index };
use IO::Uncompress::Gunzip qw{ gunzip $GunzipError };
use File::Slurp;

use YAML::Any qw/ Dump LoadFile /;


####    Settings

my $api  = 'http://www.openstreetmap.org/api/0.6';
my $proxy;

my $onering = 0;
my $noinner = 0;
my $alias_config = 'aliases.yml';

my $filename;
my $outfile;


####    Command-line

GetOptions (
    'file=s'    => \$filename,
    'o=s'       => \$outfile,
    'onering!'  => \$onering,
    'noinner!'  => \$noinner,
    'proxy=s'   => \$proxy,
    'aliases=s' => \$alias_config,
);

unless ( @ARGV ) {
    print "Usage:  getbound.pl [options] <relation> [<relation> ...]\n\n";
    print "relation - id or alias\n\n";
    print "Available options:\n";
    print "     -o <file>       - output filename (default: STDOUT)\n";
    print "     -proxy <host>   - use proxy\n";
    print "     -onering        - merge rings\n\n";
    exit;
}


####    Aliases

my ($rename) = eval{ LoadFile $alias_config };
unless ( $rename ) {
    warn "Unable to load aliases from $alias_config: $@" if $alias_config;
    $rename = {};
}



####    Process

my @rel_ids = map { $rename->{$_} // $_ } @ARGV;
my @osmdata;

# getting
if ( $filename ) {
    push @osmdata, read_file $filename;
}
else {
    for my $rel_id ( @rel_ids ) {
        print STDERR "Downloading RelID=$rel_id..";

        my $ua = LWP::UserAgent->new();
        $ua->proxy( 'http', $proxy ) if $proxy;
        $ua->default_header('Accept-Encoding' => 'gzip');
        $ua->timeout( 300 );

        my $req = HTTP::Request->new( GET => "$api/relation/$rel_id/full" );
        my $res;

        for my $attempt ( 1 .. 10 ) {
            print STDERR q{.};
            $res = $ua->request($req);
            last if $res->is_success;
        }

        unless ( $res->is_success ) {
            print STDERR "Failed\n";
            exit;
        }

        print STDERR "  Ok\n";
        gunzip \($res->content) => \my $reldata;
        push @osmdata, $reldata;
    }
}


# parsing and preloading
my %osm;
for my $osmdata ( @osmdata ) {
    my $osm = XMLin( $osmdata,
            ForceArray  => 1,
            KeyAttr     => [ 'id', 'k' ],
        );
    $osm{node}      = { %{$osm{node}     // {}},  %{$osm->{node}} };
    $osm{way}       = { %{$osm{way}      // {}},  %{$osm->{way}} };
    $osm{relation}  = { %{$osm{relation} // {}},  %{$osm->{relation}} };
}


# connecting rings
my %role = (
    ''          => 'outer',
    'outer'     => 'outer',
    'border'    => 'outer',
    'exclave'   => 'outer',
    'inner'     => 'inner',
    'enclave'   => 'inner',
);

my %result;


for my $rel_id ( @rel_ids ) {
    my $relation = $osm{relation}->{$rel_id};
    my %ring;

    for my $member ( @{ $relation->{member} } ) {
        next unless $member->{type} eq 'way';
        my $role = $role{ $member->{role} }  or next;
    
        unless ( exists $osm{way}->{$member->{ref}} ) {
            print STDERR "Incomplete data: way $member->{ref} is missing\n";
            next;
        }

        push @{ $ring{$role} },  [ map { $_->{ref} } @{$osm{way}->{$member->{ref}}->{nd}} ];
    }

    while ( my ( $type, $list_ref ) = each %ring ) {
        while ( @$list_ref ) {
            my @chain = @{ shift @$list_ref };
        
            if ( $chain[0] eq $chain[-1] ) {
                push @{$result{$type}}, [@chain];
                next;
            }

            my $pos = first_index { $chain[0] eq $_->[0] } @$list_ref;
            if ( $pos > -1 ) {
                shift @chain;
                $list_ref->[$pos] = [ (reverse @chain), @{$list_ref->[$pos]} ];
                next;
            }
            $pos = first_index { $chain[0] eq $_->[-1] } @$list_ref;
            if ( $pos > -1 ) {
                shift @chain;
                $list_ref->[$pos] = [ @{$list_ref->[$pos]}, @chain ];
                next;
            }
            $pos = first_index { $chain[-1] eq $_->[0] } @$list_ref;
            if ( $pos > -1 ) {
                pop @chain;
                $list_ref->[$pos] = [ @chain, @{$list_ref->[$pos]} ];
                next;
            }
            $pos = first_index { $chain[-1] eq $_->[-1] } @$list_ref;
            if ( $pos > -1 ) {
                pop @chain;
                $list_ref->[$pos] = [ @{$list_ref->[$pos]}, reverse @chain ];
                next;
            }
            print STDERR "Invalid data: ring is not closed\n";
            print STDERR "Non-connecting chain:\n" . Dumper( \@chain );
            exit;
        }
    }
}

unless ( exists $result{outer} ) {
    print STDERR "Invalid data: no outer rings\n";
    exit;
}


##  Merge rings


if ( $onering ) {
    my @ring = @{ shift @{$result{outer}} };

    for my $type ( 'outer', 'inner' ) {
        next unless exists $result{$type};
        next if $noinner && $type eq 'inner';

        while ( scalar @{$result{$type}} ) {

            # find close[st] points
            my @ring_center = centroid( map { [@{$osm{node}->{$_}}{'lon','lat'}] } @ring );
            
            if ( $type eq 'inner' ) {
                my ( $index_i, $dist ) = ( 0, metric( \@ring_center, $ring[0] ) );
                for my $i ( 1 .. $#ring ) {
                    my $tdist = metric( \@ring_center, $ring[$i] );
                    next unless $tdist < $dist;
                    ( $index_i, $dist ) = ( $i, $tdist );
                }
                @ring_center = @{ $osm{node}->{ $ring[$index_i] } }{'lon','lat'};
            }

            $result{$type} = [ sort { 
                    metric( \@ring_center, [centroid( map { [@{$osm{node}->{$_}}{'lon','lat'}] } @$a )] ) <=>
                    metric( \@ring_center, [centroid( map { [@{$osm{node}->{$_}}{'lon','lat'}] } @$b )] )
                } @{$result{$type}} ];

            my @add = @{ shift @{$result{$type}} };
            my @add_center = centroid( map { [@{$osm{node}->{$_}}{'lon','lat'}] } @add );

            my ( $index_r, $dist ) = ( 0, metric( \@add_center, $ring[0] ) );
            for my $i ( 1 .. $#ring ) {
                my $tdist = metric( \@add_center, $ring[$i] );
                next unless $tdist < $dist;
                ( $index_r, $dist ) = ( $i, $tdist );
            }
        
            ( my $index_a, $dist ) = ( 0, metric( $ring[$index_r], $add[0] ) );
            for my $i ( 1 .. $#add ) {
                my $tdist = metric( $ring[$index_r], $add[$i] );
                next unless $tdist < $dist;
                ( $index_a, $dist ) = ( $i, $tdist );
            }

            # merge
            splice @ring, $index_r, 0, ( $ring[$index_r], @add[ $index_a .. $#add-1 ], @add[ 0 .. $index_a-1 ], $add[$index_a] );
        }
    }

    $result{outer} = [ \@ring ];
}




##  Output

if ( $outfile ) {
    open OUT, '>', $outfile;
} 
else {
    *OUT = *STDOUT;
}

my $rel = join q{+}, @rel_ids;
print OUT "Relation $rel\n\n";

my $num = 1;
for my $type ( 'outer', 'inner' ) {
    next unless exists $result{$type};

    for my $ring ( sort { scalar @$b <=> scalar @$a } @{$result{$type}} ) {
        print OUT ( $type eq 'inner' ? q{-} : q{}) . $num++ . "\n";
        for my $point ( @$ring ) {
            printf OUT "   %-11s  %-11s\n", @{$osm{node}->{$point}}{'lon','lat'};
        }
        print OUT "END\n\n";
    }
}

print OUT "END\n";




sub metric {
    my ( $x1, $y1 ) = ref $_[0]
        ? @{ shift @_ }
        : @{ $osm{node}->{ shift @_ } }{'lon','lat'};
    my ( $x2, $y2 ) = ref $_[0]
        ? @{ shift @_ }
        : @{ $osm{node}->{ shift @_ } }{'lon','lat'};

    return (($x2-$x1)*cos( ($y2+$y1)/2/180*3.14159 ))**2 + ($y2-$y1)**2;
}

sub centroid {

    my $slat = 0;
    my $slon = 0;
    my $ssq  = 0;

    for my $i ( 1 .. $#_-1 ) {
        my $tlon = ( $_[0]->[0] + $_[$i]->[0] + $_[$i+1]->[0] ) / 3;
        my $tlat = ( $_[0]->[1] + $_[$i]->[1] + $_[$i+1]->[1] ) / 3;

        my $tsq = ( ( $_[$i]  ->[0] - $_[0]->[0] ) * ( $_[$i+1]->[1] - $_[0]->[1] ) 
                  - ( $_[$i+1]->[0] - $_[0]->[0] ) * ( $_[$i]  ->[1] - $_[0]->[1] ) );
        
        $slat += $tlat * $tsq;
        $slon += $tlon * $tsq;
        $ssq  += $tsq;
    }

    if ( $ssq == 0 ) {
        return ( 
            ((min map { $_->[0] } @_) + (max map { $_->[0] } @_)) / 2,
            ((min map { $_->[1] } @_) + (max map { $_->[1] } @_)) / 2 );
    }
    return ( $slon/$ssq , $slat/$ssq );
}
