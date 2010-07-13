#!/usr/bin/perl -w

use strict;

use LWP::UserAgent;
use Getopt::Long;
use XML::Simple;
use List::Util qw{ min max sum };
use List::MoreUtils qw{ first_index };
use IO::Uncompress::Gunzip qw{ gunzip $GunzipError };


use Data::Dump 'dd';


my $api  = 'http://www.openstreetmap.org/api/0.6';

my $onering = 0;
my $noinner = 0;


my %rename = (
    # cloudmade
    afghanistan     =>  303427,
    albania         =>  53292,
    armenia         =>  364066,
    azerbaijan      =>  364110,
    cyprus          =>  307787,
    egypt           =>  192761,
    georgia         =>  28699,
    honduras        =>  287670,
    israel          =>  214865,
    jordan          =>  184818,
    mongolia        =>  161033,
    oman            =>  305138,
    saudi_arabia    =>  307584,
    turkey          =>  174737,
    united_arab_emirates    =>  307763,
    yemen           =>  305092,

    # geofabrik
    'bosnia-herzegovina'    =>  214908,
    bulgaria        =>  186382,
    croatia         =>  214885,
    greece          =>  192307,
    hungary         =>  21335,
    kosovo          =>  53295,
    latvia          =>  72594,
    lithuania       =>  72596,
    macedonia       =>  53293,
    moldova         =>  58974,
    montenegro      =>  53296,
    romania         =>  90689,
    serbia          =>  53294,
    slovenia        =>  218657,
    ukraine         =>  60199,
    
    # gis-lab contries
    belarus         =>  59065,
    kazakhstan      =>  214665,
    kyrgyzstan      =>  178009,
    tajikistan      =>  214626,
    turkmenistan    =>  223026,
    uzbekistan      =>  196240,

    # gis-lab Russia
    adygeya         =>  253256,
    altay           =>  145194,
    altayskiy       =>  144764,
    amur            =>  147166,
    arkhan          =>  140337,
    astrakhan       =>  112819,
    bashkir         =>  77677,
    belgorod        =>  83184,
    bryansk         =>  81997,
    buryat          =>  145729,
    chechen         =>  109877,
    chel            =>  77687,
    chukot          =>  151231,
    chuvash         =>  80513,
    dagestan        =>  109876,
    evrey           =>  147167,
    ingush          =>  253252,
    irkutsk         =>  145454,
    ivanov          =>  85617,
    kabardin        =>  109879,
    kalinin         =>  103906,
    kalmyk          =>  108083,
    kaluzh          =>  81995,
    kamch           =>  151233,
    karach          =>  109878,
    karel           =>  393980,
    kemerovo        =>  144763,
    khabar          =>  151223,
    khakas          =>  190911,
    khanty          =>  140296,
    kirov           =>  115100,
    komi            =>  115136,
    kostrom         =>  85963,
    krasnodar       =>  108082,
    krasnoyarsk     =>  190090,
    kurgan          =>  140290,
    kursk           =>  72223,
    leningrad       =>  176095,
    lipetsk         =>  72169,
    magadan         =>  151228,
    mariyel         =>  115114,
    mordov          =>  72196,
    moscow          =>  102269,
    mosobl          =>  51490,
    murmansk        =>  289998,
    nenec           =>  274048,
    nizhegorod      =>  72195,
    novgorod        =>  89331,
    novosib         =>  140294,
    omsk            =>  140292,
    orenburg        =>  77669,
    orlovsk         =>  72224,
    osetiya         =>  110032,
    penz            =>  72182,
    perm            =>  115135,
    prim            =>  151225,
    pskov           =>  155262,
    rostov          =>  85606,
    ryazan          =>  71950,
    sakhalin        =>  394235,
    samar           =>  72194,
    saratov         =>  72193,
    smol            =>  81996,
    stavrop         =>  108081,
    sverdl          =>  79379,
    tambov          =>  72180,
    tatar           =>  79374,
    tomsk           =>  140295,
    tul             =>  81993,
    tumen           =>  140291,
    tver            =>  178005,
    tyva            =>  145195,
    udmurt          =>  115134,
    ulyan           =>  72192,
    vladimir        =>  72197,
    volgograd       =>  77665,
    vologda         =>  115106,
    voronezh        =>  72181,
    yakut           =>  151234,
    yamal           =>  191706,
    yarosl          =>  81994,
    zabaikal        =>  145730,
);


my $filename;
my $outfile;

GetOptions (
    'file=s'    => \$filename,
    'o=s'       => \$outfile,
    'onering!'  => \$onering,
    'noinner!'  => \$noinner,
);

unless ( @ARGV ) {
    print "Usage:  getbound.pl [-o <file>] <relation_id>\n\n";
    exit;
}


my $osmdata;

my $name =  $ARGV[0];
my $rel  =  exists $rename{$name}  ?  $rename{$name}  :  $name;


if ( $filename ) {
    open my $file, '<', $filename;
    read $file, $osmdata, 10_000_000;
}
else {

    print STDERR "Downloading RelID=$rel..";

    my $ua = LWP::UserAgent->new;
    $ua->default_header('Accept-Encoding' => 'gzip');
    $ua->timeout( 60 );
    my $req = HTTP::Request->new( GET => "$api/relation/$rel/full" );
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
    gunzip \($res->content) => \$osmdata;
}


my $osm = XMLin( $osmdata, 
            ForceArray  => 1,
            KeyAttr     => [ 'id', 'k' ],
          );


my %role = (
    ''          => 'outer',
    'outer'     => 'outer',
    'border'    => 'outer',
    'exclave'   => 'outer',
    'inner'     => 'inner',
    'enclave'   => 'inner',
);

my %ring = ( 'inner' => [], 'outer' => [] );
my %result;


for my $member ( @{ $osm->{relation}->{$rel}->{member} } ) {
    next unless $member->{type} eq 'way';
    next unless exists $role{ $member->{role} };
    
    unless ( exists $osm->{way}->{$member->{ref}} ) {
        print STDERR "Incomplete data: way $member->{ref} is missing\n";
        next;
    }

    push @{ $ring{$role{$member->{role}}} },  [ map { $_->{ref} } @{$osm->{way}->{$member->{ref}}->{nd}} ];

}

while ( my ($type,$list_ref) = each %ring ) {
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
        exit;
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
            my @ring_center = centroid( map { [@{$osm->{node}->{$_}}{'lon','lat'}] } @ring );
            
            if ( $type eq 'inner' ) {
                my ( $index_i, $dist ) = ( 0, metric( \@ring_center, $ring[0] ) );
                for my $i ( 1 .. $#ring ) {
                    my $tdist = metric( \@ring_center, $ring[$i] );
                    next unless $tdist < $dist;
                    ( $index_i, $dist ) = ( $i, $tdist );
                }
                @ring_center = @{ $osm->{node}->{ $ring[$index_i] } }{'lon','lat'};
            }

            $result{$type} = [ sort { 
                    metric( \@ring_center, [centroid( map { [@{$osm->{node}->{$_}}{'lon','lat'}] } @$a )] ) <=>
                    metric( \@ring_center, [centroid( map { [@{$osm->{node}->{$_}}{'lon','lat'}] } @$b )] )
                } @{$result{$type}} ];

            my @add = @{ shift @{$result{$type}} };
            my @add_center = centroid( map { [@{$osm->{node}->{$_}}{'lon','lat'}] } @add );

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

print OUT "Relation $rel\n\n";

my $num = 1;
for my $type ( 'outer', 'inner' ) {
    next unless exists $result{$type};

    for my $ring ( sort { scalar @$b <=> scalar @$a } @{$result{$type}} ) {
        print OUT ( $type eq 'inner' ? q{-} : q{}) . $num++ . "\n";
        for my $point ( @$ring ) {
            printf OUT "   %-11s  %-11s\n", @{$osm->{node}->{$point}}{'lon','lat'};
        }
        print OUT "END\n\n";
    }
}

print OUT "END\n";




sub metric {
    my ( $x1, $y1 ) = ref $_[0]
        ? @{ shift @_ }
        : @{ $osm->{node}->{ shift @_ } }{'lon','lat'};
    my ( $x2, $y2 ) = ref $_[0]
        ? @{ shift @_ }
        : @{ $osm->{node}->{ shift @_ } }{'lon','lat'};

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
