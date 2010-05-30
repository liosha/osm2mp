#!/usr/bin/perl -w

use strict;

use LWP::UserAgent;
use Getopt::Long;
use XML::Simple;
use List::MoreUtils qw{ first_index };
use IO::Uncompress::Gunzip qw{ gunzip $GunzipError };


use Data::Dump 'dd';


my $api  = 'http://www.openstreetmap.org/api/0.6';


my %rename = (
    # cloudmade
    armenia         =>  364066,
    azerbaijan      =>  364110,
    egypt           =>  192761,
    georgia         =>  28699,

    # geofabrik
    latvia          =>  72594,
    lithuania       =>  72596,
    moldova         =>  58974,
    ukraine         =>  60199,
    
    # gis-lab contries
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
