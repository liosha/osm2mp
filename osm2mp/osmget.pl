#!/usr/bin/perl


# use strict;
use LWP::UserAgent;
use IO::Uncompress::Gunzip qw{ gunzip $GunzipError };
use List::Util qw{ min };

my $api  = "http://api.openstreetmap.org/api/0.6/map?bbox=";
my $step = 0.5;
#my $api  = "http://www.informationfreeway.org/api/0.5/map?bbox=";
#my $step = 2;

binmode STDOUT;

my @tiles;

for my $bbox ( @ARGV ) {;

    print STDERR "Processing $bbox";

    my ($minlon, $minlat, $maxlon, $maxlat) = split q{,}, $bbox;

    my $count = 0;
    for (my $lon=$minlon; $lon<$maxlon; $lon+=$step) {
        for (my $lat=$minlat; $lat<$maxlat; $lat+=$step) {
            push @tiles, [ $lon, $lat, min( $maxlon, $lon+$step ), min( $maxlat, $lat+$step ) ];
            $count ++;
        }
    }
    print STDERR " -> $count tiles\n";
}


while (scalar @tiles) {

    my $tile = shift @tiles;
    my $tilebbox = join q{,}, @$tile;
    print STDERR "Getting bbox=$tilebbox...   ";

    my $ua = LWP::UserAgent->new;
    $ua->default_header('Accept-Encoding' => 'gzip');

    my $req = HTTP::Request->new(GET => "$api$tilebbox");
    my $res = $ua->request($req);

    if ( $res->is_success ) {
        print STDERR "Ok\n";
        my $data;
        gunzip \($res->content) => \$data;
        print $data;
    }
    else {
        print STDERR $res->code . "  --| ";

        if ( $res->code ~~ [ 400, 500, 501 ] ) {
            print STDERR "tile will be splitted\n";
            my ($lon0, $lat0, $lon1, $lat1) = @$tile;
            push @tiles, 
                [ $lon0, $lat0, ($lon0+$lon1)/2, ($lat0+$lat1)/2 ],
                [ $lon0, ($lat0+$lat1)/2, ($lon0+$lon1)/2, $lat1 ],
                [ ($lon0+$lon1)/2, $lat0, $lon1, ($lat0+$lat1)/2 ],
                [ ($lon0+$lon1)/2, ($lat0+$lat1)/2, $lon1, $lat1 ];
        }
        else {
            print STDERR "shall try again\n";
            push @tiles, $tile;
        }
    }
}

    