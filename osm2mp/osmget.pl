#!/usr/bin/perl


# use strict;
require LWP::UserAgent;



my $bbox = $ARGV[0];

my $api  = "http://api.openstreetmap.org/api/0.6/map?bbox=";
my $step = 0.5;
#my $api  = "http://www.informationfreeway.org/api/0.5/map?bbox=";
#my $step = 2;


my @tiles;
my ($minlon, $minlat, $maxlon, $maxlat) = split ",", $bbox;

for (my $lon=$minlon; $lon<$maxlon; $lon+=$step) {
    for (my $lat=$minlat; $lat<$maxlat; $lat+=$step) {
        push @tiles, join (",", ($lon, $lat, (sort($maxlon,$lon+$step))[0], (sort($maxlat,$lat+$step))[0]));
    }
}

while (scalar @tiles) {

    my $tile = shift @tiles;
    print STDERR "Getting bbox=$tile...   ";

    my $ua = LWP::UserAgent->new;
    my $req = HTTP::Request->new(GET => "$api$tile");
    my $res = $ua->request($req);

    if ($res->is_success) {
        print STDERR "Ok\n";
        print $res->content;
    } else {
#        print STDERR $res->status_line . "  --| ";
        print STDERR $res->code . "  --| ";
        if ($res->code==400 || $res->code==500 || $res->code==501) {
            print STDERR "tile will be splitted\n";
            my ($lon0, $lat0, $lon1, $lat1) = split ",", $tile;
            push @tiles, join (",", ($lon0, $lat0, ($lon0+$lon1)/2, ($lat0+$lat1)/2));
            push @tiles, join (",", ($lon0, ($lat0+$lat1)/2, ($lon0+$lon1)/2, $lat1));
            push @tiles, join (",", (($lon0+$lon1)/2, $lat0, $lon1, ($lat0+$lat1)/2));
            push @tiles, join (",", (($lon0+$lon1)/2, ($lat0+$lat1)/2, $lon1, $lat1));
        } else {
            print STDERR "shall try again\n";
            push @tiles, $tile;
        }
    }

}

