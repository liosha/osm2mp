#!/usr/bin/perl

my $cache_file = 'osmget.cache';

my $api  = "http://api.openstreetmap.org/api/0.6/map?bbox=";
my $step = 0.5;
#my $api  = "http://www.informationfreeway.org/api/0.6/map?bbox=";
#my $step = 1;


# use strict;
use Storable;

use LWP::UserAgent;
use IO::Uncompress::Gunzip qw{ gunzip $GunzipError };
use List::Util qw{ min };

use Data::Dump 'dd';


my $cache_ref = retrieve $cache_file    if -f $cache_file;
my %cache = %$cache_ref                 if ref $cache_ref;


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

my $tilecount = 0;

TILE:
while (scalar @tiles) {

    my $tile = shift @tiles;
    $tilecount ++;
    my $tilebbox = join q{,}, @$tile;
    printf STDERR "Getting [$tilecount/%d] bbox=$tilebbox...   ", (scalar @tiles+1);

    my $url = "$api$tilebbox";
    if ( exists $cache{$url}  &&  $cache{$url} == 400 ) {
        print STDERR "  --| cached to be splitted\n";
        my ($lon0, $lat0, $lon1, $lat1) = @$tile;
        push @tiles, 
            [ $lon0, $lat0, ($lon0+$lon1)/2, ($lat0+$lat1)/2 ],
            [ $lon0, ($lat0+$lat1)/2, ($lon0+$lon1)/2, $lat1 ],
            [ ($lon0+$lon1)/2, $lat0, $lon1, ($lat0+$lat1)/2 ],
            [ ($lon0+$lon1)/2, ($lat0+$lat1)/2, $lon1, $lat1 ];
        next TILE;
    }

    my $ua = LWP::UserAgent->new;
    $ua->default_header('Accept-Encoding' => 'gzip');

    my $req = HTTP::Request->new( GET => $url );
    my $res = $ua->request($req);

    if ( $res->is_success ) {

        my $data;
        gunzip \($res->content) => \$data;

        if ( $data =~ m{<error>} ) {
            print STDERR "Ok but error! shall try again\n";
            push @tiles, $tile;
            sleep 10;
            next TILE;
        }
        
        print STDERR "Ok\n";
        print $data;
    }
    else {
        print STDERR $res->code . "  --| ";

        if ( $res->code ~~ [ 400, 500, 501 ] ) {
            
            $cache{$url} = $res->code   if $res->code == 400;

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
            sleep 60;
        }
    }
}

store \%cache, $cache_file;
