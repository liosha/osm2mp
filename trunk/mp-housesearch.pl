#!/usr/bin/perl

########
# $Id$
# $Rev$
# $Author$
# $Date$


use 5.010;
use strict;
use warnings;
use autodie;

our $VERSION = 0.02;

use Readonly;
use Encode;
use Geo::Parse::PolishFormat;
use List::Util qw{ min max };
use List::MoreUtils qw{ all any };
use Math::Polygon::Tree  0.06  qw{ polygon_centroid };

use Data::Dump 'dd';



Readonly my $MP_CODEPAGE        => 'cp1251';
Readonly my $FAKE_ROAD_LENGTH   => 0.00002;
Readonly my $MAX_HOUSE_NUMBER   => 9999;



my $roadid = 1;
my $nodeid = 1;

my $callback = sub {
    my $obj = shift;

    if ( $obj->{name} eq 'IMG ID' ) {
        say '[IMG ID]';
        for my $line ( @{ $obj->{lines} } ) {
            next if index( $line, 'Routing=' ) == 0;
            $line =~ s/^ID=(\d)/'ID='.($1+1)/xmse;
            $line =~ s/^Name=/Name=S: /xms;
            say $line;
        }
        say 'Numbering=Y';
        say 'DrawPriority=20';
        say "[END-IMG ID]\n\n";
        return;
    };

    if ( $obj->{name} eq 'POLYGON' && $obj->{attributes}->{Type} eq '0x4B' ) {
        say '[POLYGON]';
        for my $line ( @{ $obj->{lines} } ) {
            print "$line\n";
        }
        say "[END]\n\n";
        return;
    };

    return
        unless $obj->{name} eq 'POLYGON' && $obj->{attributes}->{Type} eq '0x13'
            || $obj->{name} eq 'POI'     && $obj->{attributes}->{Type} ~~ [ '0x2800', '0x6100' ];

    return
        unless all { exists $obj->{attributes}->{$_} } qw{ HouseNumber StreetDesc CityName };

    my $number = encode $MP_CODEPAGE, uc( decode $MP_CODEPAGE, $obj->{attributes}->{HouseNumber} );
    $number =~ tr/ \t\r\n//;
    my $digits = min( 0+$number, $MAX_HOUSE_NUMBER );
    return unless $digits;

    say '[POLYLINE]';
    say 'Type=0x0D';
    say "Label=$number $obj->{attributes}->{StreetDesc}";

    my ($lat, $lon) = @{ polygon_centroid($obj->{attributes}->{Data0}) };
    printf "Data0=(%f,%f),(%f,%f)\n", $lat-$FAKE_ROAD_LENGTH, $lon, $lat+$FAKE_ROAD_LENGTH, $lon;

    say 'RoadID=' . $roadid++;

    $obj->{attributes}->{CityName} =~ tr/,/ /;
    $obj->{attributes}->{RegionName} =~ tr/,/ /;
    $obj->{attributes}->{CountryName} =~ tr/,/ /;
    say "Numbers1=0,B,$digits,$digits,N,-1,-1,-1,-1,$obj->{attributes}->{CityName},$obj->{attributes}->{RegionName},$obj->{attributes}->{CountryName},-1";

    printf "Nod1=0,%d,0\n", $nodeid++;
    printf "Nod2=1,%d,0\n", $nodeid++;

    while ( my ( $k, $v ) = each %{ $obj->{attributes} } ) {
        next if any { $k eq $_ } qw{ Type Label HouseNumber StreetDesc };
        next if $k =~ /^Data\d+/xms;
        print "$k=$v\n";
    }
    say "[END]\n";
};



local $SIG{__WARN__} = sub { shift };

my $parser = Geo::Parse::PolishFormat->new();
$parser->parse( $ARGV[0], $callback );



