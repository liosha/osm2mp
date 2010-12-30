#!/usr/bin/perl

use strict;
use Encode;
use Geo::Parse::PolishFormat;
use List::Util qw{ min max };
use List::MoreUtils qw{ all any };

use Data::Dump 'dd';

my $roadid = 1;
my $nodeid = 1;

my $callback = sub {
    my $obj = shift;

    if ( $obj->{name} eq 'IMG ID' ) {
        print "[IMG ID]\n";
        for my $line ( @{ $obj->{lines} } ) {
            next if $line =~ /^Routing=/;
            $line =~ s/^ID=(\d)/'ID='.($1+1)/e;
            $line =~ s/^Name=(.*)/Name=S: $1/;
            print "$line\n";
        }
        print "Numbering=Y\n";
        print "DrawPriority=20\n";
        print "[END-IMG ID]\n\n\n";
        return;
    };

    if ( $obj->{name} eq 'POLYGON' && $obj->{attributes}->{Type} eq '0x4B' ) {
        print "[POLYGON]\n";
        for my $line ( @{ $obj->{lines} } ) {
            print "$line\n";
        }
        print "[END]\n\n\n";
        return;
    };
    
    return  unless 
            $obj->{name} eq 'POLYGON' && $obj->{attributes}->{Type} eq '0x13'
        ||  $obj->{name} eq 'POI'     && $obj->{attributes}->{Type} eq '0x6100';

    return  unless all { exists $obj->{attributes}->{$_} } qw{ HouseNumber StreetDesc CityName };

    print "[POLYLINE]\n";
    print "Type=0x0D\n";

    my $number = encode 'cp1251', uc( decode 'cp1251', $obj->{attributes}->{HouseNumber} );
    $number =~ s/\s//g;

    print "Label=$number $obj->{attributes}->{StreetDesc}\n";

    my ( $lat, $lon ) = centroid( @{ $obj->{attributes}->{Data0} } );
    printf "Data0=(%f,%f),(%f,%f)\n", $lat-0.00002, $lon, $lat+0.00002, $lon;
    
    print 'RoadID=' . $roadid++ . "\n";

    ($number) = $number =~ /(\d{1,4})/;

    $obj->{attributes}->{CityName} =~ s/,/ /g;
    $obj->{attributes}->{RegionName} =~ s/,/ /g;
    $obj->{attributes}->{CountryName} =~ s/,/ /g;
    print "Numbers1=0,B,$number,$number,N,-1,-1,-1,-1,$obj->{attributes}->{CityName},$obj->{attributes}->{RegionName},$obj->{attributes}->{CountryName},-1\n";

    printf "Nod1=0,%d,0\n", $nodeid++;
    printf "Nod2=1,%d,0\n", $nodeid++;

    while ( my ( $k, $v ) = each %{ $obj->{attributes} } ) {
        next if any { $k eq $_ } qw{ Type Label HouseNumber StreetDesc };
        next if $k =~ /^Data\d+/;
        print "$k=$v\n";
    }
    print "[END]\n\n";
};



local $SIG{__WARN__} = sub { shift };

my $parser = Geo::Parse::PolishFormat->new();
$parser->parse( $ARGV[0], $callback );



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
