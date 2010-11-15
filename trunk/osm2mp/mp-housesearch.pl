#!/usr/bin/perl

use strict;
use Geo::Parse::PolishFormat;
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
            $line =~ s/^(Name=.*)/$1 (search)/;
            print "$line\n";
        }
        print "Numbering=Y\n";
        print "[END-IMG ID]\n\n\n";
        return;
    };

    return  unless $obj->{name} eq 'POLYGON';
    return  unless $obj->{attributes}->{Type} eq '0x13';
    return  unless all { exists $obj->{attributes}->{$_} } qw{ HouseNumber StreetDesc CityName };

    #dd $obj;
    print "[POLYLINE]\n";
    print "Type=0x0D\n";

    my $number = $obj->{attributes}->{HouseNumber};
    $number =~ s/\s//g;

    print "Label=$number $obj->{attributes}->{StreetDesc}\n";

    pop @{ $obj->{attributes}->{Data0} };
    print 'Data0=(' . join( q{),(}, map { join( q{,}, @$_ ) } @{ $obj->{attributes}->{Data0} } ) . ")\n";
    
    print 'RoadID=' . $roadid++ . "\n";
    print 'Nod1=0,' . $nodeid++ . ",0\n";
    print 'Nod2=' . $#{$obj->{attributes}->{Data0}} . ',' . $nodeid++ . ",0\n";

    $number += 0;
    print "Numbers1=0,B,$number,$number,N,-1,-1,-1,-1,$obj->{attributes}->{CityName},$obj->{attributes}->{RegionName},$obj->{attributes}->{CountryName},-1\n";

    while ( my ( $k, $v ) = each %{ $obj->{attributes} } ) {
        next if any { $k eq $_ } qw{ Type Label HouseNumber StreetDesc };
        next if $k =~ /^Data\d+/;
        print "$k=$v\n";
    }
    print "[END]\n\n";
};


my $parser = Geo::Parse::PolishFormat->new();
$parser->parse( $ARGV[0], $callback );
