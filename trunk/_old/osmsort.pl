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

use Readonly;
#use Data::Dump 'dd';

our $VERSION = 0.02;


Readonly my $INFINITY => 200;

my %obj;

my @otypes = qw{ node way relation };
my %otype_re = map { $_ => qr{ < $_ .*? \bid= (?<q>['"]) (?<id>.*?) \k<q> }xms } @otypes;

my $current_obj;
my ( $minlat, $minlon, $maxlat, $maxlon ) = ( $INFINITY, $INFINITY, -$INFINITY, -$INFINITY );

while ( my $line = <> ) {

    next if index( $line, '<osm'  ) >= 0;
    next if index( $line, '</osm' ) >= 0;
    next if index( $line, '<?xml' ) >= 0;

    if ( index( $line, '<bound' ) >= 0 ) {
        my %cap;
        if ( index( $line, '<bounds' ) >= 0 ) {
            $line =~
                m{  \bminlat= (?<q>['"]) (?<minlat>.*?) \k<q>
                .*? \bminlon= \k<q>      (?<minlon>.*?) \k<q>
                .*? \bmaxlat= \k<q>      (?<maxlat>.*?) \k<q>
                .*? \bmaxlon= \k<q>      (?<maxlon>.*?) \k<q> }xms;
            %cap = %+;
        }
        else {
            $line =~ m{ \bbox= (?<q>['"]) (?<minlon>.*?), (?<minlat>.*?), (?<maxlon>.*?), (?<maxlat>.*?) \k<q> }xms;
            %cap = %+;
        }

        if ( $cap{minlat} < $minlat )   {  $minlat = $cap{minlat}  }
        if ( $cap{minlon} < $minlon )   {  $minlon = $cap{minlon}  }
        if ( $cap{maxlat} > $maxlat )   {  $maxlat = $cap{maxlat}  }
        if ( $cap{maxlon} > $maxlon )   {  $maxlon = $cap{maxlon}  }

        next;
    }

    for my $type ( @otypes ) {
        if ( $line =~ $otype_re{$type} ) {
            $current_obj = \( $obj{$type}->{ $+{id} } = q{} );
            last;
        }
    }

    next unless $current_obj;

    ${$current_obj} .= $line;
}

say q{<?xml version="1.0" standalone="yes"?>};
say qq{<osm version="0.6" generator="osmsort.pl $VERSION">};

if ( $minlat < $maxlat ) {
    say qq{  <bound box="$minlat,$minlon,$maxlat,$maxlon" origin="http://www.openstreetmap.org/api/0.6"/>};
}

for my $type ( @otypes ) {
    print @{ $obj{$type} }{ sort { $a <=> $b } keys %{ $obj{$type} } };
}

say q{</osm>};
