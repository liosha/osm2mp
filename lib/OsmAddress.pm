package OsmAddress;

# ABSTRACT: OSM addressing

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::Util qw/ first reduce /;


our $NAME_RE = qr/ ^ (?: name | place_name ) \b /xms;

our @ADDRESS_PREFIX = qw/ addr is_in /;

our @ADDRESS_ITEMS = (
    [ housenumber   => { aliases => [ 'housename' ] } ],
    [ quarter       => { relation => [ 'quarter' ] } ],
    [ street        => { relation => [ qw/ street associatedStreet / ] } ],
    [ suburb        => {} ],
    [ city          => {} ],
    [ subdistrict   => {} ],
    [ district      => {} ],
    [ region        => { aliases => [ 'state' ] } ],
    [ country       => {} ],
);

our %ADDRESS_TAGS = map {
    my ($id, $prop) = @$_;
    $id => [ map { my $l = $_; map {"$_:$l"} @ADDRESS_PREFIX } ($id, @{ $prop->{aliases} || [] } ) ];
} @ADDRESS_ITEMS;

our %ADDRESS_PARENTS;
our %ADDRESS_TAG_RE;

my @addr_levels = ( q{}, map { $_->[0] } @ADDRESS_ITEMS );
while ( @addr_levels ) {
    my $level = shift @addr_levels;
    $ADDRESS_PARENTS{$level} = [ $level, @addr_levels ];

    my $tag_str = join q{|}, map { @{$ADDRESS_TAGS{$_}} } (($level ? $level : ()), @addr_levels);
    $ADDRESS_TAG_RE{$level} = qr/ ^ (?: $tag_str ) \b /xms;
}


=head2 get_address_tags( \%tags, %opt )

    Filters out all non-address tags

    Options:
        level - assume object has this level:
            * use it's name as addr:<level>
            * filter out all lower address levels

=cut


sub get_address_tags {
    my ($tags, %opt) = @_;

    my $level = $opt{level} // q{};
    my $tag_re = $ADDRESS_TAG_RE{$level};
    croak "Unknown level: $level"  if !$tag_re;

    my %result;

    if ( $level ) {
        while ( my ($k, $v) = each %$tags ) {
            next if $k !~ $NAME_RE;
            $k =~ s/$NAME_RE/addr:$level/xms;
            $result{$k} = $v;
        }
    }

    while ( my ($k, $v) = each %$tags ) {
        next if $k !~ $tag_re;
        $result{$k} = $v;
    }

    return \%result;
}



1;

