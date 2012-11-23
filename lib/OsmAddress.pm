package OsmAddress;

# ABSTRACT: OSM addressing

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::Util qw/ first /;

use AreaTree;


our @NAME_TAGS = qw/ name place_name /;

#our @ADDRESS_PREFIXES = qw/ addr is_in /;
our @ADDRESS_PREFIXES = qw/ addr /;

our @ADDRESS_ITEMS = (
    [ office        => { aliases => [ qw/ flat appartment / ] } ],
    [ entrance      => {} ],
    [ housenumber   => { aliases => [ 'housename' ] } ],
    [ postcode      => {} ],
    [ quarter       => { aliases => [ qw/ neighbourhood / ] } ], # relation => [ 'quarter' ] } ],
    [ street        => { relation => [ qw/ street associatedStreet / ] } ],
    [ suburb        => {} ],
    [ city          => {} ],
    [ subdistrict   => {} ],
    [ district      => {} ],
    [ region        => { aliases => [ 'state' ] } ],
    [ country       => {} ],
);


=head2 new

Constructor

    my $addresser = OsmAddress->new( %opts );

Options:
  * addr_items
  * addr_prefixes
  * name_tags

=cut

sub new {
    my ($class, %opt) = @_;
    my $self = {
        addr_items       => $opt{addr_items}    || \@ADDRESS_ITEMS,
        addr_prefixes    => $opt{addr_prefixes} || \@ADDRESS_PREFIXES,        
    };

    my $name_tag_str = join q{|}, @{ $opt{name_tags} || \@NAME_TAGS };
    $self->{name_tag_re} = qr/ ^ (?: $name_tag_str ) \b /xms;

    my %addr_tags =
        map {
            my ($id, $prop) = @$_;
            my @taglist = 
                map {  my $level = $_;  map {"$_:$level"} @{ $self->{addr_prefixes} }  }
                ($id, @{ $prop->{aliases} || [] } );
            ( $id => \@taglist )
        }
        @{ $self->{addr_items} };
    $self->{addr_tags} = \%addr_tags;

    my %tag_level =
        map {  my $level = $_;  map {( $_ => $level )} @{ $addr_tags{$level} } }
        keys %addr_tags;
    $self->{tag_level} = \%tag_level;

    my @addr_levels = ( q{}, map { $_->[0] } @{ $self->{addr_items} } );
    while ( @addr_levels ) {
        my $level = shift @addr_levels;
        $self->{addr_parents}->{$level} = [ @addr_levels ];
        
        my $tag_str = join q{|}, map { @{ $addr_tags{$_} } } (($level ? $level : ()), @addr_levels);
        $self->{addr_tag_re}->{$level} = qr/ ^ (?: $tag_str ) \b /xms;
    }

    return bless $self, $class;
}



sub load_area {
    my ($self, $level, $info, @contours) = @_;

    my $tree = $self->{areas}->{$level} ||= AreaTree->new();
    $tree->add_area( $info, @contours );

    return;
}


sub find_area {
    my ($self, $level, @points) = @_;

    my $tree = $self->{areas}->{$level}
        or return;

    return $tree->find_area(@points);
}



=head2 $addresser->get_address_tags( \%tags, %opt )

    Filters out all non-address tags

    Options:
        level - assume object has this level:
            * use it's name as addr:<level>
            * filter out all lower address levels

=cut


sub get_address_tags {
    my ($self, $tags, %opt) = @_;

    my $level = $opt{level} // q{};
    my $tag_re = $self->{addr_tag_re}->{$level};
    croak "Unknown level: $level"  if !$tag_re;

    my %result;

    if ( $level ) {
        while ( my ($k, $v) = each %$tags ) {
            next if $k !~ $self->{name_tag_re};
            $k =~ s/$self->{name_tag_re}/addr:$level/xms;
            $result{$k} = $v;
        }
    }

    while ( my ($k, $v) = each %$tags ) {
        next if $k !~ $tag_re;
        $result{$k} = $v;
    }

    return \%result;
}



sub get_lang_address {
    my ($self, $tags, $lang_select, %opt) = @_;

    my %address;
    while ( my ($level, $keys) = each %{ $self->{addr_tags} } ) {
        my $value = first {defined} map { $lang_select->get_value($_, $tags) } @$keys;
        next if !$value;
        $address{$level} = $value;
    }

    return \%address;
}


1;

