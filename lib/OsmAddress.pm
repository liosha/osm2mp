package OsmAddress;

# ABSTRACT: OSM addressing

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::Util qw/ first /;
use List::MoreUtils qw/ none first_index /;

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

our $MIN_DEFAULT_LEVEL = 'city';


=head2 new

Constructor

    my $addresser = OsmAddress->new( %opts );

Options:
  * addr_items
  * addr_prefixes
  * name_tags
  * rename_*

=cut

sub new {
    my ($class, %opt) = @_;
    my $self = {
        addr_items       => $opt{addr_items}    || \@ADDRESS_ITEMS,
        addr_prefixes    => $opt{addr_prefixes} || \@ADDRESS_PREFIXES,
        default_address  => {},
    };
    bless $self, $class;

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

        next if !$level;
        my $table = $opt{"rename_$level"};
        next if !$table;
        $self->add_rename_table( $level => $table );
    }

    return $self;
}


# !!! should be lang-dependent!
sub add_rename_table {
    my ($self, $level, $table) = @_;

    $self->{rename}->{$level} = $table;
    return;
}



sub load_area {
    my ($self, $level, $info, @contours) = @_;

    return if none { /addr:$level\b/xms } keys %$info;

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
        if ( !$value && %{ $self->{default_address} } ) {
            $value = first {defined} map { $lang_select->get_value($_, $self->{default_address}) } @$keys;
        }
        next if !$value;

        if ( my $table = $self->{rename}->{$level} ) {
            $value = $table->{uc $value} // $value;
        }

        $address{$level} = $value;
    }

    return \%address;
}



sub get_getopt {
    my ($self) = @_;

    my $first_idx = first_index { $_->[0] eq $MIN_DEFAULT_LEVEL } @{ $self->{addr_items} };

    my @opt_list;
    for my $i ( $first_idx .. $#{ $self->{addr_items} } ) {
        my $level = $self->{addr_items}->[$i]->[0];
        push @opt_list, ( "default-$level|default$level=s" => sub {
                my ($lang, $name) = $_[1] =~ / (?: (\w{1,3}) : )? (.*) /xms;
                my $vtag = $lang ? "addr:$level:$lang" : "addr:$level";
                $self->{default_address}->{$vtag} = $_[1];
            } );
    }

    return @opt_list;
}
   

=method get_usage()

=cut

sub get_usage {
    my ($self) = @_;
    my $first_idx = first_index { $_->[0] eq $MIN_DEFAULT_LEVEL } @{ $self->{addr_items} };
    
    my @opt_list;
    for my $i ( $first_idx .. $#{ $self->{addr_items} } ) {
        my $level = $self->{addr_items}->[$i]->[0];
        push @opt_list, [ "default-$level" => "default $level" ];
    }
    
    return @opt_list;
}


1;

