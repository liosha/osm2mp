package OSM::Hash;

# ABSTRACT: default osm-data storage engine

# $Id$

use 5.010;
use strict;
use warnings;

use Carp;



=head1 Constructor
=cut

sub new {
    my ($class, %opt) = @_;

    my $self = {
        nodes => {},
        chains => {},
        tags => { node => {}, way => {}, relation => {} },
        relations => {},
    };

    return bless $self, $class;
}


sub add_polygon {
    my ($self, $mpid, $mpoly, $tags) = @_;
    
    $self->{mpoly}->{$mpid} = $mpoly;
    $self->set_tags('way', $mpid, { %$tags })  if $tags;
    return;
}




=head1 Accessors
=cut

sub set_lonlat {
    my ($self, $id, $lon, $lat) = @_;

    ($lon, $lat) = @$lon  if ref $lon eq 'ARRAY';

    $self->{nodes}->{$id} = join q{,}, $lat, $lon;
    return;
}


sub get_lonlat {
    my ($self, $nodes) = @_;

    my $is_single_node = ref $nodes ne 'ARRAY';
    my $ids = $is_single_node ? [ $nodes ] : $nodes;

    my @points =
        map {[ reverse split /,/x, $_ ]}
        map { $self->{nodes}->{$_} // $_ }
        @$ids;

    return $is_single_node ? shift @points : \@points;
}


sub is_node_exists {
    my ( $self, $id ) = @_;

    return exists $self->{nodes}->{$id};
}


sub set_way_chain {
    my ($self, $id, $chain) = @_;

    $self->{chains}->{$id} = $chain;
    return;
}


sub get_way_chain {
    my ($self, $id) = @_;

    return $self->{chains}->{$id};
}


sub is_way_exists {
    my ( $self, $id ) = @_;

    return exists $self->{chains}->{$id};
}


sub set_relation_members {
    my ($self, $id, $type, $members) = @_;

    $self->{relations}->{$type}->{$id} = $members;
    return;
}


sub is_relation_exists {
    my ( $self, $id ) = @_;

    return exists $self->{tags}->{relation}->{$id};
}


sub set_tags {
    my ($self, $type, $id, $tags) = @_;    

    my $tag_store = $self->{tags}->{$type};
    croak "Invalid object type $type" if !$tag_store;

    $tag_store->{$id} = $tags;
}


sub get_tags {
    my ($self, $type, $id) = @_;

    my $tag_store = $self->{tags}->{$type};
    croak "Invalid object type $type" if !$tag_store;

    return $tag_store->{$id};
}


=head1 Iterators

* to be improved

=cut

sub iterate_nodes {
    my ($self, $sub, %opt) = @_;

    # !!! tagged nodes only
    while ( my ($id, $tags) = each %{ $self->{tags}->{node} } ) {
        my $node_info = {
            type    => 'Node',
            id      => $id,
            tag     => $tags,
        };

        $sub->($node_info);
    }

    return;
}


sub iterate_ways {
    my ($self, $sub, %opt) = @_;

    # !!! taged ways and multipolygons
    while ( my ($id, $tags) = each %{ $self->{tags}->{way} } ) {
        my %way_info = (
            type    => 'Way',
            id      => $id,
            tag     => $tags,
        );

        if ( my $mpoly = $self->{mpoly}->{$id} ) {
            my ($outers, $inners) = @$mpoly;
            $way_info{outer} = $outers;
            $way_info{inner} = $inners;
        }
        else {
            my $chain = $self->{chains}->{$id};
            $way_info{chain} = $chain;
            $way_info{outer} = [ $chain ]  if $chain->[0] ~~ $chain->[-1];
        }

        $sub->(\%way_info);
    }

    return;
}


sub iterate_relations {
    my ($self, $type, $sub, %opt) = @_;

    my $rels = $self->{relations}->{$type};
    return if !$rels;

    while ( my ($id, $members) = each %$rels ) {
        $sub->($id, $members, $self->{tags}->{relation}->{$id});
    }

    return;
}

1;

