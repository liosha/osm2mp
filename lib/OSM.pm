package OSM;

# ABSTRACT: preload OSM data in useful way

# $Id$


use 5.010;
use strict;
use warnings;
use autodie;

use Carp;
use List::MoreUtils qw/ all notall first_index /;

use Geo::Openstreetmap::Parser;


=method new

    my $osm = OSM->new( %params );

Create new instance.
Options:
    fh - file handler to load data

=cut

sub new {
    my ($class, %opt) = @_;

    my $self = OSM::Hash->new();

    if ( $opt{fh} ) {
        $self->load($opt{fh}, %opt);
    }

    return $self;
}


=method load

    $osm->load(*STDIN, %opt);

Load osm data from file handler

Options:

    skip_tags => [ qw/ source created_by ... / ]
    handlers => { bound => sub{...} }

=cut

sub load {
    my ($self, $fh, %opt) = @_;

    my %skip_tag = map {( $_ => 1 )} @{ $opt{skip_tags} || [] };

    my %parse_handler = (
        node => sub {
                my $obj = shift;
                delete $obj->{tag}  if $obj->{tag} && all { $skip_tag{$_} } keys %{$obj->{tag}};
                return $self->add_node($obj);
            },
        way => sub {
                my $obj = shift;
                delete $obj->{tag}  if $obj->{tag} && all { $skip_tag{$_} } keys %{$obj->{tag}};
                return $self->add_way($obj);
            },
        relation => sub {
                my $obj = shift;
                my $type = $obj->{tag}->{type}  or return;

                if ( $type ~~ [ qw/ multipolygon boundary / ] ) {
                    my ($mpoly, $oldstyle_id) = $self->_merge_multipolygon($obj);
                    if ( $mpoly ) {
                        my $id = "r$obj->{attr}->{id}";
                        $self->add_polygon($id, $mpoly, $obj->{tag});
                        $self->add_polygon($oldstyle_id, $mpoly)  if $oldstyle_id;
                    }
                    return;
                }

                $self->add_relation($obj);                
                return;
            },
        bound  => sub {
                my $obj = shift;
                $self->{bbox} = [ @{[ split q{,}, $obj->{attr}->{box} ]}[1,0,3,2] ];
                return;
            },
        bounds => sub {
                my $obj = shift;
                $self->{bbox} = [ @{ $obj->{attr} }{ qw/ minlon minlat maxlon maxlat / } ];
                return;
            },
        %{ $opt{handlers} || {} },
    );

    my $parser = Geo::Openstreetmap::Parser->new( %parse_handler );
    $parser->parse($fh);

    return;
}



sub _merge_multipolygon {
    my ($self, $obj) = @_;

    my @members = grep { $_->{type} ~~ 'way' } @{ $obj->{member} };

    my @outer_ids = map { $_->{ref} }  grep { $_->{role} ~~ [ q{}, 'outer', 'exclave' ] }  @members;
    return if !@outer_ids;

    my @inner_ids = map { $_->{ref} }  grep { $_->{role} ~~ [ 'inner', 'enclave' ] }  @members;

    my ($oldstyle_id) = (@outer_ids == 1 && @inner_ids) ? @outer_ids : ();

    my @result;
    for my $list_ref ( \@outer_ids, \@inner_ids ) {
        my @rings;

        my %chain = map {( $_ => $self->get_way_chain($_) )} @$list_ref;

        # skip incomplete objects
        return if notall {defined} values %chain;

        LIST:
        while ( @$list_ref ) {
            my $id = shift @$list_ref;
            my @contour = @{ $chain{$id} };

            CONTOUR: {
                if ( $contour[0] eq $contour[-1] ) {
                    push @rings, \@contour;
                    next LIST;
                }

                my $add = first_index { $contour[-1] eq $chain{$_}->[0] } @$list_ref;
                if ( $add > -1 ) {
                    my $add_id = $list_ref->[$add];
                    $id .= ":$add_id";
                    pop @contour;
                    push @contour, @{ $chain{$add_id} };
                    splice @$list_ref, $add, 1;
                    redo CONTOUR;
                }

                $add = first_index { $contour[-1] eq $chain{$_}->[-1] } @$list_ref;
                if ( $add > -1 ) {
                    my $add_id = $list_ref->[$add];
                    $id .= ":r$add_id";
                    pop @contour;
                    push @contour, reverse @{ $chain{$add_id} };
                    splice @$list_ref, $add, 1;
                    redo CONTOUR;
                }

                # skip broken multipolygons
                return;
            }
        }

        push @result, \@rings;
    }

    return \@result, $oldstyle_id;
}



package OSM::Hash;

use base 'OSM';

use Carp;


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


sub add_node {
    my ($self, $obj) = @_;

    my $attr = $obj->{attr};
    my $id = $attr->{id};
    $self->{nodes}->{$id} = "$attr->{lat},$attr->{lon}";
    $self->{tags}->{node}->{$id} = $obj->{tag}  if $obj->{tag};

    return;
}


sub add_way {
    my ($self, $obj) = @_;

    my $id = $obj->{attr}->{id};
    
    my $prev;
    my @chain =
        grep { my $is_dupe = $_ ~~ $prev; $prev = $_; !$is_dupe }
        grep { exists $self->{nodes}->{$_} }
        @{ $obj->{nd} || [] };
    
    return if @chain < 2;

    $self->{chains}->{$id} = \@chain;
    $self->{tags}->{way}->{$id} = $obj->{tag}  if $obj->{tag};

    return;
}


sub get_way_chain {
    my ($self, $id) = @_;

    return $self->{chains}->{$id};
}


sub add_polygon {
    my ($self, $id, $mpoly, $tags) = @_;
    
    $self->{mpoly}->{$id} = $mpoly;
    $self->{tags}->{way}->{$id} = $tags  if $tags;
    return;
}


sub add_relation {
    my ($self, $obj) = @_;

    my $tags = $obj->{tag};
    return if !$tags;

    my @members =
        grep {
            my ($id, $t) = @$_{'ref', 'type'};
            $t ~~ 'node'     ? exists $self->{nodes}->{$id} :
            $t ~~ 'way'      ? exists $self->{chains}->{$id} :
            $t ~~ 'relation' ? exists $self->{tags}->{relation}->{$id} :
            0;
        }
        @{ $obj->{member} || [] };
    return if !@members;

    my $id = $obj->{attr}->{id};
    $self->{tags}->{relation}->{$id} = $tags;
    $self->{relations}->{$tags->{type}}->{$id} = \@members;
                
    return;
}


sub get_tags {
    my ($self, $type, $id) = @_;

    my $tag_store = $self->{tags}->{$type};
    croak "Invalid object type $type" if !$tag_store;

    return $tag_store->{$id};
}


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

