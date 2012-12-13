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

    my $self = {
        nodes => {},
        chains => {},
        tags => { node => {}, way => {}, relation => {} },
        relations => {},
    };

    bless $self, $class;

    if ( $opt{fh} ) {
        $self->load($opt{fh}, %opt);
        $self->merge_multipolygons();
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

    my ($nodes, $chains, $relations) = @$self{ qw/nodes chains relations/ };
    my ($nodetag, $waytag, $reltag) = @{$self->{tags}}{ qw/node way relation/ };

    my %skip_tag = map {( $_ => 1 )} @{ $opt{skip_tags} || [] };

    my %parse_handler = (
        node => sub {
                my $obj = shift;

                my $attr = $obj->{attr}; 
                my $id = $attr->{id};
                $nodes->{$id} = "$attr->{lat},$attr->{lon}";

                if ( my $tags = $obj->{tag} ) {
                    return if all { $skip_tag{$_} } keys %$tags;
                    $nodetag->{$id} = $tags;
                }
                return;
            },
        way => sub {
                my $obj = shift;
                my $id = $obj->{attr}->{id};

                my $prev;
                my @chain =
                    grep { my $is_dupe = $_ ~~ $prev; $prev = $_; !$is_dupe }
                    grep { exists $nodes->{$_} }
                    @{ $obj->{nd} || [] };

                return if @chain < 2;

                $chains->{$id} = \@chain;

                if ( my $tags = $obj->{tag} ) {
                    return if all { $skip_tag{$_} } keys %$tags;
                    $waytag->{$id} = $tags;
                }
                return;
            },
        relation => sub {
                my $obj = shift;

                my $tags = $obj->{tag};
                return if !$tags;

                my @members =
                    grep {
                            my ($id, $t) = @$_{'ref', 'type'};
                            $t ~~ 'node'     ? exists $nodes->{$id} : 
                            $t ~~ 'way'      ? exists $chains->{$id} :
                            $t ~~ 'relation' ? exists $reltag->{$id} :
                            0;
                        }
                    @{ $obj->{member} || [] };
                return if !@members;

                if ( my $type = $tags->{type} ) {
                    my $id = $obj->{attr}->{id};
                    $reltag->{$id} = $tags;
                    $relations->{$type}->{$id} = \@members;
                }
                return;
            },
        %{ $opt{handlers} || {} },
    );

    my $parser = Geo::Openstreetmap::Parser->new( %parse_handler );
    $parser->parse($fh);

    return;
}


=method merge_multipolygons

Process multipolygons

=cut

sub merge_multipolygons {
    my ($self) = @_;

    my $waytag = $self->{tags}->{way};
    my $reltag = $self->{tags}->{relation};

    for my $type ( qw/ multipolygon boundary / ) {
        while ( my ($id, $members) = each %{ $self->{relations}->{$type} } ) {
            my @outers = map {$_->{ref}} grep { $_->{type} ~~ 'way' && $_->{role} ~~ [ q{}, 'outer', 'exclave' ] } @$members;
            next if !@outers;

            my @inners = map {$_->{ref}} grep { $_->{type} ~~ 'way' && $_->{role} ~~ [ 'inner', 'enclave' ] } @$members;

            my $mpoly = _merge_multipolygon($self, \@outers, \@inners);
            next if !$mpoly;

            my $rid = "r$id";
            $self->{mpoly}->{$rid} = $mpoly;
            $self->{mpoly}->{$outers[0]} = $mpoly   if @outers == 1 && @inners;
            $waytag->{$rid} = $reltag->{$id};
        }
    }
    return;
}


sub _merge_multipolygon {
    my ($self, $outers, $inners) = @_;

    my $chains = $self->{chains};

    my @result;
    for my $list_ref ( [@$outers], [@$inners] ) {
        my @rings;

        # skip incomplete objects
        return if notall { exists $chains->{$_} } @$list_ref;

        LIST:
        while ( @$list_ref ) {
            my $id = shift @$list_ref;
            my @contour = @{$chains->{$id}};

            CONTOUR: {
                if ( $contour[0] eq $contour[-1] ) {
                    push @rings, \@contour;
                    next LIST;
                }

                my $add = first_index { $contour[-1] eq $chains->{$_}->[0] } @$list_ref;
                if ( $add > -1 ) {
                    my $add_id = $list_ref->[$add];
                    $id .= ":$add_id";
                    pop @contour;
                    push @contour, @{$chains->{$add_id}};
                    splice @$list_ref, $add, 1;
                    redo CONTOUR;
                }

                $add = first_index { $contour[-1] eq $chains->{$_}->[-1] } @$list_ref;
                if ( $add > -1 ) {
                    my $add_id = $list_ref->[$add];
                    $id .= ":r$add_id";
                    pop @contour;
                    push @contour, reverse @{$chains->{$add_id}};
                    splice @$list_ref, $add, 1;
                    redo CONTOUR;
                }

                # skip broken multipolygons
                return;
            }
        }

        push @result, \@rings;
    }

    return \@result;
}



1;

