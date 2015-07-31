package AreaTree;

# ABSTRACT: search area containing object

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::Util qw/ first reduce /;

use base qw/ Tree::R /;
use Math::Polygon::Tree 0.06;



=method add_area

    $area_tree->add_area( $object, @contours );

Add area to tree.

=cut

sub add_area {
    my ($self, $object, $outers, $inners ) = @_;

    my $bbox = Math::Polygon::Tree::polygon_bbox([ map { @$_ } @$outers ]);
    my $area = {
        data  => $object,
        bound => Math::Polygon::Tree->new( @$outers ),
        ( $inners && @$inners
            ? ( inners => Math::Polygon::Tree->new( @$inners ) )
            : ()
        ),
    };

    $self->insert( $area, @$bbox );
    $self->{_count} ++;
    return;
}


=method find_area

    my $object = $area_tree->find_area( @points );

Returns object for area containing all points

=cut

sub find_area {
    my ($self, @points) = @_;
    return if !$self->{_count};

    my $possible_areas =
        reduce { _array_intersection($a, $b) }
        map { my @objects; $self->query_point( @$_, \@objects ); \@objects }
        @points;

    return if !$possible_areas;
    return if !@$possible_areas;

    my $object = first {
        $_->{bound}->contains_points(@points)
        && ( !$_->{inners} || !$_->{inners}->contains_points(@points) )
    }
    @$possible_areas;

    return if !$object;
    return $object->{data};
}


sub _array_intersection {
    my ($ar1, $ar2) = @_;
    my %ar2 = map {($_ => 1)} @$ar2;
    my @ar = grep {$ar2{$_}} @$ar1;
    return \@ar;
}


1;

