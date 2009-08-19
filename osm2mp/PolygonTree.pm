package PolygonTree;

use strict;
use Carp;

use List::Util qw{ sum min max };
use List::MoreUtils qw{ uniq };

use Math::Polygon;
use Math::Geometry::Planar::GPC::Polygon qw{ new_gpc };

use Data::Dump qw{ dd };


my $MAX_LEAF_POINTS = 16;       # minimum 6


sub new {
    my $class = shift;
    my $self  = {};

    ##  load and close polys, calc bbox
    while ( my $chain_ref = shift ) {
        my @epoint = ();
        push @epoint, $chain_ref->[0]
            unless  $chain_ref->[0] ~~ $chain_ref->[-1];

        my $poly = Math::Polygon->new( @$chain_ref, @epoint );
        push @{$self->{poly}}, $poly;

        my ($xmin, $ymin, $xmax, $ymax) = $poly->bbox();
        $self->{xmin} = $xmin       if  !(exists $self->{xmin})  ||  $xmin < $self->{xmin};
        $self->{xmax} = $xmax       if  !(exists $self->{xmax})  ||  $xmax > $self->{xmax};
        $self->{ymin} = $ymin       if  !(exists $self->{ymin})  ||  $ymin < $self->{ymin};
        $self->{ymax} = $ymax       if  !(exists $self->{ymax})  ||  $ymax > $self->{ymax};
    }

    my $nrpoints = sum map { $_->nrPoints() } @{$self->{poly}};

    ##  full square?
    if ( $nrpoints == 5 ) {
        my $poly = $self->{poly}->[0];
        my @xs = uniq map { $_->[0] } $poly->points();
        my @ys = uniq map { $_->[1] } $poly->points();

        if ( @xs == 2  &&  @ys == 2 ) {     # simple check
            $self->{full} = 1;
        }
    }

    ##  branch if big poly
    if ( $nrpoints > $MAX_LEAF_POINTS ) {
        # 0 - horisontal split, 1 - vertical
        $self->{hv}  =  my $hv  =  ($self->{xmax}-$self->{xmin}) < ($self->{ymax}-$self->{ymin});
        $self->{avg} =  my $avg =  $hv  ?  ($self->{ymax}+$self->{ymin})/2  :  ($self->{xmax}+$self->{xmin})/2;

        my $gpc = new_gpc();
        for my $poly ( @{$self->{poly}} ) {
            $gpc->add_polygon( [ $poly->points() ], 0 );
        }

        my $part1_gpc = new_gpc();
        $part1_gpc->add_polygon( [
                [ $self->{xmin}, $self->{ymin} ],
                $hv  ?  [ $self->{xmin}, $avg          ]  :  [ $self->{xmin}, $self->{ymax} ],
                $hv  ?  [ $self->{xmax}, $avg          ]  :  [ $avg         , $self->{ymax} ],
                $hv  ?  [ $self->{xmax}, $self->{ymin} ]  :  [ $avg         , $self->{ymin} ],
                [ $self->{xmin}, $self->{ymin} ],
            ], 0 );
        $part1_gpc = $gpc->clip_to( $part1_gpc, 'INTERSECT' );
        $self->{part1} = PolygonTree->new( $part1_gpc->get_polygons() );

        my $part2_gpc = new_gpc();
        $part2_gpc->add_polygon( [
                [ $self->{xmax}, $self->{ymax} ],
                $hv  ?  [ $self->{xmax}, $avg          ]  :  [ $self->{xmax}, $self->{ymin} ],
                $hv  ?  [ $self->{xmin}, $avg          ]  :  [ $avg         , $self->{ymin} ],
                $hv  ?  [ $self->{xmin}, $self->{ymax} ]  :  [ $avg         , $self->{ymax} ],
                [ $self->{xmax}, $self->{ymax} ],
            ], 0 );
        $part2_gpc = $gpc->clip_to( $part2_gpc, 'INTERSECT' );
        $self->{part2} = PolygonTree->new( $part2_gpc->get_polygons() );

        delete $self->{poly};
    }

    bless ($self, $class);
    return $self;
}



sub contains {
    my $self  = shift;
    my $point = shift;
    my ($px, $py) = @$point;

    return 0    if  $px < $self->{xmin}  ||  $px > $self->{xmax}
                 || $py < $self->{ymin}  ||  $py > $self->{ymax};

    return $self->{full}    if  exists $self->{full};

    if ( exists $self->{hv} ) {
        if ( $point->[$self->{hv}] < $self->{avg} ) {
            return $self->{part1}->contains( $point );
        }
        else {
            return $self->{part2}->contains( $point );
        }
    }

    if ( exists $self->{poly} ) {
        for my $poly ( @{$self->{poly}} ) {
            return 1    if $poly->contains( $point );
        }
    }

    return 0;
}


sub contains_bbox_rough {
    my $self  = shift;
    my ($xmin, $ymin, $xmax, $ymax) = @_;

    return 0
        if   $xmax < $self->{xmin}  ||  $xmin > $self->{xmax}
         ||  $ymax < $self->{ymin}  ||  $ymin > $self->{ymax};

    if (  $xmin > $self->{xmin}  &&  $xmax < $self->{xmax}
      &&  $ymin > $self->{ymin}  &&  $ymax < $self->{ymax} ) {

        return $self->{full}    if  exists $self->{full};

        if ( exists $self->{hv} ) {
            if ( $self->{hv} ) {
                return $self->{part1}->contains_bbox_rough( @_ )
                    if  $ymax < $self->{avg}; 
                return $self->{part2}->contains_bbox_rough( @_ )
                    if  $ymin > $self->{avg}; 
            }
            else {
                return $self->{part1}->contains_bbox_rough( @_ )
                    if  $xmax < $self->{avg}; 
                return $self->{part2}->contains_bbox_rough( @_ )
                    if  $xmin > $self->{avg}; 
            }
        }
    }

    return undef;     
}


sub contains_polygon_rough {
    my $self = shift;
    my $poly = shift; 

    my @bbox = (
        ( min map { $_->[0] } @$poly ),
        ( min map { $_->[1] } @$poly ),
        ( max map { $_->[0] } @$poly ),
        ( max map { $_->[1] } @$poly ),
    );

    return $self->contains_bbox_rough( @bbox );
}



sub bbox {
    my $self  = shift;
    return ( $self->{xmin}, $self->{ymin}, $self->{xmax}, $self->{ymax} );
}



1;

