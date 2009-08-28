package Math::Polygon::Tree;

use warnings;
use strict;
use Carp;

our $VERSION = '0.02';

use List::Util qw{ sum min max };
use List::MoreUtils qw{ uniq };

use Math::Polygon;
use Math::Geometry::Planar::GPC::Polygon qw{ new_gpc };


my $MAX_LEAF_POINTS = 16;       # minimum 6


sub new {
    my $class = shift;
    my $self  = {};

    ##  load and close polys, calc bbox
    while ( my $chain_ref = shift ) {
        croak "Polygon should be a reference to array of points" 
            unless ref $chain_ref;

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

        # other clipper?
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
        $self->{part1} = Math::Polygon::Tree->new( $part1_gpc->get_polygons() );

        my $part2_gpc = new_gpc();
        $part2_gpc->add_polygon( [
                [ $self->{xmax}, $self->{ymax} ],
                $hv  ?  [ $self->{xmax}, $avg          ]  :  [ $self->{xmax}, $self->{ymin} ],
                $hv  ?  [ $self->{xmin}, $avg          ]  :  [ $avg         , $self->{ymin} ],
                $hv  ?  [ $self->{xmin}, $self->{ymax} ]  :  [ $avg         , $self->{ymax} ],
                [ $self->{xmax}, $self->{ymax} ],
            ], 0 );
        $part2_gpc = $gpc->clip_to( $part2_gpc, 'INTERSECT' );
        $self->{part2} = Math::Polygon::Tree->new( $part2_gpc->get_polygons() );

        delete $self->{poly};
    }

    bless ($self, $class);
    return $self;
}



sub contains {
    my $self  = shift;
    my $point = shift;
    croak "Point should be a reference" 
        unless ref $point;

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
    croak "Box should be 4 values xmin, ymin, xmax, ymax"
        unless @_ == 4;

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

    croak "Polygon should be a reference to array of points" 
        unless ref $poly;

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

__END__


=head1 NAME

Math::Polygon::Tree - Class for fast check if point is inside polygon

=head1 SYNOPSIS

Math::Polygon::Tree creates a B-tree of polygon parts for fast check if object is inside this polygon.
This method is effective if polygon has hundreds or more segments.

    use Math::Polygon::Tree;

    my $poly  = [ [0,0], [0,2], [2,2], ... ];
    my $bound = Math::Polygon::Tree->new( $poly );

    if ( $bound->contains( [1,1] ) )  { ... }


=head1 METHODS

=head2 Constructor

Takes polygons (at least one) and creates a tree structure. All polygons are outer, inners in not implemented.

    my $poly1 = [ [0,0], [0,2], [2,2], ... ];   
    ...
    my $bound = Math::Polygon::Tree->new( $poly1, $poly2, ... );

=head2 contains

Checks if point is inside bound polygon.
Returns 1 if point is inside polygon or 0 otherwise.

    if ( $bound->contains( [1,1] ) )  { ...

=head2 contains_bbox_rough

Checks if box is inside bound polygon.
Returns 1 if box is inside polygon, 0 if box is outside polygon or B<undef> if it 'doubts'. 

    my ($xmin, $ymin, $xmax, $ymax) = ( 1, 1, 2, 2 );
    if ( $bound->contains_bbox_rough( $xmin, $ymin, $xmax, $ymax ) )  { ... }

=head2 contains_polygon_rough

Checks if polygon is inside bound polygon.
Returns 1 if inside, 0 if outside or B<undef> if 'doubts'. 

    if ( $bound->contains_polygon_rough( [ [1,1], [1,2], [2,2], ... ] ) )  { ... }

=head1 AUTHOR

liosha, C<< <liosha at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-math-polygon-tree at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Math-Polygon-Tree>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Math::Polygon::Tree


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Math-Polygon-Tree>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Math-Polygon-Tree>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Math-Polygon-Tree>

=item * Search CPAN

L<http://search.cpan.org/dist/Math-Polygon-Tree/>

=back


=head1 COPYRIGHT & LICENSE

Copyright 2009 liosha.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut
