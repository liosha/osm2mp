package Clipper;

# ABSTRACT: clipping library wrapper

# $Id$


use 5.010;
use strict;
use warnings;
use Carp;

our $CLIPPER_CLASS;


BEGIN {
    our @clippers = qw/
        Math::Geometry::Planar::GPC::Polygon
        Math::Geometry::Planar::GPC::PolygonXS
    /;

    for my $class ( @clippers ) {
        eval "require $class"  or next;
        $CLIPPER_CLASS = $class;
        last;
    }
}

=method new

    my $gpc = Clipper->new();

Constructor.

=cut

sub new {
    my ($class) = @_;

    croak "No clipper class available"  if !$CLIPPER_CLASS;

    return $CLIPPER_CLASS->new_gpc();
}


1;

