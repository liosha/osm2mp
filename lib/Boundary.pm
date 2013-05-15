package Boundary;

# ABSTRACT: map boundary

# $Id$


use 5.010;
use strict;
use warnings;

use autodie;
use Carp;

use Math::Polygon;
use Math::Polygon::Tree 0.065;

use Clipper;



=method new

    my $bound = Boundary->new( \@points );
    my $bound = Boundary->new( $poly_file_name );

Constructor

=cut

sub new {
    my ($class, $boundary) = @_;
    
    if ( !ref $boundary ) {
        my @bound;
        open my $pf, '<', $boundary;
        while ( my $line = readline $pf ) {
            if ( $line =~ /^\d/x ) {
                @bound = ();
            }
            elsif ( $line =~ /^\s+ ([0-9.E+-]+) \s+ ([0-9.E+-]+) /xms ) {
                push @bound, [ $1+0, $2+0 ];
            }
            elsif ( $line =~ /^END/x ) {
                # !!! first ring only!
                @bound = reverse @bound  if Math::Polygon->new( @bound )->isClockwise();
                last;
            }
        }
        close $pf;
        $boundary = \@bound;
    }

    my $gpc = Clipper->new();
    $gpc->add_polygon( $boundary, 0 );

    my $self = {
        chain => $boundary,
        tree  => Math::Polygon::Tree->new($boundary, {prepare_rough=>1}),
        gpc   => $gpc,
    };  

    return bless $self, $class;
}



=method get_points

    my $chain = $bound->get_points();

=cut

sub get_points {
    my ($self) = @_;
    return $self->{chain};
}


=method contains

    if ( $bound->contains([$x, $y]) )  { ... }

=cut

sub contains {
    my ($self, $point) = @_;
    return $self->{tree}->contains( $point );
}



1;

