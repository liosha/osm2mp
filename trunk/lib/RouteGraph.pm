package RouteGraph;

# ABSTRACT: routing graph (draft)

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;



=method new

    my $rgraph = RouteGraph->new( %opts );

Options:

Constructor

=cut

sub new {
    my ($class, %opt) = @_;
    my $self = {
    };

    return bless $self, $class;
}




1;

