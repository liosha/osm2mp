use 5.010;
use strict;
use warnings;
use autodie;

package OSM;

# ABSTRACT: preload OSM data in useful way

# $Id$

use Carp;
use List::MoreUtils qw/ all /;

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

                my $chain = $obj->{nd};
                return if !$chain || @$chain < 2;
                # !!! filter dupes!

                $chains->{$id} = $chain;

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

                if ( my $type = $tags->{type} ) {
                    my $id = $obj->{attr}->{id};
                    $relations->{$type}->{$id} = $obj->{member};
                }
                return;
            },
        %{ $opt{handlers} || {} },
    );

    my $parser = Geo::Openstreetmap::Parser->new( %parse_handler );
    $parser->parse($fh);

    return;
}


1;

