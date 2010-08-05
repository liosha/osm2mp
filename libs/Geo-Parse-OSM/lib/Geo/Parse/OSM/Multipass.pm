package Geo::Parse::OSM::Multipass;
use base qw{ Geo::Parse::OSM };

use strict;
use warnings;

use List::MoreUtils qw{ true };
use Data::Dump 'dd';


my %role_type = (
    q{}      => 'outer',
    outer    => 'outer',
    border   => 'outer',
    exclave  => 'outer',
    inner    => 'inner',
    enclave  => 'inner',
);


sub new {
    my $class = shift;

    our $self = $class->SUPER::new( shift );

    $self->{nod}            = {};
    $self->{waychain}       = {};
    $self->{mpoly}          = {};
    $self->{ways_to_load}   = {};

    our %param = @_;

    ## First pass - load multipolygon parameters

    my $osm_pass1 = sub {
        my ($obj) = @_;

        if ( $obj->{tag}->{type} =~ /multipolygon|boundary/ ) {
            # old-style multipolygons - load lists of inner rings
            if ( (true { $_->{role} eq 'outer' } @{ $obj->{members} }) == 1 ) {
                $self->{mpoly}->{$obj->{id}} = 
                    [ map { $_->{ref} } grep { $_->{role} eq 'inner' } @{ $obj->{members} } ];
            }
            # advanced multipolygons - load lists of ways
            for my $member ( @{ $obj->{members} } ) {
                next unless $member->{type} eq 'way';
                next unless exists $role_type{ $member->{role} };
                $self->{ways_to_load}->{ $member->{ref} } = 1;
            }
        }

        &{ $param{pass1} }( $obj )  if exists $param{pass1};
    };

    $self->SUPER::parse( $osm_pass1, only => 'relation' );

    ## Second pass - load necessary primitives

    $self->seek_to_nodes();

    my $osm_pass2 = sub {
        my ($obj) = @_;

        if ( $obj->{type} eq 'node' ) {
            $self->{nod}->{ $obj->{id} } = [ $obj->{lon}, $obj->{lat} ];
        }
        elsif ( $obj->{type} eq 'way' && exists $self->{ways_to_load}->{$obj->{id}} ) {
            $self->{waychain}->{ $obj->{id} } = $obj->{chain};
            delete $self->{ways_to_load}->{$obj->{id}};
        }

        &{ $param{pass2} }( $obj )  if exists $param{pass2};
    };

    $self->SUPER::parse( $osm_pass2 );

    $self->seek_to_nodes();

    bless ($self, $class);
    return $self;
}


sub parse {

    my $self = shift;
    our $callback = shift;
    my %prop = @_;

    my $parse_extent = sub {
        &$callback( @_ );
    };
    
    $self->SUPER::parse( $parse_extent, @_ );    
}


1;

=head1 NAME

Geo::Parse::OSM::Multipass - Multipass OpenStreetMap file parser


=head1 SYNOPSIS

Geo::Parse::OSM::Multipass extends Geo::Parse::OSM class to resolve geometry.

    use Geo::Parse::OSM::Multipass;

    my $osm = Geo::Parse::OSM::Multipass->new( 'planet.osm.gz' );
    $osm->seek_to_relations;
    $osm->parse( sub{ warn $_[0]->{id}  if  $_[0]->{user} eq 'Alice' } );


=head1 METHODS

=head2 new

    my $osm = Geo::Parse::OSM::Multipass->new( 'planet.osm' );

Creates parser instance and makes two passes:
1 - only relations, create the list of multipolygon parts
2 - loads those parts and nodes

=head2 parse

Same as in Geo::Parse::OSM, but callback object has extra fields

=head1 AUTHOR

liosha, C<< <liosha at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-geo-parse-osm at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Geo-Parse-OSM>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Geo::Parse::OSM


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Geo-Parse-OSM>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Geo-Parse-OSM>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Geo-Parse-OSM>

=item * Search CPAN

L<http://search.cpan.org/dist/Geo-Parse-OSM/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2010 liosha.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut
