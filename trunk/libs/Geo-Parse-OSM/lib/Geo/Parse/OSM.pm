package Geo::Parse::OSM;

use warnings;
use strict;
use Carp;

use HTML::Entities;

use Data::Dump 'dd';


sub parse_file {
    my $class = shift;

    my ( $file, $callback ) = @_;
    my $handle;

    unless ( open $handle, '<:encoding(utf8)', $file ) {
        carp "Can't open file: $file";
    }

    my %object;
    while ( my $line = readline $handle ) {

        # start of object
        if ( my ($obj) = $line =~ m{<(node|way|relation)} ) {
            %object = ( type => $obj );
            
            # ALL attributes
            my @res = $line =~ m{(\w+)=(?:"([^"]*)"|'([^']*)')}g;
            while (@res) {
                my ( $attr, $val1, $val2 ) = ( shift @res, shift @res, shift @res );
                $object{$attr} = decode_entities( $val1 || $val2 );
            }
            
            if ( $line =~ m{/>\s*$} ) {
                &$callback( \%object );
                %object = ();
            }
        }

        # tag
        if ( my ($key, undef, $val) = $line =~ m{<tag.* k=["']([^"']+)["'].* v=(["'])(.+)\2} ) {
            $object{tag}->{$key} = decode_entities( $val );
        }

        # node ref
        if ( my ($ref) = $line =~ m{<nd.* ref=["']([^"']+)["']} ) {
            push @{$object{chain}}, $ref;
        }

        # relation member
        if ( my ($type, $ref, $role) = $line =~ /<member.* type=["']([^"']+)["'].* ref=["']([^"']+)["'].* role=["']([^"']*)["']/ ) {
            push @{$object{members}}, { type => $type, ref => $ref, role => $role };
        }

        # end of object
        if ( my ($obj) = $line =~ m{</(node|way|relation)} ) {
            &$callback( \%object );
            %object = ();
        }

    }

}


=head1 NAME

Geo::Parse::OSM - Openstreetmap file parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS


    use Geo::Parse::OSM;
    use Data::Dumper;

    Geo::Parse::OSM->parse_file( 'planet.osm', sub{ print Dumper $_[0] } );


=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 SUBROUTINES/METHODS

=head2 parse_file

Parses contents of file and executes callback function for every object.


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

1; # End of Geo::Parse::OSM
