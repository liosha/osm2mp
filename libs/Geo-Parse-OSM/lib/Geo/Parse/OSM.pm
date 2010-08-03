package Geo::Parse::OSM;

use warnings;
use strict;
use Carp;

our $VERSION = '0.221';

use Encode;
use HTML::Entities;
use IO::Uncompress::AnyUncompress qw($AnyUncompressError);


sub new {
    my $class = shift;

    my $self = {
        file        => shift,
        node        => undef,
        way         => undef,
        relation    => undef,
    };

    $self->{stream} = new IO::Uncompress::AnyUncompress $self->{file}
        or croak "Error with $self->{file}: $AnyUncompressError";

    bless ($self, $class);
    return $self;
}


sub parse {

    my $self = shift;
    my $callback = shift;

    my %prop = @_;

    my %object;

    if ( exists $self->{saved} ) {
        my $res = &$callback( $self->{saved} );

        delete $self->{saved}   unless defined $res && $res eq 'stop' && $prop{save};
        return  if  defined $res && $res eq 'stop';
    }

    my $pos = tell $self->{stream};

    LINE:
    while ( my $line = decode( 'utf8', $self->{stream}->getline() ) ) {

        # start of object
        if ( my ($obj) = $line =~ m{^\s*<(node|way|relation)} ) {

            $self->{$obj} = $pos    unless defined $self->{$obj};
            
            next LINE if exists $prop{only} && $prop{only} ne $obj;

            %object = ( type => $obj );
            
            # ALL attributes
            my @res = $line =~ m{(\w+)=(?:"([^"]*)"|'([^']*)')}g;
            while (@res) {
                my ( $attr, $val1, $val2 ) = ( shift @res, shift @res, shift @res );
                $object{$attr} = decode_entities( $val1 || $val2 );
            }
        }
        # tag
        elsif ( %object && ( my ($key, undef, $val) = $line =~ m{^\s*<tag.* k=["']([^"']+)["'].* v=(["'])(.+)\2} ) ) {
            $object{tag}->{$key} = decode_entities( $val );
        }
        # node ref
        elsif ( %object && ( my ($nref) = $line =~ m{^\s*<nd.* ref=["']([^"']+)["']} ) ) {
            push @{$object{chain}}, $nref;
        }
        # relation member
        elsif ( %object && ( my ($type, $mref, $role) = $line =~ /^\s*<member.* type=["']([^"']+)["'].* ref=["']([^"']+)["'].* role=["']([^"']*)["']/ ) ) {
            push @{$object{members}}, { type => $type, ref => $mref, role => $role };
        }

        # end of object
        if ( %object && ( my ($obj) = ( $line =~ m{^\s*</(node|way|relation)} or $line =~ m{^\s*<(node|way|relation).*/>} ) ) ) {
            my $res = &$callback( \%object );
            if ( defined $res && $res eq 'stop' ) {
                $self->{saved} = \%object    if $prop{save};
                return;
            }
            %object = ();
        }

    } continue { $pos = tell $self->{stream} }

    for my $type ( qw{ node way relation } ) {
        $self->{$type} = $pos    unless defined $self->{$type};
    }
}


sub seek_to {
    my $self = shift;
    my $obj = shift;

    return unless exists $self->{$obj};

    if ( defined $self->{$obj} ) {
        delete $self->{saved};
        $self->{stream} = new IO::Uncompress::AnyUncompress $self->{file}
            if tell $self->{stream} > $self->{node};
        seek $self->{stream}, $self->{$obj}, 0;
    }
    else {
        parse( $self, sub{ 'stop' }, only => $obj, save => 1 );
    }
}

sub seek_to_nodes     {  seek_to( shift(), 'node' )  }

sub seek_to_ways      {  seek_to( shift(), 'way' )  }

sub seek_to_relations {  seek_to( shift(), 'relation' )  }



sub parse_file {
    my $class = shift;

    my ( $file, $callback ) = @_;

    my $obj = Geo::Parse::OSM->new( $file );
    $obj->parse( $callback ); 
}


my %attrorder = (
    action      => 1,
    id          => 2,
    version     => 3,
    timestamp   => 4,
    uid         => 5,
    user        => 6,
    changeset   => 7,
    lat         => 8,
    lon         => 9,
);

my $enc = q{\x00-\x19<>&"'};

sub to_xml {
    my $class = shift;
    my %obj = %{ shift() };

    my $type = $obj{type}; 
    delete $obj{type};

    my $res = qq{  <$type }
        . join( q{ },
            map { qq{$_="} . encode("utf8",encode_entities($obj{$_}, $enc)) . q{"} }
            grep { ! ref $obj{$_} }
            sort { (exists $attrorder{$a} ? $attrorder{$a} : 999) <=> (exists $attrorder{$b} ? $attrorder{$b} : 999) or $a cmp $b } keys %obj );

    if ( grep { ref } values %obj ) {
        $res .= qq{>\n};
        if ( exists $obj{chain} ) {
            for my $nd ( @{$obj{chain}} ) {
                $res .= qq{    <nd ref="$nd"/>\n};
            }
        }
        if ( exists $obj{members} ) {
            for my $nd ( @{$obj{members}} ) {
                $res .= qq{    <member type="$nd->{type}" ref="$nd->{ref}" role="$nd->{role}"/>\n};
            }
        }
        if ( exists $obj{tag} ) {
            for my $tag ( sort keys %{$obj{tag}} ) {
                $res .= q{    <tag k="}
                    . encode("utf8",encode_entities($tag, $enc))
                    . q{" v="}
                    . encode("utf8",encode_entities($obj{tag}->{$tag}, $enc))
                    . qq{"/>\n};
            }
        }
        $res .= qq{  </$type>\n};
    }
    else {
        $res .= qq{/>\n};
    }

    return $res;
}


=head1 NAME

Geo::Parse::OSM - OpenStreetMap file parser

=head1 VERSION

Version 0.221

=head1 SYNOPSIS


    use Geo::Parse::OSM;

    my $osm = Geo::Parse::OSM->new( 'planet.osm.gz' );
    $osm->seek_to_relations;
    $osm->parse( sub{ warn $_[0]->{id}  if  $_[0]->{user} eq 'Alice' } );


=head1 METHODS

=head2 new

Creates parser instance and opens file

    my $osm = Geo::Parse::OSM->new( 'planet.osm' );

Compressed files (.gz, .bz2) are also supported.

=head2 parse

Parses file and executes callback function for every object.
Stops parsing if callback returns 'stop'

    $osm->parse( sub{ warn $_[0]->{id} and return 'stop' } );

It's possible to filter out unnecessary object types

    $osm->parse( sub{ ... }, only => 'way' );

=head2 seek_to

Seeks to the first object of selected type.

    $osm->seek_to( 'way' );

Can be slow on compressed files.

=head2 seek_to_nodes

=head2 seek_to_ways

=head2 seek_to_relations

    $osm->seek_to_ways;

=head1 FUNCTIONS

=head2 parse_file

Simple parser - parses contents of file and executes callback function for every object.

    Geo::Parse::OSM->parse_file( 'planet.osm', sub{ print Dumper $_[0] } );

=head2 to_xml

Returns xml representation of the object.

    sub callback {
        my ($obj) = @_;
        print Geo::Parse::OSM->to_xml( $obj );
    }
    Geo::Parse::OSM->parse_file( 'planet.osm', \&callback );


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
