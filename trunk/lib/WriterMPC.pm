package WriterMPC;

# ABSTRACT: garmin shapefile writer

# $Id$


use 5.010;
use strict;
use warnings;
use autodie;

use Carp;

use Encode;

use Geo::Shapefile::Writer;
use TextFilter;

use YAML;


=method new( param => $value )

Create writer instance
Options:
    codepage

=cut

sub new {
    my ($class, %opt) = @_;
    
    my $self = bless {}, $class;

    ##  Encoding
    $self->_register_codepage( $opt{codepage} );

    ##  Initialize filters
    my $filter_chain = $self->{filter_chain} = TextFilter->new();

    return $self;
}




=method output( $template, $data )

=cut

sub output {
    my ( $self, $template, $data ) = @_;

    my $group = $self->{output_base} // q{};

    if ( !$self->{shp} ) {
        my $prefix = $self->{output_base} ? "$self->{output_base}." : q{};
        $self->{shp}->{points} = Geo::Shapefile::Writer->new("$prefix.points", 'POINT', 'type' );
        # ...
    }

    say Dump $template, $data; exit;
#    print {$fh} $self->_process( $template => $data );
    return;
}



=method finalize()

=cut

sub finalize {
    my ($self) = @_;

    for my $shp ( values %{ $self->{shp} } ) {
        $shp->finalize();
    }

    return;
}


{
my %enc_to_cp = (
    ( map {($_ => 65001)} qw/ utf8 utf-8 / ),
    ( map {/(cp(\d+))/xms}  grep {/^cp\d{3,}$/xms} Encode->encodings(':all') ),
);
my %cp_to_enc = reverse %enc_to_cp;

sub _register_codepage {
    my $self = shift;
    my $cp = lc( shift || 'utf8' );

    if ( $enc_to_cp{$cp} ) {
        $self->{codepage} = $cp;
    }
    elsif ( $cp_to_enc{$cp} ) {
        $self->{codepage} = $cp_to_enc{$cp};
    }
    else {
        croak "Unknown code page: $cp";
    }
    return;
}
}




=method get_getopt()

=cut

sub get_getopt {
    my ($self) = @_;
    return (
        'o|output=s'            => \$self->{output_base},
#        'mapid=s'               => sub { $self->{header_opts}->{ID} = $_[1] },
#        'mapname=s'             => sub { $self->{header_opts}->{Name} = $_[1] },
        'codepage=s'            => sub { $self->_register_codepage( $_[1] ) },
        'filter=s'              => sub { $self->{filter_chain}->add_filter( $_[1] ) },
        'upcase!'               => sub { $self->{filter_chain}->add_filter( 'upcase' ) },
        'translit!'             => sub { $self->{filter_chain}->add_filter( 'translit' ) },
        'perlio-filter|textfilter=s' => sub { $self->{filter_chain}->add_perlio_filter( $_[1] ) },
        'ttable=s'              => sub { $self->{filter_chain}->add_table_filter( $_[1] ) },
    );
}


=method get_usage()

=cut

sub get_usage {
    my ($self) = @_;
    return (
        [ 'o|output' => 'output file', 'stdout' ],
        [ 'codepage <num>' => 'output character encoding', $self->{header_opts}->{CodePage} // 'utf8' ],
        [ 'filter <name>' => 'add predefined filter' ],
        [ 'upcase' => 'same as --filter upcase' ],
        [ 'translit' => 'same as --filter translit' ],
        [ 'perlio-filter' => 'use perlio via-layer as filter' ],
        [ 'ttable' => 'character conversion table' ],
    );
}


1;

