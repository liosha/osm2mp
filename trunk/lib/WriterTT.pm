package WriterTT;

# ABSTRACT: writer via Template-toolkit

# $Id$

# TODO: need abstract writer base class?


use 5.010;
use strict;
use warnings;
use autodie;

use Carp;

use Encode;
use List::Util qw/ first /;

use Template;
use Template::Context;
use YAML;

use TextFilter;
use GarminTools;


=method new( param => $value )

Create writer instance
Options:
    templates_file
    codepage
    version

    filters => \@filter_name_list,

=cut

sub new {
    my ($class, %opt) = @_;
    
    my $self = bless { output => {}, version => $opt{version} }, $class;
    my $ttc = $self->{tt_context} = Template::Context->new(
        VARIABLES => {
            speed_code => \&_get_mp_speed_code,
        },
    );

    ##  Read options, preload templates
    my $cfg_file = $opt{templates_file} || $opt{config_file};
    if ( $cfg_file ) {
        my %cfg = YAML::LoadFile($cfg_file);

        my $templates = $cfg{output} || $cfg{templates} || {};
        my $ttp = $ttc->{LOAD_TEMPLATES}->[0];
        $ttp->{STAT_TTL} = 2**31;
        while ( my ($tt_name, $tt_text) = each %$templates ) {
            $ttp->store( $tt_name, $ttc->template( \$tt_text ) );
        }
    }

    # Extra header values
    if ( my $header = $opt{header} ) {
        $header = [ $header ]  if !ref $header;
        for my $header_opt ( @$header ) {
            my ($key, $val) = split /\s* = \s*/xms, $header_opt, 2;
            $self->{header_opts}->{$key} = $val;
        }
    }

    ##  Encoding
    $self->_register_codepage( $opt{codepage} );

    ##  Initialize filters
    $TextFilter::PREDEFINED_FILTER{upcase} = sub {
        my $text = uc shift;
        $text =~ s/ \b 0X (?=\w)/0x/xms;
        return $text;
    };

    my $filter_chain = $self->{filter_chain} = TextFilter->new();
    my $ttf = $ttc->{LOAD_FILTERS}->[0];
    $ttf->store( mp_filter => sub { $filter_chain->apply(@_) } );

    return $self;
}


=method _process( tt_name => \%vars )

internal

=cut
{
my %mp_addr_field = (
    house       => 'HouseNumber',
    street      => 'StreetDesc',
    city        => 'CityName',
    region      => 'RegionName',
    country     => 'CountryName',
    postcode    => 'Zip',
);

sub _process {
    my ($self, $tt_name, $vars) = @_;
    $self->{_count}->{$tt_name} ++;

    my $data = $vars->{data} || {};

    if ( $data->{city} ) {
        $data->{extra_fields}->{City} = 'Y';
    }

    if ( my $address = $data->{address} ) {
        my $garmin_address = GarminTools::get_garmin_address($address);
        while ( my ($k, $v) = each %$garmin_address ) {
            $data->{extra_fields}->{ $mp_addr_field{$k} } = $v;
        }
    }

    return $self->{tt_context}->process($tt_name => $vars);
}
}



=method output( $template, $data )

=cut

sub output {
    my ( $self, $template, $data ) = @_;

    my $multiout = $self->{multiout} // q{};
    my $group = $self->{output_base} && ($data->{$multiout} || $data->{opts}->{$multiout}) // q{};
    my $fh = $self->{output}->{$group};

    if ( !$fh ) {
        if ( $self->{output_base} ) {
            my $filename = $self->{output_base};
            if ( length $group ) {
                $filename =~ s/( \. \w+ )? $/.$group$1/xms;
            }
            open $fh, '>', $filename;
        }
        else {
            $fh = *STDOUT;
        }
        binmode $fh, $self->{binmode} || ':utf8';
        $self->{output}->{$group} = $fh;
        print {$fh} $self->_process( header => {
                $multiout => $group,
                opts => { %{$self->{header_opts}} },
                version => $self->{version},
            } );
    }

    print {$fh} $self->_process( $template => $data );
    return;
}



=method finalize()

=cut

sub finalize {
    my ($self) = @_;

    for my $group ( keys %{ $self->{output} } ) {
        my $fh = delete $self->{output}->{$group};
        print {$fh} $self->_process( footer => {} );
        close $fh;
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
        $self->{binmode} = ":encoding($cp)";
        $self->{header_opts}->{CodePage} = $enc_to_cp{$cp};
    }
    elsif ( $cp_to_enc{$cp} ) {
        $self->{binmode} = ":encoding($cp_to_enc{$cp})";
        $self->{header_opts}->{CodePage} = $cp;
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
        'o|output=s'            => sub { $self->{output_base} = $_[1] ~~ '-' ? q{} : $_[1] // q{} },
        'multiout=s'            => \$self->{multiout},
        'header|mp-header=s%'   => sub { $self->{header_opts}->{$_[1]} = $_[2] },
        'mapid=s'               => sub { $self->{header_opts}->{ID} = $_[1] },
        'mapname=s'             => sub { $self->{header_opts}->{Name} = $_[1] },
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
        [ multiout   => 'multiwriter base field (experimental)' ],
        [ 'header <key>=<val>' => 'extra header options' ],
        [ 'codepage <num>' => 'output character encoding', $self->{header_opts}->{CodePage} // 'utf8' ],
        [ 'filter <name>' => 'add predefined filter' ],
        [ 'upcase' => 'same as --filter upcase' ],
        [ 'translit' => 'same as --filter translit' ],
        [ 'perlio-filter' => 'use perlio via-layer as filter' ],
        [ 'ttable' => 'character conversion table' ],
    );
}



sub _get_mp_speed_code {
    my ($speed) = @_;

    state $tholds = [ 0, 10, 30, 50, 70, 85, 100, 120 ];
    my $speed_code = first { $speed >= $tholds->[$_] } reverse (1 .. 7);
    return $speed_code || 0;
}



1;

