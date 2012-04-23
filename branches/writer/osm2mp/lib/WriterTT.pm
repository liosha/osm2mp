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
use Template::Context;
use YAML;


=method new( param => $value )

Create writer instance
Options:
    templates_file
    codepage
    version

    filters => \@filter_name_list,
    templates => { tt_name => $tt_text, ... },

=cut

sub new {
    my ($class, %opt) = @_;
    
    my $self = bless { output => {}, version => $opt{version} }, $class;
    my $ttc = $self->{tt_context} = Template::Context->new();

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

    $self->_register_codepage( $opt{codepage} );



    ##  Initialize filters
    my $ttf = $ttc->{LOAD_FILTERS}->[0];

    # predefined 
    $ttf->store( 'upcase',   sub {  my $text = uc shift;  $text =~ s/ \b 0X (?=\w)/0x/xms;  return $text;  } );
    $ttf->store( 'translit', sub {  require Text::Unidecode; return Text::Unidecode::unidecode( shift );  } );

    # custom plugins
    for my $filter ( @{ $opt{filters} || [] } ) {
        my ($filter_sub) = $ttf->fetch( $filter );
        next if $filter_sub;
        
        my $filter_package = "Template::Plugin::Filter::$filter";
        eval "require $filter"
            or eval "require $filter_package"
            or croak $@;

        my $filter_object = $filter_package->new( $ttc );
        $filter_object->init( $filter );
    }

    # master filter chain
    $ttf->store( 'mp_filter', sub {
            my $text = shift;
            for my $filter ( @{ $opt{filters} || [] } ) {
                $text = $ttf->fetch($filter)->($text);
            }
            return $text;
        });

    return $self;
}


=method _process( tt_name => \%vars )

internal

=cut

sub _process {
    my ($self, $tt_name, $vars) = @_;
    $self->{_count}->{$tt_name} ++;
    return $self->{tt_context}->process($tt_name => $vars);
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
    );
}


1;

