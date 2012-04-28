package WriterSQL;

# ABSTRACT: simple plain SQL writer

# $Id$


use 5.010;
use strict;
use warnings;
use autodie;

use Carp;

use Encode;



=method new( param => $value )

Create writer instance
Options:

=cut

sub new {
    my ($class, %opt) = @_;
    
    my $self = bless { output => {} }, $class;
    return $self;
}



=method _process

=cut

sub _process {
    my ($self, $template, $data) = @_;
    $self->{_count}->{$template} ++;
    # !!!
    use YAML;
    return Dump $template, $data;
}


=method output( $template, $data )

=cut

sub output {
    my ( $self, $template, $data ) = @_;

    # !!! points only
    #return if $template ne 'point';

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
        binmode $fh, ':utf8';
        $self->{output}->{$group} = $fh;
    }

    say {$fh} $self->_process( $template => $data );
    return;
}



=method finalize()

=cut

sub finalize {
    my ($self) = @_;

    for my $group ( keys %{ $self->{output} } ) {
        my $fh = delete $self->{output}->{$group};
        #print {$fh} $self->_process( footer => {} );
        close $fh;
    }

    return;
}


=method get_getopt()

=cut

sub get_getopt {
    my ($self) = @_;
    return (
        'o|output=s'            => sub { $self->{output_base} = $_[1] ~~ '-' ? q{} : $_[1] // q{} },
        'multiout=s'            => \$self->{multiout},
        'field=s%'              => sub { $self->{fixed_field}->{$_[1]} = $_[2] },
    );
}


=method get_usage()

=cut

sub get_usage {
    my ($self) = @_;
    return (
        [ 'o|output' => 'output file', 'stdout' ],
        [ multiout   => 'multiwriter base field (experimental)' ],
        [ 'field <key>=<val>' => 'fixed field value' ],
    );
}


1;

