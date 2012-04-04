use 5.010;
use strict;
use warnings;

package WriterTT;

# $Id$

# ABSTRACT: writer via Template-toolkit

use Carp;

use Template::Context;


=method new( param => $value )

Create writer instance
Options:
    filters => \@filter_name_list,
    templates => { tt_name => $tt_text, ... },

=cut

sub new {
    my ($class, %opt) = @_;

    my $ttc = Template::Context->new();

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
            or die $@;
        
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


    ##  Initialize template blocks
    my $ttp = $ttc->{LOAD_TEMPLATES}->[0];

    $ttp->{STAT_TTL} = 2**31;
    while ( my ($tt_name, $tt_text) = each %{ $opt{templates} || {} } ) {
        $ttp->store( $tt_name, $ttc->template( \$tt_text ) );
    }

    my $self = { tt_context => $ttc };
    return bless $self, $class;
}


=method process( tt_name => \%vars )

=cut

sub process {
    my ($self, $tt_name, $vars) = @_;
    return $self->{tt_context}->process($tt_name => $vars);
}


=method print( $fh, tt_name => \%vars )

=cut

sub print {
    my ($self, $fh, $tt_name, $vars) = @_;
    print {$fh} $self->process($tt_name => $vars);
    return;
}

1;

