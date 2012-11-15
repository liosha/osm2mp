package TextFilter;

# ABSTRACT: text filter chain

# $Id$


use 5.010;
use strict;
use warnings;
use utf8;

use autodie;
use Carp;
use Encode;


our %PREDEFINED_FILTER = (
    upcase      => sub { return uc shift },
    translit    => sub { require Text::Unidecode; return Text::Unidecode::unidecode( shift ) },
);




=method new

    my $finter_chain = TextFilter->new();

Constructor

=cut

sub new {
    my ($class) = @_;
    return bless { chain => [] }, $class;
}


=method add_filter

    $filter_chain = add_filter( $filter_name );
    $filter_chain = add_filter( $filter_sub );

=cut

sub add_filter {
    my ($self, $filter) = @_;

    $filter = $PREDEFINED_FILTER{$filter}  if $PREDEFINED_FILTER{$filter};
    croak "Bad filter: $filter"  if ref $filter ne 'CODE';

    push @{ $self->{chain} }, $filter;
    return;
}


=method add_perlio_filter

    $filter_chain->add_perlio_filter( $filter );

wrapper for perlio filters - slow!

=cut

sub add_perlio_filter {
    my ($self, $perlio) = @_;
    my $package = "PerlIO::via::$perlio";

    eval "require $package" or eval "require $perlio"
        or croak "Invalid perlio filter: $package";

    return $self->add_filter( sub {
            my $dump = q{};
            open my $fh, ">:utf8:via($perlio):utf8", \$dump;
            print {$fh} shift();
            close $fh;

            return decode 'utf8', $dump;
        });
}


=method add_table_filter

    $filter_chain->add_table_filter( $filename );
    $filter_chain->add_table_filter( { $bad_letter => $good_letter, ... } );

=cut

sub add_table_filter {
    my ($self, $table) = @_;

    if ( !ref $table ) {
        require YAML;
        $table = YAML::LoadFile( $table );
    }

    return $self->add_filter( sub {
            return join q{}, map { $table->{$_} // $_ } unpack '(Z)*', shift;
        });
}



=method apply

    my $filtered_text = $filter_chain->apply( $text );

=cut

sub apply {
    my ($self, $text) = @_;

    for my $filter ( @{ $self->{chain} } ) {
        $text = $filter->($text);
    }
    return $text;
}





1;

