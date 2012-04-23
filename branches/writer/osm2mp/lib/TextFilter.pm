package TextFilter;

# ABSTRACT: text filter chain

# $Id$


use 5.010;
use strict;
use warnings;

use autodie;
use Carp;


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

