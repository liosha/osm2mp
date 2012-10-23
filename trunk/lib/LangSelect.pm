package LangSelect;

# ABSTRACT: OSM tag language selector and converter

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::Util qw/ first /;
use Text::Unidecode;


sub new {
    my $class = shift;
    my (%opt) = @_;

    my %data = (
        target_lang     => $opt{target_lang} || q{},
        default_lang    => $opt{default_lang} || q{},
        tag_re          => {},
    );

    return bless \%data, $class;
}


sub get_value {
    my $self = shift;
    my ($base_key, $tags) = @_;

    my $re = $self->{tag_re}->{$base_key} ||= do {
        my $k = quotemeta $base_key;
        qr/ ^ $k (?: : (\w+) )? $ /xms
    };

    my %val = map { m/$re/xms ? (($1 || q{}) => $tags->{$_}) : () } keys %$tags;
    return if !%val;

    my $def_lang = $self->{default_lang};
    $val{$def_lang} //= $val{q{}}  if $def_lang && defined $val{q{}};

    # return value on desired lang if defined
    my $target_lang = $self->{target_lang};
    return $val{$target_lang}  if defined $val{$target_lang};

    # TODO: select and use lang converter

    # last chance: return unidecoded default value
    return unidecode( $val{q{}} // first {defined} values %$tags );
}



1;

