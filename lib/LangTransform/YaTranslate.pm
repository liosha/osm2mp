package LangTransform::YaTranslate;

# $Id$

# ABSTRACT: translation using translate.yandex.ru api

use 5.010;
use strict;
use warnings;
use utf8;

use Carp;

use JSON;
use LWP::Simple;
use URI::Escape;


our $PRIORITY = -1;  # don't use without explicit selection
our $API_URL = 'http://translate.yandex.net/api/v1/tr.json';


sub _get_langs {
    my $api_response = get "$API_URL/getLangs";
    my $dirs = decode_json($api_response)->{dirs};
    croak "Bad api response"  if ref $dirs ne 'ARRAY';

    return map {[ split /-/x ]} @$dirs;
}


sub _make_transformer {
    my ($from, $to) = @_;
    my %cache;

    return sub {
        my ($text) = @_;

        # TODO: need external cache (sqlite?)
        my $cached_result = $cache{$text};
        return $cached_result  if defined $cached_result;

        my $api_response = get "$API_URL/translate?lang=$from-$to&text=" . uri_escape_utf8($text);
        my $response = decode_json $api_response;

        return undef if $response->{code} ne 200;

        # TODO: check if result really on desired lang
        my $result = join q{ }, @{ $response->{text} };
        $cache{$text} = $result;
        return $result;
    };
}


sub get_transformers {
    return map {
        my ($from, $to) = @$_;
        {
            id => "yatr_${from}_$to",
            from => $from,
            to => $to,
            priority => $PRIORITY,
            transformer => _make_transformer($from, $to),
        }} _get_langs();
}

