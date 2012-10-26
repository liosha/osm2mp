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


sub _translate {
    my ($from, $to, $text) = @_;

    # TODO: need cache (sqlite?)
    my $api_response = get "$API_URL/translate?lang=$from-$to&text=" . uri_escape_utf8($text);
    my $response = decode_json $api_response;

    return undef if $response->{code} ne 200;

    # TODO: check if result really on desired lang
    return join q{ }, @{ $response->{text} };
}


sub get_transformers {
    return map {
        my ($from, $to) = @$_;
        {
            id => "yatr_${from}_$to",
            from => $from,
            to => $to,
            priority => $PRIORITY,
            transformer => sub { _translate($from, $to, @_) },
        }} _get_langs();
}

