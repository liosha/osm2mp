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

our $DEBUG;


sub init {
    my (undef, %callback) = @_;
    for my $tr ( get_transformers() ) {
        $tr->{plugin} = __PACKAGE__;
        $callback{register_transformer}->($tr);
    }
    
    return;
}


sub _get_langs {
    my $api_response = get "$API_URL/getLangs";
    my $dirs = decode_json($api_response)->{dirs};
    croak "Bad api response"  if ref $dirs ne 'ARRAY';

    return map {[ split /-/x ]} @$dirs;
}


sub _make_transformer {
    my ($from, $to) = @_;
    my $base_url = "$API_URL/translate?lang=$from-$to&text=";
    my $cache = LangTransform::YaTranslate::Cache->new("yatr-$from-$to");

    return sub {
        my ($text) = @_;

        my $cached_result = $cache->get($text);
        return $cached_result  if defined $cached_result;

        my $url = $base_url . uri_escape_utf8($text);
        say STDERR $url  if $DEBUG;
        my $api_response = get $url;
        my $response = decode_json $api_response;

        return undef if $response->{code} ne 200;

        # TODO: check if result really on desired lang
        my $result = join q{ }, @{ $response->{text} };
        $cache->set($text => $result);
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
        }
    } _get_langs();
}


1;


package LangTransform::YaTranslate::Cache;

use Encode;
use Fcntl qw(O_CREAT O_RDWR);

our @ENGINES = (
    [ SQLite_File   => 'sqlite' ],
    [ DB_File       => 'dbfile' ],
);


sub new {
    my $class = shift;
    my ($id) = @_;

    return bless { id => $id }, $class;
}

sub _get_cache {
    my $self = shift;
    return $self->{cache}  if exists $self->{cache};

    my %cache;
    for my $engine_info ( @ENGINES ) {
        my ($package, $ext) = @$engine_info;

        last if eval {
            require "$package.pm";
            tie %cache, $package => "$self->{id}.$ext", O_CREAT | O_RDWR;
            1;
        }
    }

    warn 'All cache engines failed; data will not be saved!'  if !tied %cache;

    $self->{cache} = \%cache;
    return \%cache;
}

sub set {
    my $self = shift;
    my ($k, $v) = map { encode utf8 => $_ } @_;

    my $cache = $self->_get_cache();
    $cache->{$k} = $v;
    return;
}

sub get {
    my $self = shift;
    my ($k) = map { encode utf8 => $_ } @_;

    my $cache = $self->_get_cache();
    return decode utf8 => $cache->{$k};
}



1;
