package LangTransform::YaTranslate;

# $Id$

# ABSTRACT: translation using translate.yandex.ru api v1.5

use 5.010;
use strict;
use warnings;
use utf8;

use Carp;

use File::Slurp;
use JSON;
use LWP::Simple;
use LWP::Protocol::https;
use URI::Escape;


our $PRIORITY = 2;
our $API_URL = 'https://translate.yandex.net/api/v1.5/tr.json';
our $API_KEY;

our $DEBUG = $ENV{YATR_DEBUG};


sub init {
    my (undef, %callback) = @_;

    my $init_cb = $callback{register_transformer};
    $callback{register_getopt}->([
        'lt-yatr-key=s' => sub { _set_api_key($_[1]); $init_cb->($_) for get_transformers() },
        'lt-yatr-key <key>' => 'api key or @keyfile',
    ]);

    $callback{register_getopt}->([
        'lt-yatr-cache-dir=s' => \$LangTransform::YaTranslate::Cache::CACHE_DIR,
        'lt-yatr-cache-dir <dir>' => 'directory to store cache',
    ]);
    
    return;
}


sub _set_api_key {
    my ($key) = @_;
    if ( my ($file) = $key =~ / ^ \@ (.*) /xms ) {
        my $data = read_file $file;
        ($key) = $data =~ /(\S+)/xms;
    }

    $API_KEY = $key;
    return;
}


sub _api_request {
    my ($method, %arg) = @_;
    return if !$API_KEY;

    my $request = "$API_URL/$method?key=$API_KEY" . join( q{}, map { "&$_=" . uri_escape_utf8($arg{$_}) } keys %arg );
    say STDERR "request: $request"  if $DEBUG;

    my $response = get $request;
    say STDERR "response: ${\( $response // q{} )}\n" if $DEBUG;

    my $result = decode_json($response);
    return $result;
}


sub _get_langs {
    my $api_response = _api_request( getLangs => ( ui => 'en' ) );
    my $dirs = $api_response->{dirs};
    croak "Bad api response"  if ref $dirs ne 'ARRAY';

    return map {[ split /-/x ]} @$dirs;
}


sub _make_transformer {
    my ($from, $to) = @_;
    
    my $lang_pair = "$from-$to";
    my $cache = LangTransform::YaTranslate::Cache->new("yatr-$lang_pair");

    return sub {
        my ($text) = @_;

        my $cached_result = $cache->get($text);
        return $cached_result  if defined $cached_result;

        my $response = eval { _api_request( translate => ( lang => $lang_pair, text => $text ) ) };
        if ( !defined $response ) {
            warn "yatr: failed request $lang_pair for $text";
            return undef;
        }

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
use FindBin;

our @ENGINES = (
    [ SQLite_File   => 'sqlite' ],
    [ DB_File       => 'dbfile' ],
);

our $CACHE_DIR = $ENV{YATR_CACHE_DIR} // $FindBin::Bin // q{.};

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
            tie %cache, $package => "$CACHE_DIR/$self->{id}.$ext", O_CREAT | O_RDWR;
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
