package LangSelect;

# ABSTRACT: OSM tag language selector and converter

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::Util qw/ first /;
use List::MoreUtils qw/ notall /;
use Text::Unidecode;



our $PLUGIN_BASE = 'LangTransform';

our %TRANSFORMER;
our %TRANSFORMERS_BY_LANG;

our @GETOPT = (
    [
        'lt-priority=s%'            => sub { shift; _set_priority (@_) },
        'lt-priority <id>=<val>'    => 'set tranformer priority',
    ],
    [
        'lt-dump'                   => sub { _dump_transformers(); exit },
        'lt-dump'                   => 'list registered transformers',
    ],
);


_init_plugins();



sub _init_plugins {

    for my $lib_dir ( @INC ) {
        my $plugin_dir = "$lib_dir/$PLUGIN_BASE";
        next if !-d $plugin_dir;

        for my $plugin_file ( glob "$plugin_dir/*.pm" ) {
            my ($plugin) = $plugin_file =~ m# [\\/] (\w+) \.pm $ #xms;
            my $plugin_package = "${PLUGIN_BASE}::$plugin";

            eval {
                require $plugin_file;
                $plugin_package->init(
                    register_getopt => \&_register_getopt,
                    register_transformer => sub { $_[0]->{plugin} = $plugin_package; _register_transformer(@_) },
                );
                1;
            }
            or warn "Init failed for $plugin_package: $@";
        }
    }
    
    return;
}


sub _register_getopt {
    my ($getopt) = @_;
    croak 'Wrong getopt data'  if @$getopt < 2;

    push @GETOPT, $getopt;
    return;
}


sub _register_transformer {
    my ($tr_data) = @_;
    croak 'Wrong transformer data'  if notall { $tr_data->{$_} } qw/ id transformer / ;

    my $tr_id = $tr_data->{id};
    $TRANSFORMER{$tr_id} = $tr_data;
    push @{ $TRANSFORMERS_BY_LANG{ $tr_data->{to} // q{} } }, $tr_data;

    return;
}



=head1 METHODS
=cut


sub new {
    my $class = shift;
    my (%opt) = @_;

    my $target_lang = $opt{target_lang} || q{};
    my @transformers =
        sort { $b->{priority} <=> $a->{priority} }
        grep { defined $_->{priority} && $_->{priority} >= 0 }
        @{ $TRANSFORMERS_BY_LANG{$target_lang} || [] };

    my %data = (
        target_lang     => $target_lang,
        default_lang    => $opt{default_lang} || q{},
        tag_re          => {},
        transformers    => \@transformers,
    );

    return bless \%data, $class;
}


sub get_value {
    my $self = shift;
    my ($base_key, $tags) = @_;

    my $re = $self->{tag_re}->{$base_key} ||= do {
        my $k = quotemeta $base_key;
        my $div = length $k ? q{:} : q{};
        qr/ ^ $k (?: $div (\w+) )? $ /xms
    };

    my %val = map { m/$re/xms ? (($1 || q{}) => $tags->{$_}) : () } keys %$tags;
    return if !%val;

    my $def_lang = $self->{default_lang};
    $val{$def_lang} //= $val{q{}}  if $def_lang && defined $val{q{}};

    # return value on desired lang if defined
    my $target_lang = $self->{target_lang};
    return $val{$target_lang}  if defined $val{$target_lang};

    # use lang transformer if possible
    for my $tr ( @{ $self->{transformers} } ) {
        my $val = $val{$tr->{from}};
        next if !defined $val;
        my $result = $tr->{transformer}->($val);
        return $result  if defined $result;
    }

    # use english value if exists
    return $val{en}  if $val{en};

    # last chance: return unidecoded default value
    return unidecode( $val{q{}} // first {defined} values %val );
}



sub _set_priority {
    my ($id, $priority) = @_;
    my $data = $TRANSFORMER{$id};
    croak "No transformer id=$id found"  if !$data;

    $data->{priority} = $priority;
    return;
}


sub _dump_transformers {
    say "\nRegistered language transformers:";
    for my $id ( sort keys %TRANSFORMER ) {
        printf "%-16s %3s %3s %5d   %s\n", @{ $TRANSFORMER{$id} }{ qw/ id from to priority plugin / };
    }
    return;
}



sub get_getopt {
    return map { @$_[0,1] } @GETOPT;
}



sub get_usage {
    return map {[ @$_[2,3] ]} grep { $_->[2] } @GETOPT;
}


1;

