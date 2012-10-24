package LangSelect;

# ABSTRACT: OSM tag language selector and converter

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::Util qw/ first /;
use Text::Unidecode;



our $PLUGIN_BASE = 'LangTransform';

our %TRANSFORMER;
our %TRANSFORMERS_BY_LANG;

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
                for my $tr_data ( $plugin_package->get_transformers() ) {
                    my $tr_id = $tr_data->{id};
                    $TRANSFORMER{$tr_id} = $tr_data  if $tr_id;
                    push @{ $TRANSFORMERS_BY_LANG{ $tr_data->{to} // q{} } }, $tr_data;
                }
                1;
            }
            or warn "Init failed for $plugin_package: $@";
        }
    }
    
    return;
}






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
        qr/ ^ $k (?: : (\w+) )? $ /xms
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

    # last chance: return unidecoded default value
    return unidecode( $val{q{}} // first {defined} values %val );
}



1;

