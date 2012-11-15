package LangTransform::Equal;

# $Id$

# ABSTRACT: language aliases 

use 5.010;
use strict;
use warnings;
use utf8;

our $PRIORITY = 0;


sub init {
    my (undef, %callback) = @_;

    my $registrator = sub {
        my (undef, $tlang, $langs) = @_;
        for my $slang ( split /[;,]/, $langs ) {
            my $tr = {
                id => "equal_${slang}_$tlang",
                from => $slang,
                to => $tlang,
                priority => $PRIORITY,
                transformer => sub { my ($text) = @_; return $text },
            };
            $callback{register_transformer}->($tr);
        }
        return;
    };

    my @getopt = (
        'lt-equal=s%' => $registrator,
        'lt-equal <lang>=<langs>' => 'set language alias (comma-separated list)',
    );

    $callback{register_getopt}->(\@getopt);

    return;
}



1;

