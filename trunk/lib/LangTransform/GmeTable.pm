package LangTransform::GmeTable;

# $Id$

# ABSTRACT: GpsMapEdit transliteration tables

use 5.010;
use strict;
use warnings;
use utf8;

use Carp;
use Encode;

our $PRIORITY = 2;


sub init {
    my (undef, %callback) = @_;

    my $registrator = sub {
        my (undef, $data) = @_;

        my ($from, $to, $file) = $data =~ / (\w+) - (\w+) : (.+) /xms;
        croak "Invalid parameter: $data"  if !$file;

        my $tr = {
            id => "gme_${from}_$to",
            from => $from,
            to => $to,
            priority => $PRIORITY,
            transformer => _make_gme_transformer($file),
        };
        
        $callback{register_transformer}->($tr);
        return;
    };

    my @getopt = (
        'lt-gme=s' => $registrator,
        'lt-gme <data>' => 'GME table, data is <from_lang>-<to_lang>:<file>',
    );

    $callback{register_getopt}->(\@getopt);
    return;
}


sub _make_gme_transformer {
    my ($file) = @_;

    my $encoding = 'utf8';
    my %table;

    open my $in, '<', $file
        or croak "File not found: $file";

    while ( defined( my $line = readline $in ) ) {
        $line =~ s/ ^ \xEF \xBB \xBF //xms;
        $line =~ s/ \s* $ //xms;

        if ( my ($cp_code) = $line =~ / ^ \.CODEPAGE \s+ (\d+) /xms ) {
            $encoding = "cp$cp_code";
        }

        next if $line =~ / ^ (?: \s | ; | \# | \. | $ ) /xms;

        my ($from, $to) = split "\t", decode( $encoding, $line );
        $table{$from} = $to;
    }
    close $in;

    my $re_text;
    eval {
        require Regexp::Assemble;
        $re_text = Regexp::Assemble->new()->add( map { quotemeta $_ } keys %table )->re();
    }
    or eval {
        $re_text = join q{|}, map { quotemeta $_ } sort { length $b <=> length $a } keys %table;
        require Regexp::Optimizer;
        $re_text = Regexp::Optimizer->new()->optimize($re_text);
    }; 
    
    my $re = qr/($re_text)/;

    my $tranformer = sub {
        my ($str) = @_;
        $str =~ s/$re/$table{$1}/gxms;
        return $str;
    };

    return $tranformer;
}


1;

