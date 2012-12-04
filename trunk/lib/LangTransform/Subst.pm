package LangTransform::Subst;

# $Id$

# ABSTRACT: simple substitute transliterator, just proof-of-concept

use 5.010;
use strict;
use warnings;
use utf8;

use Unicode::Normalize;

our $PRIORITY = 1;

our %DATA = (
    uk_ru => {
        from  => 'uk',
        to    => 'ru',
        table => {
            'ґ' => 'г',
            'е' => 'э',
            'є' => 'е',
            'и' => 'ы',
            'і' => 'и',
            'i' => 'и',
            'ї' => 'йи',
            'щ' => 'шч',
        },
        same_upcase => 1,
    },
    ar_ru => {
        # http://ru.wikipedia.org/wiki/Арабско-русская_практическая_транскрипция
        # http://en.wikipedia.org/wiki/Arabic_(Unicode_block)
        from  => 'ar',
        to    => 'ru',
        table => {
            ( map {( chr(hex $_) => 'а')}  qw/ 0623 FE83 FE84  0622 FE81 FE82  0649 FEEF FEF0 0627 / ), # أ آ ى
            ( map {( chr(hex $_) => '')}   qw/ 0621 / ), # ﺀ 
            ( map {( chr(hex $_) => 'б')}  qw/ 0628 FE8F FE90 FE92 FE91 / ), # ﺏ
            ( map {( chr(hex $_) => 'т')}  qw/ 062A FE95 FE96 FE98 FE97  062A FE95 FE96 FE98 FE97 / ), # ﺕ ﺙ
            ( map {( chr(hex $_) => 'дж')} qw/ 062C FE9D FE9E FEA0 FE9F / ), # ﺝ
            ( map {( chr(hex $_) => 'х')}  qw/ 062D FEA1 FEA2 FEA4 FEA3  062E FEA5 FEA6 FEA8 FEA7 / ), # ﺡ ﺥ
            ( map {( chr(hex $_) => 'д')}  qw/ 062F FEA9 FEAA  0630 FEAB FEAC / ), # ﺩ ﺫ
            ( map {( chr(hex $_) => 'р')}  qw/ 0631 FEAD FEAE / ), # ﺭ
            ( map {( chr(hex $_) => 'з')}  qw/ 0632 FEAF FEB0 / ), # ﺯ
            ( map {( chr(hex $_) => 'с')}  qw/ 0633 FEB1 FEB2 FEB4 FEB3 / ), # ﺱ
            ( map {( chr(hex $_) => 'ш')}  qw/ 0634 FEB5 FEB6 FEB8 FEB7 / ), # ﺵ
            ( map {( chr(hex $_) => 'с')}  qw/ 0635 FEB9 FEBA FEBC FEBB / ), # ﺹ
            ( map {( chr(hex $_) => 'д')}  qw/ 0636 FEBD FEBE FEC0 FEBF / ), # ﺽ
            ( map {( chr(hex $_) => 'т')}  qw/ 0637 FEC1 FEC2 FEC4 FEC3 / ), # ﻁ
            ( map {( chr(hex $_) => 'з')}  qw/ 0638 FEC5 FEC6 FEC8 FEC7 / ), # ﻅ
            ( map {( chr(hex $_) => '')}   qw/ 0639 FEC9 FECA FECC FECB / ), # ﻉ
            ( map {( chr(hex $_) => 'г')}  qw/ 063A FECD FECE FED0 FECF / ), # ﻍ
            ( map {( chr(hex $_) => 'ф')}  qw/ 0641 FED1 FED2 FED4 FED3 / ), # ﻑ
            ( map {( chr(hex $_) => 'к')}  qw/ 0642 FED5 FED6 FED8 FED7  0643 FED9 FEDA FEDC FEDB / ), # ﻕ ﻙ
            ( map {( chr(hex $_) => 'л')}  qw/ 0644 FEDD FEDE FEE0 FEDF / ), # ﻝ
            ( map {( chr(hex $_) => 'м')}  qw/ 0645 FEE1 FEE2 FEE4 FEE3 / ), # ﻡ
            ( map {( chr(hex $_) => 'н')}  qw/ 0646 FEE5 FEE6 FEE8 FEE7 / ), # ﻥ
            ( map {( chr(hex $_) => 'х')}  qw/ 0647 FEE9 FEEA FEEC FEEB / ), # ﻩ
            ( map {( chr(hex $_) => 'у')}  qw/ 0648 FEED FEEE / ), # ﻭ
            ( map {( chr(hex $_) => 'и')}  qw/ 064A FEF1 FEF2 FEF4 FEF3 / ), # ﻱ
        
            'ة' => 'ат',
            'ﻻ' => 'ла',
            'ال' => 'аль-',

            'َ' => 'а',
            'ُ' => 'у',
            'ِ' => 'и',
            'َا' => 'а',
            'ٰ' => 'а',
            'َى' => 'а',
            'ىٰ' => 'а',
            'ُو' => 'у',
            'ِي' => 'и',
            'َو' => 'ав',
            'َي' => 'ай',
            'َوّ' => 'ув',
            'ِيّ' => 'ий',
            'ـً' => 'ан',
            'ًى' => 'ан',
            'ـٌ' => 'ун',
            'ـٍ' => 'ин',
            'ْ' => '',
            'ّ' => '',
            'ٱ' => '',
            'ؐ' => '',
        },
        preprocess => sub { my $t = NFC shift(); $t =~ s/\x{200E}//gxms; return $t },
    },
);


sub init {
    my (undef, %callback) = @_;

    for my $tr ( get_transformers() ) {
        $callback{register_transformer}->($tr);
    }

    return;
}



sub get_transformers {

    my @result;
    while ( my ($id, $data) = each %DATA ) {
        my %table = (
            %{ $data->{table} },
            ( $data->{same_upcase}
                ? ( map {( uc($_) => ucfirst($data->{table}->{$_}) )} keys %{ $data->{table} } )
                : ()
            )
        );
        my $re_text = join q{|}, map {quotemeta $_} sort { length $b <=> length $a } keys %table;
        my $re = qr/($re_text)/xms; # TODO: optimize

        push @result, {
            id => "subst_$id",
            from => $data->{from},
            to => $data->{to},
            priority => $PRIORITY,
            transformer => sub {
                my ($text) = @_;
                $text = $data->{preprocess}->($text)  if $data->{preprocess};
                $text =~ s/$re/$table{$1}/gexms;
                return $text
            },
        };
    }

    return @result;
}


1;

