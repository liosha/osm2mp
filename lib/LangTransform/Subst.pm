package LangTransform::Subst;

# $Id$

# ABSTRACT: substitute transliterator

use 5.010;
use strict;
use warnings;
use utf8;

use Unicode::Normalize;

use Utils;

our $PRIORITY = 1;

our %DATA = (
    ru_uk => {
        from  => 'ru',
        to    => 'uk',
        table => {
            'е' => 'є',
            '\bё' => 'йо',
            '(?<=[аяэеоёуюыи])ё' => 'йо',
            '(?<=[ьъ])ё' => 'о',
            'ё' => 'ьо',
            '(?<=[ьъаяэеоёуюыи])и' => 'ї',
            'и' => 'і',
            'ъ' => q{'},
            'ы' => 'и',
            'э' => 'е',
            'ния\b' => 'ння',
            'ский\b' => 'ський',
            'ская\b' => 'ська',
            'ское\b' => 'ське',
            'ские\b' => 'ські',
            'ная\b' => 'на',
            'ное\b' => 'не',
            'ные\b' => 'ні',
        },
        same_upcase => 1,
    },
    ru_be => {
        from  => 'ru',
        to    => 'be',
        table => {
            'и' => 'і',
            'ъ' => q{'},
            'щ' => 'ш',
            '(?<=[жшчц])и' => 'ы',
            '(?<=[аяэеоёуюыи])в(?=\b|[^аяэеоёуюыи])' => 'ў',
            '(?<=\s)в(?=\s)' => 'ў',
        },
        same_upcase => 1,
    },
    uk_ru => {
        from  => 'uk',
        to    => 'ru',
        table => {
            'ґ' => 'г',
            'е' => 'э',
            'є' => 'е',
            'и' => 'ы',
            'і' => 'и',
            'ї' => 'йи',
            'щ' => 'шч',
            'ьо' => 'ё',
            'ння\b' => 'ния',
        },
        same_upcase => 1,
    },
    # http://ru.wikipedia.org/wiki/%C1%EE%EB%E3%E0%F0%F1%EA%E8%E9_%E0%EB%F4%E0%E2%E8%F2#.D0.9F.D1.80.D0.B0.D0.BA.D1.82.D0.B8.D1.87.D0.B5.D1.81.D0.BA.D0.B0.D1.8F_.D1.82.D1.80.D0.B0.D0.BD.D1.81.D0.BA.D1.80.D0.B8.D0.BF.D1.86.D0.B8.D1.8F_.D0.BD.D0.B0_.D1.80.D1.83.D1.81.D1.81.D0.BA.D0.B8.D0.B9_.D1.8F.D0.B7.D1.8B.D0.BA
    bg_ru => {
        from  => 'bg',
        to    => 'ru',
        table => {
            'щ' => 'шч',
            'ъ' => 'ы',
            '\bъ' => 'и',
            '(?<=[жшц])ъ' => 'е',
            '(?<=б)ъ(?=лгар)' => 'о',
            '(?<=п)ъ(?=рв)' => 'е',
            'ьо' => 'ё',
            'йо' => 'ё',
            '\bйо' => 'йо',
        },
        same_upcase => 1,
    },

# http://ru.wikipedia.org/wiki/%D7%E5%F8%F1%EA%EE-%F0%F3%F1%F1%EA%E0%FF_%EF%F0%E0%EA%F2%E8%F7%E5%F1%EA%E0%FF_%F2%F0%E0%ED%F1%EA%F0%E8%EF%F6%E8%FF
    cs_ru => {
        from  => 'cs',
        to    => 'ru',
        table => {
            '[aá]' => 'а',
            '(?<=[ďňť])[aá]' => 'я',
            'b' => 'б',
            'c' => 'ц',
            'č' => 'ч',
            '[dď]' => 'д',
            'ď(?![aáeéěiíoóuúůyý])' => 'дь',
            '[eéě]' => 'е',
            '(?:\b|(?<=[aáeéěiíoóuúůyý]))[eé]' => 'э',
            'f' => 'ф',
            'g' => 'г',
            'h' => 'г',
            'ch' => 'х',
            '[ií]' => 'и',
            '(?<=\B[šč])[ií]\b' => 'ий',
            'j' => 'й',
            '(?:\b|(?<=[aáeéěiíoóuúůyý]))ja' => 'я',
            'ja' => 'ья',
            '(?:\b|(?<=[aáeéěiíoóuúůyý]))je' => 'е',
            'je' => 'ье',
            '\bjo' => 'йо',
            '(?<=[aáeéěiíoóuúůyý])jo' => 'ё',
            'jo' => 'ьё',
            '(?:\b|(?<=[aáeéěiíoóuúůyý]))j[uů]' => 'ю',
            'j[uů]' => 'ью',
            '[kq]' => 'к',
            'l(?=[aáeéěiíoóuúůyý])' => 'л',
            'l' => 'ль',
            'm' => 'м',
            '[nň]' => 'н',
            'ň(?![aáeéěiíoóuúůyý])' => 'нь',
            '(?<=[ďňť])[oó]' => 'ё',
            '[oó]' => 'о',
            'p' => 'п',
            'r' => 'р',
            'ř' => 'рж',
            '(?<=[cčfhkpsštťx])ř' => 'рш',
            's' => 'с',
            'š' => 'ш',
            '[tť]' => 'т',
            'ť(?![aáeéěiíoóuúůyý])' => 'ть',
            '[uúů]' => 'у',
            '(?<=[ďňť])[uúů]' => 'ю',
            '[vw]' => 'в',
            'x' => 'кс',
            '[yý]' => 'ы',
            '(?<=[ghk])[yý]' => 'и',
            '(?<=\B[hk])ý\b' => 'ий',
            '(?<=\B[nl])ý\b' => 'ый',
            'z' => 'з',
            'ž' => 'ж',
        },
        same_upcase => 1,
        preprocess => sub { NFC shift() },
    },

    ar_ru => {
        # http://ru.wikipedia.org/wiki/Арабско-русская_практическая_транскрипция
        # http://en.wikipedia.org/wiki/Arabic_(Unicode_block)
        from  => 'ar',
        to    => 'ru',
        table => {
            ( map {( chr(hex $_) => '')}   qw/ 0621 / ), # ﺀ 
            ( map {( chr(hex $_) => 'а')}  qw/ 0622 FE81 FE82 / ), # ﺁ
            ( map {( chr(hex $_) => 'а')}  qw/ 0623 FE83 FE84 / ), # ﺃ
            ( map {( chr(hex $_) => 'у')}  qw/ 0624 / ), # ؤ
            ( map {( chr(hex $_) => 'а')}  qw/ 0625 / ), # إ
            ( map {( chr(hex $_) => 'й')}  qw/ 0626 / ), # ئ
            ( map {( chr(hex $_) => 'а')}  qw/ 0627 / ), # ا
            ( map {( chr(hex $_) => 'б')}  qw/ 0628 FE8F FE90 FE92 FE91 / ), # ﺏ
            ( map {( chr(hex $_) => 'ат')} qw/ 0629 / ), # ة
            ( map {( chr(hex $_) => 'т')}  qw/ 062A FE95 FE96 FE98 FE97 / ), # ﺕ 
            ( map {( chr(hex $_) => 'т')}  qw/ 062B FE99 FE9A FE9C FE9B / ), # ﺙ
            ( map {( chr(hex $_) => 'дж')} qw/ 062C FE9D FE9E FEA0 FE9F / ), # ﺝ
            ( map {( chr(hex $_) => 'х')}  qw/ 062D FEA1 FEA2 FEA4 FEA3 / ), # ﺡ
            ( map {( chr(hex $_) => 'х')}  qw/ 062E FEA5 FEA6 FEA8 FEA7 / ), # ﺥ
            ( map {( chr(hex $_) => 'д')}  qw/ 062F FEA9 FEAA / ), # ﺩ
            ( map {( chr(hex $_) => 'д')}  qw/ 0630 FEAB FEAC / ), # ﺫ
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
            ( map {( chr(hex $_) => 'к')}  qw/ 0642 FED5 FED6 FED8 FED7 / ), # ﻕ 
            ( map {( chr(hex $_) => 'к')}  qw/ 0643 FED9 FEDA FEDC FEDB / ), # ﻙ
            ( map {( chr(hex $_) => 'л')}  qw/ 0644 FEDD FEDE FEE0 FEDF / ), # ﻝ
            ( map {( chr(hex $_) => 'м')}  qw/ 0645 FEE1 FEE2 FEE4 FEE3 / ), # ﻡ
            ( map {( chr(hex $_) => 'н')}  qw/ 0646 FEE5 FEE6 FEE8 FEE7 / ), # ﻥ
            ( map {( chr(hex $_) => 'х')}  qw/ 0647 FEE9 FEEA FEEC FEEB / ), # ﻩ
            ( map {( chr(hex $_) => 'у')}  qw/ 0648 FEED FEEE / ), # ﻭ
            ( map {( chr(hex $_) => 'а')}  qw/ 0649 FEEF FEF0 / ), # ى
            ( map {( chr(hex $_) => 'и')}  qw/ 064A FEF1 FEF2 FEF4 FEF3 / ), # ﻱ
        
            'ﻻ' => 'ла',
            '\bال' => 'аль-',

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

            "\x{064B}" => 'ан',
            "\x{064B}\x{0649}" => 'ан',
            "\x{064C}" => 'ун',
            "\x{064D}" => 'ин',
            
            'ْ' => '',
            'ّ' => '',
            'ٱ' => '',
            'ؐ' => '',

            # punctuation
            "\x{060c}" => ',',
            "\x{061f}" => '?',
            ( map {( chr(hex $_) => '')} qw/ 0640 / ),
            # digits
            ( map {( chr(0x660+$_) => $_ )} ( 0 .. 9 ) ),
        },
        preprocess => sub { my $t = NFC shift(); $t =~ s/\x{200E}//gxms; return $t },
    },
    ( map {("${_}_en_iso" => _get_iso_tr($_))} qw/ ru be uk bg mk / ),
);


BEGIN {
my %ISO9B = (
    'А' => 'a',
    'Б' => 'b',
    'В' => 'v',
    'Г' => { ru => 'g', be => 'h', uk => 'h', bg => 'g', mk => 'g' },
    'Ѓ' => { mk => 'g`' },
    'Ґ' => { uk => 'g`' },
    'Д' => 'd',
    'Е' => 'e',
    'Ё' => { ru => 'yo', be => 'yo' },
    'Є' => { uk => 'ye' },
    'Ж' => 'zh',
    'З' => 'z',
    'S' => { mk => 'z`' },
    'И' => { ru => 'i', uk => 'y`', bg => 'i', mk => 'i' },
    'Й' => { ru => 'j', be => 'j', uk => 'j', bg => 'j' },
    'J' => { mk => 'j' },   
    'I' => {
        ru => { 'I(?=[^аеёиоуыэюяIѢѴ])' => 'i`', 'I' => 'i' },
        be => 'i',
        uk => 'i',
        bg => { 'I(?=[^аеёиоуюяIЪѢѴѪ])' => 'i`', 'I' => 'i' } },
    'Ї' => { uk => 'yi' },
    'К' => 'k',
    'Ќ' => { mk => 'k`' },
    'Л' => 'l',
    'Љ' => { mk => 'l`' },
    'М' => 'm',
    'Н' => 'n',
    'Њ' => { mk => 'n`' },
    'О' => 'o',
    'П' => 'p',
    'Р' => 'r',
    'С' => 's',
    'Т' => 't',
    'У' => 'u',
    'Ў' => { be => 'u`' },              
    'Ф' => 'f',
    'Х' => 'x',
    'Ц' => { map {($_ => { 'Ц(?=[ЕЁЄИЙJIЇЫЭЮЯѢѴ])' => 'c', 'Ц' => 'cz' })} qw/ ru be uk bg mk / },
    'Ч' => 'ch',
    'Џ' => { mk => 'dh' },
    'Ш' => 'sh',
    'Щ' => { ru => 'shh', uk => 'shh', bg => 'sth' },
    'Ъ' => { ru => '``', bg => 'a`' },
    'Ы' => { ru => 'y`', be => 'y`' },              
    'Ь' => { map {($_ => '`')} qw/ ru be uk bg / },
    'Э' => { ru => 'e`', be => 'e`' },              
    'Ю' => { map {($_ => 'yu')} qw/ ru be uk bg / },
    'Я' => { map {($_ => 'ya')} qw/ ru be uk bg / },
    '’' => q{'},
    'Ѣ' => { ru => 'ye', bg => 'ye' },
    'Ѳ' => { ru => 'fh', bg => 'fh' },
    'Ѵ' => { ru => 'yh', bg => 'yh' },
    'Ѫ' => { bg => 'о`' },
    '№' => '#',
);

sub _get_iso_tr {
    my ($lang) = @_;

    my %table;
    while ( my ($key, $rule) = each %ISO9B ) {
        if ( !ref $rule ) {
            $table{$key} = $rule;
            next;
        }

        $rule = $rule->{$lang};
        next if !$rule;

        if ( !ref $rule ) {
            $table{$key} = $rule;
        }
        else {
            %table = (%table, %$rule);
        }
    }

    return {
        from  => $lang,
        to    => 'en',
        same_upcase => 1,
        table => \%table,
    };
}
}


# http://ru.wikipedia.org/wiki/ISO_9


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

        my @rule_keys = keys %{ $data->{table} };
        my $re = Utils::make_re_from_list( \@rule_keys, capture => 1, get_rule_num => 1, i => $data->{same_upcase} );

        my $apply_rule = sub {
            my ($in, $rule) = @_;
            my $text = $data->{table}->{$rule // $in};
            return $text  if !$data->{same_upcase};

            my $upper_first = $in =~ / ^ \p{Uppercase_Letter} /xms;
            return $text  if !$upper_first;

            my $upper_last  = $in =~ / \p{Uppercase_Letter} $ /xms;
            return $upper_last ? uc $text : ucfirst $text;
        };

        push @result, {
            id => "subst_$id",
            from => $data->{from},
            to => $data->{to},
            priority => $PRIORITY,
            transformer => sub {
                my ($text) = @_;
                $text = $data->{preprocess}->($text)  if $data->{preprocess};
                $text =~ s# $re # $apply_rule->($1, $rule_keys[$^R]) #gexms;
                return $text;
            },
        };
    }

    return @result;
}


1;

