#!/usr/bin/perl

use 5.010;
use strict;
use warnings;
use utf8;


use Encode;
use Encode::Locale;

use Getopt::Long;



Encode::Locale::decode_argv();
GetOptions(
    'e|encoding=s' => \my $encoding,
);


while ( my $file = encode locale_fs => shift @ARGV ) {
    if ( $file =~ /\.dbf$/ix ) {
        process_dbf( $file, encoding => $encoding);
    }
    else {
        process_mp( $file, encoding => $encoding || 'cp1251' );
    }
}


exit;



sub process_dbf {
    my ($file, %opt) = @_;
    my $encoding = $opt{encoding} || 'utf8';

    require XBase;
    my $dbf = XBase->new($file)  or die XBase->errstr();

    for my $n ( 0 .. $dbf->last_record() ) {
        my $rec = $dbf->get_record_as_hash($n);
        my %update;

        for my $field ( qw/ NAME STATE L_STATE R_STATE / ) {
            next if !$rec->{$field};
            my $val = decode $encoding => ($update{$field} || $rec->{$field});
            $update{$field} = encode $encoding => _clear_region($val);
        }
        
        for my $field ( qw/ NAME / ) {
            next if !$rec->{$field};
            my $val = decode $encoding => ($update{$field} || $rec->{$field});
            $update{$field} = encode $encoding => _clear_street($val);
        }

        next if !%update;
        $dbf->update_record_hash($n, %update);
    }

    $dbf->close();

    return;
}

sub process_mp {
    my ($file, %opt) = @_;
    my $encoding = $opt{encoding} || 'utf8';

    rename $file, "$file.old";

    open my $in,  "<:encoding($encoding)", "$file.old";
    open my $out, ">:encoding($encoding)", $file;

    my $object;

    LINE:
    while ( my $line = readline $in ) {

        if ( $line =~ /^\[(.*)\]/ ) {
            $object = $1;
        }

        my ($tag, $val) = $line =~ / ^ \s* (\w+) \s* = \s* (.*\S) \s* $ /xms;

        if ( $tag ~~ [ qw/ Label StreetDesc CityName RegionName / ] ) {
            $val = _clear_bad_symbols($val);
        }

        #   region name
        if ( $tag ~~ [ qw/DefaultRegionCountry RegionName/ ] ) {
            $val = _clear_region($val);
        }

        #   street names
        if ( ($object eq 'POLYLINE' && $tag ~~ 'Label') || $tag ~~ 'StreetDesc' ) {
            $val = _clear_street($val);
        }

        $line = "$tag=$val\n"  if $tag;
        print {$out} $line;
    }

    close $in;
    close $out;

    unlink "$file.old";

    return;
}


BEGIN {
my @short_names = (
    # russian
    [ 'ул(?:|ица)'              =>  'ул.'    ],
    [ 'пер(?:|еул|еулок)'       =>  'пер.'   ],
    [ 'пр(?:\-к?т|осп|оспект)'  =>  'пр-т'   ],
    [ 'пр(?:\-з?д|оезд)'        =>  'пр-д'   ],
    [ 'п'                       =>  'п.'     ],
    [ 'пл(?:|ощадь)'            =>  'пл.'    ],
    [ 'ш(?:|оссе)'              =>  'ш.'     ],
    [ 'туп(?:|ик)'              =>  'туп.'   ],
    [ 'б(?:ул|ульв|-р|ульвар)'  =>  'б-р'    ],
    [ 'наб(?:|ережная)'         =>  'наб.'   ],
    [ 'ал(?:|лея)'              =>  'ал.'    ],
    [ 'мост'                    =>  'мост'   ],
    [ 'тракт'                   =>  'тракт'  ],
    [ 'просек'                  =>  'просек' ],
    [ 'линия'                   =>  'линия'  ],
    [ 'кв(?:|арт|артал)'        =>  'кв.'    ],
    [ 'м(?:к?рн?|икрорайон)'    =>  'мкр'    ],

    # ukrainian
    [ 'вул(?:|иця)'             =>  'вул.'   ],
    [ 'пр(?:|овулок)'           =>  'пр.'    ],
    [ 'шосе'                    =>  'ш.'     ],
    [ 'проїзд'                  =>  'пр-д'   ],
    [ 'площа'                   =>  'пл.'    ],
    [ 'мікрорайон'              =>  'мкр'    ],
    [ 'набережна'               =>  'наб.'   ],
    [ 'алея'                    =>  'ал.'    ],
);

sub _clear_street {
    my ($in) = @_;

    my $line = join q{ }, map { ucfirst } grep { $_ } split / /, $in;

    SUFF:
    for my $short ( @short_names ) {
        my ( $s, $r ) = @$short;
        next SUFF unless
            my ( $prefix, $postfix )
                = $line =~ / ^ (?: (.*\S)? \s+ )? $s (?: (?: \s+ | \s*\.\s* ) (.*) )? $ /ix;

        next SUFF unless $prefix || $postfix;

        $line = join q{ }, grep {$_} ($prefix, $postfix);

        $line =~ s/(\d+-?.?[йяе])(\s+(.*))/$2 $1/;
        $line =~ s/(\d+)-?.?([йяе])(\s.*)?$/$1-$2/;

        $line =~ s/\s\s+/ /;
        $line =~ s/^ //;
        $line =~ s/ $//;
        return "$line $r";
    }

    return $in;
}
}





sub _clear_region {
    my ($line) = @_;

    $line =~ s/область/обл./;
    $line =~ s/район/р-н/;
    $line =~ s/Автономный округ - Югра автономный округ/АО (Югра)/;
    $line =~ s/Кабардино-Балкарская республика/Кабардино-Балкария/;
    $line =~ s/Карачаево-Черкесская республика/Карачаево-Черкесия/;
    $line =~ s/автономный округ/АО/i;
    $line =~ s/автономная обл./АО/i;
    $line =~ s/городской округ/ГО/ig;
    $line =~ s/ город//i;
    $line =~ s/([ие]я|[^я]) республика/$1/i;
    $line =~ s/ - Алания//;

    return $line;
}


sub _clear_bad_symbols {
    my ($line) = @_;

    $line =~ s/[«»"]//g;
    $line =~ s/&#xD;//g;

    return $line;
}

