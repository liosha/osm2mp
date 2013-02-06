#!/usr/bin/perl

use 5.010;
use strict;
use warnings;
use utf8;

my $file = shift @ARGV;
exit unless $file;

rename $file, "$file.old";

open my $in,  '<:encoding(cp1251)', "$file.old";
open my $out, '>:encoding(cp1251)', $file;


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

exit;


BEGIN {
my @short_names = (
    [ 'ул(?:|ица)'              =>  'ул.'    ],
    [ 'пер(?:|еул|еулок)'       =>  'пер.'   ],
    [ 'пр(?:\-к?т|осп|оспект)'  =>  'пр-т'   ],
    [ 'пр(?:\-з?д|оезд)'        =>  'пр-д'   ],
    [ 'п'                       =>  'п.'     ],
    [ 'пр'                      =>  'пр.'    ],
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
    [ 'кв(?:|арт|артал)'        =>  'кв.'  ],
    [ 'м(?:к?рн?|икрорайон)'    =>  'мкр'  ],
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

