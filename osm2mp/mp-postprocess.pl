#!/usr/bin/perl

use strict;
use utf8;

my $file = shift @ARGV;
exit unless $file;

rename $file, "$file.old";

open my $in,  '<:encoding(cp1251)', "$file.old";
open my $out, '>:encoding(cp1251)', $file;


my @short = (
    [ 'ул(\.|ица)?'                 =>  'ул.'  ],
    [ 'пер(\.|еул\.|еулок)'         =>  'пер.' ],
    [ 'пр(-к?т|осп\.|оспект)'       =>  'пр-т' ],
    [ 'пр(-з?д|оезд)'               =>  'пр-д' ],
    [ 'пр(\.)'                      =>  'пр.'  ],
    [ 'пл(\.|ощадь)'                =>  'пл.'  ],
    [ 'ш(\.|оссе)'                  =>  'ш.'   ],
    [ 'б(ул\.|ульв\.|-р|ульвар)'    =>  'б-р'  ],
    [ 'наб(\.|ережная)'             =>  'наб.' ],
    [ 'ал(\.|лея)'                  =>  'ал.'  ],
    [ 'мос(т)'                      =>  'мост' ],
);


my $object;

LINE:
while ( my $line = readline $in ) {

    if ( $line =~ /^\[(.*)\]/ ) {
        $object = $1;
    }
    
    #   region name
    if ( $line =~ /^(DefaultRegionCountry|RegionName)/i ) {
        $line =~ s/область/обл./;
        $line =~ s/район/р-н/;
        $line =~ s/Автономный округ - Югра автономный округ/АО (Югра)/;
        $line =~ s/Кабардино-Балкарская республика/Кабардино-Балкария/;
        $line =~ s/Карачаево-Черкесская республика/Карачаево-Черкесия/;
        $line =~ s/автономный округ/АО/i;
        $line =~ s/автономная обл./АО/i;
        $line =~ s/ город//i;
        $line =~ s/(ия|[^я]) республика/$1/i;
        $line =~ s/ - Алания//;
    }

    #   street name
    if ( $object eq 'POLYLINE' && $line =~ /^Label=/i ) {
        SUFF:
        for my $short ( @short ) {
            my ( $s, $r ) = @$short;
            next unless
                my ( undef, $prefix, undef, $postfix )
                    = $line =~ /=((.*\S)\s+)?$s\s*((\s+|\.)\S.*)?$/i;
            $line = "$prefix $postfix";

            $line =~ s/(\d+-?.?[йяе])(\s+(.*))/$2 $1/;
            $line =~ s/(\d+)-?.?([йяе])(\s.*)?$/$1-$2/;

            $line =~ s/\s\s+/ /;
            $line =~ s/^ //;
            $line =~ s/ $//;

            $line = "Label=" . $line . "~[0x1c] $r\n";
            last SUFF;
        }
    }

    print $out $line;
}

close $in;
close $out;

unlink "$file.old";
