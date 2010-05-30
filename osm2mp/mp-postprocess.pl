#!/usr/bin/perl

use strict;

my $file = shift @ARGV;
exit unless $file;

rename $file, "$file.old";

open my $in,  '<', "$file.old";
open my $out, '>', $file;

while ( my $line = readline $in ) {
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
    print $out $line;
}

close $in;
close $out;

unlink "$file.old";
