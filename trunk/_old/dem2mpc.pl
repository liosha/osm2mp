#!/usr/bin/perl

use Geo::Shapefile::Writer;

my $gmpath = "C:\\Program Files\\GlobalMapper11\\global_mapper11.exe";

my $step0 = 20;
my $step1 = 100;
my $step2 = 500;

my $negative = 1;


print "\n    ---|  dem2mpc  v0.2\n\n";


###     Determining coordinates

my $fi = $ARGV[0];

my ($fn, $ns, $alat, $ew, $alon)  =  $fi =~ /(([NS])(\d+)([EW])(\d+))/i;

die "Please use a valid HGT-like file name!\n"
    unless $fn;

my $flat = (($ns =~ /n/i) ? "0" : "1") . $alat;
my $rlat = ($ns =~ /n/i) ? $alat : -$alat;

my $flon = (($ew =~ /e/i) ? $alon : (360 - $alon));
my $rlon = ($ew =~ /e/i) ? $alon : -$alon;


my $mode;

if ( -f $fi  &&  $fi =~ /^AST.+zip/i ) {
    $mode = 'aster';
    my $tif = $fi;
    $tif =~ s/\.zip/_dem.tif/;
    `7z x $fi $tif`;
    $fi = $tif;
}
elsif ( -f $fi ) {
    $mode = 'srtm';
}
else {
    $mode = 'cgiar';
    $fi = sprintf "srtm_%02d_%02d.zip", (1+(180+$rlon)/5), (1+(60-$rlat-1)/5);
    die "File $fi not found"
        unless  -f $fi;
}



print "Converting $fi  lat=$rlat  lon=$rlon\n";
print "Generating contours...\n";


###     Creating GM script

my ($mlat,$mlon) = ($rlat+1, $rlon+1);
my $gms = "
GLOBAL_MAPPER_SCRIPT VERSION=1.00 ENABLE_PROGRESS=YES

UNLOAD_ALL

IMPORT FILENAME=\"$fi\" ANTI_ALIAS=YES 
EXPORT_ELEVATION FILENAME=\"$fn.bil\" TYPE=BIL FILL_GAPS=YES BYTES_PER_SAMPLE=2 LAT_LON_BOUNDS=$rlon,$rlat,$mlon,$mlat
GENERATE_CONTOURS INTERVAL=$step0 ELEV_UNITS=METERS SIMPLIFICATION=0 LAT_LON_BOUNDS=$rlon,$rlat,$mlon,$mlat
EXPORT_VECTOR FILENAME=\"$fn.mp\" TYPE=POLISH_MP MAP_NAME=$fn MP_IMAGE_ID=89$flat$flon
";

open GMS, '>', "$fn.gms";
print GMS $gms;
close GMS;


###     Running Global Mapper

`start /wait \"$gmpath\" $fn.gms 1> con`;
unlink "$fn.gms";
unlink $fi      if  $mode eq 'aster';


print "Processing temporary MP file...\n";

open MPX, "<", "$fn.mp";

$shp = Geo::Shapefile::Writer->new( $fn, 'polyline', 
        { name=>'GRMN_TYPE',     type=>'C', length=>20, decimals=>0 },
        { name=>'NAME',          type=>'C', length=>80, decimals=>0 },
        );

while (<MPX>) {
    if ( /^Label=(\-?\d+)/ ) { 

        $labelf = $1;
        $labelm = int ($1 * 0.3048 + (($1<0) ? -0.5 : +0.5)); 

        if    ( ($labelm > 0) && ($labelm % $step2 == 0) ) { $grmn_type = 'MAJOR_CONTOUR'; }
        elsif ( ($labelm > 0) && ($labelm % $step1 == 0) ) { $grmn_type = 'INT_CONTOUR'; }
        else                                               { $grmn_type = 'MINOR_CONTOUR'; }

    }

    if ( /^Data\d=\((.+)\)\s*$/ && ( $negative || ($label > 0) ) ) {
        $shp->add_shape([[map {[reverse split /,\s*/, $_ ]} split /\),\s*\(/, $1 ]], $grmn_type, $labelf);
    }
}

$shp->finalize();
close MPX;

`del $fn.mp`;

print "Finished $fi !\n\n";
