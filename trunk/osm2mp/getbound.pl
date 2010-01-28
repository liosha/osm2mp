#!/usr/bin/perl -w

use strict;

my $bdir = '_bounds/';
my $api  = 'http://www.openstreetmap.org/api/0.6';
my $nul  = 'nul';


my %rename = (
    # gis-lab
    adygeya         =>  253256,
    altay           =>  145194,
    altayskiy       =>  144764,
    amur            =>  147166,
    arkhan          =>  140337,
    astrakhan       =>  112819,
    bashkir         =>  77677,
    belgorod        =>  83184,
    bryansk         =>  81997,
    buryat          =>  145729,
    chechen         =>  109877,
    chel            =>  77687,
    chukot          =>  151231,
    chuvash         =>  80513,
    dagestan        =>  109876,
    evrey           =>  147167,
    ingush          =>  253252,
    irkutsk         =>  145454,
    ivanov          =>  85617,
    kabardin        =>  109879,
    kalinin         =>  103906,
    kalmyk          =>  108083,
    kaluzh          =>  81995,
    kamch           =>  151233,
    karach          =>  109878,
    karel           =>  393980,
    kemerovo        =>  144763,
    khabar          =>  151223,
    khakas          =>  190911,
    khanty          =>  140296,
    kirov           =>  115100,
    komi            =>  115136,
    kostrom         =>  85963,
    krasnodar       =>  108082,
    krasnoyarsk     =>  190090,
    kurgan          =>  140290,
    kursk           =>  72223,
    leningrad       =>  176095,
    lipetsk         =>  72169,
    magadan         =>  151228,
    mariyel         =>  115114,
    mordov          =>  72196,
    mosobl          =>  51490,
    murmansk        =>  289998,
    nenec           =>  274048,
    nizhegorod      =>  72195,
    novgorod        =>  89331,
    novosib         =>  140294,
    omsk            =>  140292,
    orenburg        =>  77669,
    orlovsk         =>  72224,
    osetiya         =>  110032,
    penz            =>  72182,
    perm            =>  115135,
    prim            =>  151225,
    pskov           =>  155262,
    rostov          =>  85606,
    ryazan          =>  71950,
    sakhalin        =>  394235,
    samar           =>  72194,
    saratov         =>  72193,
    smol            =>  81996,
    stavrop         =>  108081,
    sverdl          =>  79379,
    tambov          =>  72180,
    tatar           =>  79374,
    tomsk           =>  140295,
    tul             =>  81993,
    tumen           =>  140291,
    tver            =>  178005,
    tyva            =>  145195,
    udmurt          =>  115134,
    ulyan           =>  72192,
    vladimir        =>  72197,
    volgograd       =>  77665,
    vologda         =>  115106,
    voronezh        =>  72181,
    yakut           =>  151234,
    yamal           =>  191706,
    yarosl          =>  81994,
    zabaikal        =>  145730,
);




die     unless @ARGV;

my $name =  $ARGV[0];
my $rel  =  exists $rename{$name}  ?  $rename{$name}  :  $name;

`curl $api/relation/$rel/full -o $rel.osm`;
`boundaries.pl -in=$rel.osm -poly -csv=$nul -html=$nul -polybase=reg`;
unlink "$rel.osm";
if ( -f "reg.$rel.poly" ) {
    unlink "$bdir$name.poly"     if -f "$bdir$name.poly";
    rename "reg.$rel.poly", "$bdir$name.poly";
}
