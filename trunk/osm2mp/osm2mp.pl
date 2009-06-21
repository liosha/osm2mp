#!/usr/bin/perl


##
##  Required packages: 
##    * Template-toolkit
##    * Getopt::Long
##    * Text::Unidecode
##    * Math::Polygon
##    * Math::Geometry::Planar
##  See http://cpan.org/ or use PPM (Perl package manager)
##


use Math::Polygon;
use Math::Geometry::Planar;


####    Settings

my $version = "0.72a";

my $cfgpoi      = "poi.cfg";
my $cfgpoly     = "poly.cfg";
my $cfgheader   = "header.tpl";

my $mapid       = "88888888";
my $mapname     = "OSM routable";

my $codepage    = "1251";
my $nocodepage;

my $defaultcountry = "Earth";
my $defaultregion  = "OSM";
my $defaultcity    = "";

my $mergeroads     = 1;
my $mergecos       = 0.2;

my $detectdupes    = 1;

my $splitroads     = 1;

my $fixclosenodes  = 1;
my $fixclosedist   = 3.0;       # set 5.5 for cgpsmapper 0097 and earlier

my $restrictions   = 1;

my $nametaglist    = "name,ref,int_ref,addr:housenumber";
my $upcase         = 0;
my $translit       = 0;

my $bbox;
my $background     = 0;
my $osmbbox        = 0;

my $disableuturns  = 0;

my $shorelines     = 0;
my $navitel        = 0;
my $makepoi        = 1;


# FIXME make command-line parameters?
my @housenamelist       = ("addr:housenumber", "addr:housename");
my @citynamelist        = ("place_name", "name");
my @regionnamelist      = ("addr:region", "is_in:region", "addr:state", "is_in:state");
my @countrynamelist     = ("addr:country", "is_in:country_code", "is_in:country");


my %yesno = (  "yes"            => 1,
               "true"           => 1,
               "1"              => 1,
               "permissive"     => 1,
               "no"             => 0,
               "false"          => 0,
               "0"              => 0,
               "private"        => 0);

use Getopt::Long;
$result = GetOptions (
                        "cfgpoi=s"              => \$cfgpoi,
                        "cfgpoly=s"             => \$cfgpoly,
                        "header=s"              => \$cfgheader,
                        "mapid=s"               => \$mapid,
                        "mapname=s"             => \$mapname,
                        "codepage=s"            => \$codepage,
                        "nocodepage"            => \$nocodepage,
                        "mergeroads!"           => \$mergeroads,
                        "mergecos=f"            => \$mergecos,
                        "detectdupes!"          => \$detectdupes,
                        "splitroads!"           => \$splitroads,
                        "fixclosenodes!"        => \$fixclosenodes,
                        "fixclosedist=f"        => \$fixclosedist,
                        "restrictions!"         => \$restrictions,
                        "defaultcountry=s"      => \$defaultcountry,
                        "defaultregion=s"       => \$defaultregion,
                        "defaultcity=s"         => \$defaultcity,
                        "nametaglist=s"         => \$nametaglist,
                        "upcase!"               => \$upcase,
                        "translit!"             => \$translit,
                        "bbox=s"                => \$bbox,
                        "osmbbox!"              => \$osmbbox,
                        "background!",          => \$background,
                        "disableuturns!",       => \$disableuturns,
                        "shorelines!",          => \$shorelines,
                        "navitel!",             => \$navitel,
                        "makepoi!",             => \$makepoi,
                      );

undef $codepage         if ($nocodepage);



####    Action

use strict;
use Template;

print STDERR "\n  ---|   OSM -> MP converter  $version   (c) 2008,2009  liosha, xliosha\@gmail.com\n\n";


if ($ARGV[0] eq "") {

    my @onoff = ( "off", "on");

    print "Usage:  osm2mp.pl [options] file.osm > file.mp

Possible options [defaults]:

    --mapid <id>              map id            [$mapid]
    --mapname <name>          map name          [$mapname]

    --cfgpoi <file>           poi config        [$cfgpoi]
    --cfgpoly <file>          way config        [$cfgpoly]
    --header <file>           header template   [$cfgheader]

    --bbox <bbox>             comma-separated minlon,minlat,maxlon,maxlat
    --osmbbox                 use bounds from .osm              [$onoff[$osmbbox]]
    --background              create background object          [$onoff[$background]]

    --codepage <num>          codepage number                   [$codepage]
    --nocodepage              leave all labels in utf-8         [$onoff[$nocodepage]]
    --upcase                  convert all labels to upper case  [$onoff[$upcase]]
    --translit                tranliterate labels               [$onoff[$translit]]

    --nametaglist <list>      comma-separated list of tags for Label    [$nametaglist]
    --defaultcountry <name>   default data for street indexing  [$defaultcountry]
    --defaultregion <name>                                      [$defaultregion]
    --defaultcity <name>                                        [$defaultcity]
    --navitel                 write addresses for polygons              [$onoff[$navitel]]

    --mergeroads              merge same ways                           [$onoff[$mergeroads]]
    --mergecos <cosine>       maximum allowed angle between roads to merge      [$mergecos]
    --splitroads              split long and self-intersecting roads    [$onoff[$splitroads]]
    --fixclosenodes           enlarge distance between too close nodes  [$onoff[$fixclosenodes]]
    --fixclosedist <dist>     minimum allowed distance                  [$fixclosedist m]
    --detectdupes             detect road duplicates                    [$onoff[$detectdupes]]

    --restrictions            process turn restrictions                 [$onoff[$restrictions]]
    --disableuturns           disable u-turns on nodes with 2 links     [$onoff[$disableuturns]]

    --shorelines              process shorelines                        [$onoff[$shorelines]]
    --makepoi                 create POIs for polygons                  [$onoff[$makepoi]]


You can use no<option> disable features (i.e --nomergeroads)
";
    exit;
}


####    Reading configs

my %poitype;

open CFG, $cfgpoi;
while (<CFG>) {
   if ( (!$_) || /^\s*[\#\;]/ ) { next; }
   chomp;
   my ($k, $v, $type, $llev, $hlev, $city) = split /\s+/;
   if ($type) {
     $llev = 0          if ($llev eq "");
     $hlev = 1          if ($hlev eq "");
     $city = (($city ne "") ? 1 : 0);
     $poitype{"$k=$v"} = [ $type, $llev, $hlev, $city ];
   }
}
close CFG;


my %polytype;

open CFG, $cfgpoly;
while (<CFG>) {
   if ( (!$_) || /^\s*[\#\;]/ ) { next; }
   chomp;

   my $prio = 0;
   my ($k, $v, $mode, $type, $llev, $hlev, $rp, @p) = split /\s+/;

   if ($type) {
     if ($type =~ /(.+),(\d)/) {     $type = $1;    $prio = $2;    }
     $llev = 0          if ($llev eq "");
     $hlev = 1          if ($hlev eq "");

     $polytype{"$k=$v"} = [ $mode, $type, $prio, $llev, $hlev, $rp ];
   }
}
close CFG;


my @nametagarray = split (/,/, $nametaglist);



####    Header

my $tmp = Template->new({ABSOLUTE => 1});
$tmp->process ($cfgheader, {
        mapid           => $mapid,
        mapname         => $mapname,
        codepage        => $codepage,
        defaultcountry  => $defaultcountry,
        defaultregion   => $defaultregion,
      }) || die $tmp->error();


####    Info


use POSIX qw(strftime);
print "\n; Converted from OpenStreetMap data with  osm2mp $version  (" . strftime ("%Y-%m-%d %H:%M:%S", localtime) . ")\n\n";


open IN, $ARGV[0];
print STDERR "Processing file $ARGV[0]\n\n";



####    Bounds

my $bounds;
my $boundpoly = Math::Geometry::Planar->new;

$bounds = 1 if $bbox;

my ($minlon, $minlat, $maxlon, $maxlat) = split /,/, $bbox;
$boundpoly->points ([[$minlon,$minlat],[$maxlon,$minlat],[$maxlon,$maxlat],[$minlon,$maxlat]]);




####    1st pass 
###     loading nodes

my %nodes;
print STDERR "Loading nodes...          ";

while (<IN>) {

    if ( /\<node.* id=["'](\-?\d+)["'].*lat=["'](\-?\d+\.?\d*)["'].*lon=["'](\-?\d+\.?\d*)["'].*$/ ) {
        $nodes{$1} = "$2,$3";
        next;
    }

    if ( $osmbbox && /\<bounds/ ) {
        ($minlat, $minlon, $maxlat, $maxlon) = ( /minlat=["'](\-?\d+\.?\d*)["'] minlon=["'](\-?\d+\.?\d*)["'] maxlat=["'](\-?\d+\.?\d*)["'] maxlon=["'](\-?\d+\.?\d*)["']/ );
        $bbox = join ",", ($minlon, $minlat, $maxlon, $maxlat);
        $bounds = 1 if $bbox;
        $boundpoly->points ([[$minlon,$minlat],[$maxlon,$minlat],[$maxlon,$maxlat],[$minlon,$maxlat]]);
    }

    last if /\<way/;
}
printf STDERR "%d loaded\n", scalar keys %nodes;



###     saving file positions

my $waypos  = tell IN;
my $waystr  = $_;

while (<IN>) {     last if /\<relation/;     }

my $relpos  = tell IN;
my $relstr  = $_;



###     loading relations

my %mpoly;
my %mpholes;

my %trest;
my %nodetr;


print STDERR "Loading relations...      ";

my $id;
my $reltype;

my $mp_outer;
my @mp_inner;

my ($tr_from, $tr_via, $tr_to, $tr_type);

while ($_) {

    if ( /\<relation/ ) {
        /^.* id=["'](\-?\d+)["'].*$/;

        $id = $1;
        undef $reltype;
        undef $mp_outer;        undef @mp_inner;
        undef $tr_from;         undef $tr_via;
        undef $tr_to;           undef $tr_type;
        next;
    }

    if ( /\<member/ ) {
        /type=["'](\w+)["'].*ref=["'](\-?\d+)["'].*role=["'](\w+)["']/;

        $mp_outer = $2                  if ($3 eq "outer" && $1 eq "way");
        push @mp_inner, $2              if ($3 eq "inner" && $1 eq "way");

        $tr_from = $2                   if ($3 eq "from"  && $1 eq "way");
        $tr_to = $2                     if ($3 eq "to"    && $1 eq "way");
        $tr_via = $2                    if ($3 eq "via"   && $1 eq "node");

        next;
    }

    if ( /\<tag/ ) {
        /k=["'](\w+)["'].*v=["'](\w+)["']/;
        $reltype = $2                           if ( $1 eq "type" );
        $tr_type = $2                           if ( $1 eq "restriction" );
        next;
    }

    if ( /\<\/relation/ ) {
        if ( $reltype eq "multipolygon" ) {
            $mpoly{$mp_outer} = [ @mp_inner ];
            @mpholes{@mp_inner} = @mp_inner;
        }
        if ( $restrictions && $reltype eq "restriction" ) {
            $tr_to = $tr_from                   if ($tr_type eq "no_u_turn" && !$tr_to);

            if ( $tr_from && $tr_via && $tr_to ) {
                $trest{$id} = { node => $tr_via,
                                type => ($tr_type =~ /^only_/) ? "only" : "no",
                                fr_way => $tr_from,   fr_dir => 0,   fr_pos => -1,
                                to_way => $tr_to,     to_dir => 0,   to_pos => -1 };
                push @{$nodetr{$tr_via}}, $id;
            } else {
                print "; ERROR: Wrong restriction RelID=$id\n";
            }
        }
        next;
    }

} continue { $_ = <IN>; }

printf STDERR "%d multipolygons, %d turn restrictions\n", scalar keys %mpoly, scalar keys %trest;




####    2nd pass
###     loading cities, multipolygon holes and checking node dupes

my %cityname;
my %citybound;

print STDERR "Loading cities...         ";

seek IN, $waypos, 0;
$_ = $waystr;

my $id;
my %waytag;
my @chain;
my $dupcount;

while ($_) {

   if ( /\<way.* id=["'](\-?\d+)["'].*$/ ) {
      $id = $1;
      @chain = ();
      %waytag = ();
      $dupcount = 0;
      next;
   }

   if ( /\<nd.*ref=["'](.*)["'].*$/ ) {
      if ($nodes{$1}  &&  $1 ne $chain[-1] ) {
          push @chain, $1;
      } elsif ($nodes{$1}) {
          print "; ERROR: WayID=$id has dupes at ($nodes{$1})\n";
          $dupcount ++;
      }
      next;
   }

   if ( /\<tag.*k=["'](.*)["'].*v=["'](.*)["'].*$/ ) {
       $waytag{$1} = $2;
       next;
   }

   if ( /\<\/way/ ) {

       ##       this way is multipolygon inner
       if ( $mpholes{$id} ) {
           $mpholes{$id} = [ @chain ];
       }
       ##       this way is city bounds
       if ($waytag{"place"} eq "city" || $waytag{"place"} eq "town") { 
           my $name = convert_string ( (grep {defined} @waytag{@citynamelist})[0] );
           if ($name && $chain[0] eq $chain[-1]) {
               print "; Found city: WayID=$id $name\n";
               my $region = convert_string ( (grep {defined} @waytag{@regionnamelist})[0] );
               my $country = convert_string ( (grep {defined} @waytag{@countrynamelist})[0] );
               $cityname{$id} = [ $name, $region, $country ];
               $citybound{$id} = Math::Polygon->new( map { [split ",",$nodes{$_}] } @chain );
           } else {
               print "; ERROR: City without name WayID=$id\n"           unless ($name);
               print "; ERROR: City polygon WayID=$id is not closed\n"  if ($chain[0] ne $chain[-1]);
           }
       }
       next;
   }

   last if /\<relation/;
} continue { $_ = <IN>; }

printf STDERR "%d loaded\n", scalar keys %cityname;





####    3rd pass
###     writing POIs

seek IN, 0, 0;

print STDERR "Writing POIs...           ";
print "\n\n\n; ### Points\n\n";

my $countpoi = 0;
my $id;
my %nodetag;

while (<IN>) {

   if ( /\<node.* id=["'](\-?\d+)["']/ ) {
       $id = $1;
       %nodetag = ();
       next;
   }

   if ( /\<tag.* k=["'](.*)["'].* v=["'](.*)["'].*$/ ) {
       $nodetag{$1} = $2;
       next;
   }

   if ( /\<\/node/ && (!$bounds || insidebounds($nodes{$id})) ) {

       my @typelist = grep {$poitype{"$_=$nodetag{$_}"}} keys %nodetag;
       next unless $typelist[0];

       $countpoi ++;
       my $poi = "$typelist[0]=$nodetag{$typelist[0]}";
       my $poiname = convert_string ((grep {defined} @nodetag{@nametagarray})[0]);
       my @type = @{$poitype{$poi}};

       print  "; NodeID = $id\n";
       print  "; $poi\n";
       print  "[POI]\n";
       print  "Type=$type[0]\n";
       printf "Data%d=($nodes{$id})\n",   $type[1];
       printf "EndLevel=%d\n",            $type[2]      if ($type[2] > $type[1]);
       printf "City=Y\n",                               if ($type[3]);
       print  "Label=$poiname\n"                        if ($poiname);

       my $housenumber = convert_string ( (grep {defined} @nodetag{@housenamelist})[0] );
       print  "HouseNumber=$housenumber\n"                                              if $housenumber;
       printf "StreetDesc=%s\n",        convert_string($nodetag{"addr:street"})         if $nodetag{"addr:street"};

       for my $i (keys %cityname) {
           if ( $citybound{$i}->contains([split ",",$nodes{$id}]) ) {
               print "CityName=".$cityname{$i}->[0]."\n";
               print "RegionName=".$cityname{$i}->[1]."\n"          if $cityname{$i}->[1];
               print "CountryName=".$cityname{$i}->[2]."\n"         if $cityname{$i}->[2];
               last;
           } elsif ( $defaultcity ) {
               print "CityName=$defaultcity\n";
           }
       }

       printf "Zip=%s\n",               convert_string($nodetag{"addr:postcode"})       if $nodetag{"addr:postcode"};
       printf "Phone=%s\n",             convert_string($nodetag{"phone"})               if $nodetag{"phone"};

       print  "[END]\n\n";
   }

   last if /\<way/;
}

printf STDERR "%d written\n", $countpoi;





####    Loading roads and coastlines, and writing other ways

my %rchain;
my %rprops;
my %risin;

my %xnodes;
my %schain;

print STDERR "Processing ways...        ";
print "\n\n\n; ### Lines and polygons\n\n";

my $countlines    = 0;
my $countpolygons = 0;

my $id;
my @chain;
my @chainlist;
my $inbbox;

my $isin;

while ($_) {

   last if /\<relation/;

   if ( /\<way/ ) {
      /^.* id=["'](\-?\d+)["'].*$/;

      $id = $1;

      %waytag = ();
      @chain = ();
      @chainlist = ();
      $inbbox = 0;
      $isin = 0;

      next;
   }

   if ( /\<nd/ ) {
      /^.*ref=["'](.*)["'].*$/;
      if ($nodes{$1}  &&  $1 ne $chain[-1] ) {
          push @chain, $1;
          if ($bounds) {
              if ( !$inbbox &&  insidebbox($nodes{$1}) )        { push @chainlist, ($#chain ? $#chain-1 : 0); }
              if (  $inbbox && !insidebbox($nodes{$1}) )        { push @chainlist, $#chain; }
              $inbbox = insidebbox($nodes{$1});
          }
      }
      next;
   }

   if ( /\<tag/ ) {
       /^.*k=["'](.*)["'].*v=["'](.*)["'].*$/;
       $waytag{$1} = $2;
       next;
   }

   if ( /\<\/way/ ) {

       my @typelist = sort {@polytype{$b}->[2] cmp @polytype{$a}->[2]} grep {@polytype{$_}} map {"$_=$waytag{$_}"} keys %waytag;
       my $poly = $typelist[0];

       my @namelist = grep {defined} @waytag{@nametagarray};
       my $polyname = convert_string ($namelist[0]);

       if ( !$bounds )                  {   @chainlist = (0);   }
       if ( !($#chainlist % 2) )        {   push @chainlist, $#chain;   }

       ##       this way is road
       if ( $polytype{$poly}->[0] eq "r"  &&  scalar @chain <= 1 ) {
           print "; ERROR: Road WayID=$id has too few nodes at ($nodes{$chain[0]})\n";
       }
       if ( $polytype{$poly}->[0] eq "r"  &&  scalar @chain > 1 ) {
           my @rp = split /,/, $polytype{$poly}->[5];
           if ($waytag{"maxspeed"}>0) {
               if ($waytag{"maxspeed"} =~ /mph$/i)   {  $waytag{"maxspeed"} *= 1.61;  }
               $rp[0]  = speedcode($waytag{"maxspeed"});
           }

           $rp[2]  = $yesno{$waytag{"oneway"}}                  if exists $waytag{"oneway"};
           $rp[3]  = $yesno{$waytag{"toll"}}                    if exists $waytag{"toll"};

           $rp[5]=$rp[6]=$rp[7]=$rp[8]=$rp[9]=$rp[10]=$rp[11] = 1-$yesno{$waytag{"access"}}
                                                                if exists $waytag{"access"};
           $rp[5]=$rp[6]=$rp[7]=$rp[8]=$rp[10] = 1-$yesno{$waytag{"auto"}}
                                                                if exists $waytag{"auto"};
           $rp[5]=$rp[6]=$rp[7]=$rp[8]=$rp[10] = 1-$yesno{$waytag{"vehicle"}}
                                                                if exists $waytag{"vehicle"};
           $rp[9]=$rp[10] = $yesno{$waytag{"motorroad"}}        if exists $waytag{"motorroad"};
           
           $rp[5]=$rp[6]=$rp[8] = 1-$yesno{$waytag{"motorcar"}} if exists $waytag{"motorcar"};
           $rp[9]  = 1-$yesno{$waytag{"foot"}}                  if exists $waytag{"foot"};
           $rp[10] = 1-$yesno{$waytag{"bicycle"}}               if exists $waytag{"bicycle"};
           $rp[7]  = 1-$yesno{$waytag{"psv"}}                   if exists $waytag{"psv"};
           $rp[11] = 1-$yesno{$waytag{"hgv"}}                   if exists $waytag{"hgv"};


           for my $i (keys %cityname) {
               if ( $citybound{$i}->contains([split ",",$nodes{$chain[0]}]) 
                 && $citybound{$i}->contains([split ",",$nodes{$chain[-1]}]) ) {
                   $isin = $i;
                   last;
               }
           }

           for (my $i=0; $i<$#chainlist+1; $i+=2) {
               $rchain{"$id:$i"} = [ @chain[$chainlist[$i]..$chainlist[$i+1]] ];
               $rprops{"$id:$i"} = [ $poly, $polyname, join (",",@rp) ];
               $risin{"$id:$i"}  = $isin         if ($isin && $polyname);

               if ($bounds) {
                   if ( !insidebbox($nodes{$chain[$chainlist[$i]]}) ) {
                       $xnodes{ $chain[$chainlist[$i]] }        = 1;
                       $xnodes{ $chain[$chainlist[$i]+1] }      = 1;
                   }
                   if ( !insidebbox($nodes{$chain[$chainlist[$i+1]]}) ) {
                       $xnodes{ $chain[$chainlist[$i+1]] }      = 1;
                       $xnodes{ $chain[$chainlist[$i+1]-1] }    = 1;
                   }
               }
           }

           # processing associated turn restrictions

           if ($restrictions) {
               if ( $chainlist[0] == 0 ) {
                   for my $relid (@{$nodetr{$chain[0]}}) {
                       if ( $trest{$relid}->{fr_way} eq $id ) {
                           $trest{$relid}->{fr_way} = "$id:0";
                           $trest{$relid}->{fr_dir} = -1;
                           $trest{$relid}->{fr_pos} = 0;
                       }
                       if ( $trest{$relid}->{to_way} eq $id ) {
                           $trest{$relid}->{to_way} = "$id:0";
                           $trest{$relid}->{to_dir} = 1;
                           $trest{$relid}->{to_pos} = 0;
                       }
                   }
               }
               if ( $chainlist[-1] == $#chain ) {
                   for my $relid (@{$nodetr{$chain[-1]}}) {
                       if ( $trest{$relid}->{fr_way} eq $id ) {
                           $trest{$relid}->{fr_way} = "$id:" . (($#chainlist-1)>>1);
                           $trest{$relid}->{fr_dir} = 1;
                           $trest{$relid}->{fr_pos} = $chainlist[-1] - $chainlist[-2];
                       }
                       if ( $trest{$relid}->{to_way} eq $id ) {
                           $trest{$relid}->{to_way} = "$id:" . (($#chainlist-1)>>1);
                           $trest{$relid}->{to_dir} = -1;
                           $trest{$relid}->{to_pos} = $chainlist[-1] - $chainlist[-2];
                       }
                   }
               }
           }
       }


       ##       this way is map line

#       if ( $polytype{$poly}->[0] eq "l" || ($polytype{$poly}->[0] eq "s" && !$shorelines) ) {
       if ( $polytype{$poly}->[0] eq "l" || $polytype{$poly}->[0] eq "s" ) {
           my $d = "";
           if ( scalar @chain < 2 ) {
               print "; ERROR: WayID=$id has too few nodes at ($nodes{$chain[0]})\n";
               $d = "; ";
           }

           my @type = @{$polytype{$poly}};

           for (my $i=0; $i<$#chainlist+1; $i+=2) {
               $countlines ++;

               print  "; WayID = $id\n";
               print  "; $poly\n";
               print  "${d}[POLYLINE]\n";
               printf "${d}Type=%s\n",        $type[1];
               printf "${d}EndLevel=%d\n",    $type[4]              if ($type[4] > $type[3]);
               print  "${d}Label=$polyname\n"                       if ($polyname);
               # print  "${d}DirIndicator=$polydir\n"                 if defined $polydir;
               printf "${d}Data%d=(%s)\n",    $type[3], join ("), (", @nodes{@chain[$chainlist[$i]..$chainlist[$i+1]]});
               print  "${d}[END]\n\n\n";
           }
       }


       ##       this way is coastline

       if ( $polytype{$poly}->[0] eq "s" && $shorelines ) {
           if ( scalar @chain < 2 ) {
               print "; ERROR: WayID=$id has too few nodes at ($nodes{$chain[0]})\n";
           } else {
               for (my $i=0; $i<$#chainlist+1; $i+=2) {
                   $schain{$chain[$chainlist[$i]]} = [ @chain[$chainlist[$i]..$chainlist[$i+1]] ];
               }
           }
       }


       ##       this way is map polygon

       if ( $polytype{$poly}->[0] eq "p" ) {
           my $d = "";
           if ( scalar @chain < 4 ) {
               print "; ERROR: WayID=$id has too few nodes near ($nodes{$chain[0]})\n";
               $d = "; ";
           }
           if ( $chain[0] ne $chain[-1] ) {
               print "; ERROR: area WayID=$id is not closed at ($nodes{$chain[0]})\n";
           }

           my @type = @{$polytype{$poly}};

           print  "; WayID = $id\n";
           print  "; $poly\n";

           if (!$d && (!$bounds || scalar @chainlist)) {

               my $polygon = Math::Polygon->new( map { [split ",",$nodes{$_}] } @chain );
               $polygon = $polygon->fillClip1 ($minlat, $minlon, $maxlat, $maxlon) if ($bounds);
               
               if ($polygon) {
                   $countpolygons ++;
           
                   print  "[POLYGON]\n";
                   printf "Type=%s\n",        $type[1];
                   printf "EndLevel=%d\n",    $type[4]              if ($type[4] > $type[3]);
                   print  "Label=$polyname\n"                       if ($polyname);

                   my $city;
                   if ($navitel || ($makepoi && $polyname && $type[5])) {
                       for my $i (keys %cityname) {
                           if ( $citybound{$i}->contains([split ",",$nodes{$chain[0]}]) ) {
                               $city = $i;
                               last;
                           }
                       }
                   }

                   ## Navitel
                   if ($navitel) {
                       my $housenumber = convert_string ( (grep {defined} @waytag{@housenamelist})[0] );
                       print  "HouseNumber=$housenumber\n"                              if $housenumber;
                       printf "StreetDesc=%s\n", convert_string($waytag{"addr:street"}) if $waytag{"addr:street"};
                       if ( $waytag{"addr:housenumber"} &&  $waytag{"addr:street"} ) {
                           if ( $city ) {
                               print "CityName=".$cityname{$city}->[0]."\n";
                               print "RegionName=".$cityname{$city}->[1]."\n"           if $cityname{$city}->[1];
                               print "CountryName=".$cityname{$city}->[2]."\n"          if $cityname{$city}->[2];
                           } elsif ( $defaultcity ) {
                               print "CityName=$defaultcity\n";
                           }
                       }
                   }

                   # printf "${d}Data%d=(%s)\n",    $type[3], join ("), (", @nodes{@chain});
                   printf "Data%d=(%s)\n",    $type[3], join ("), (", map {join(",", @{$_})} @{$polygon->points});
                   if ($mpoly{$id}) {
                       printf "; this is multipolygon with %d holes: %s\n", scalar @{$mpoly{$id}}, join (", ", @{$mpoly{$id}});
                       for my $hole (@{$mpoly{$id}}) {
                           if ($mpholes{$hole} ne $hole && @{$mpholes{$hole}}) {
                               printf "Data%d=(%s)\n",    $type[3], join ("), (", @nodes{@{$mpholes{$hole}}});
                           }
                       }
                   }
                   print  "[END]\n\n\n";

                   if ($makepoi && $polyname && $type[5]) {

                       my ($poi,$pll,$phl) = split ",", $type[5];

                       print  "[POI]\n";
                       print  "Type=$poi\n";
                       print  "EndLevel=$phl\n";
                       print  "Label=$polyname\n";
                       printf "Data%d=(%f,%f)\n", $pll, centroid(($polygon->points));

                       my $housenumber = convert_string ( (grep {defined} @waytag{@housenamelist})[0] );
                       print  "HouseNumber=$housenumber\n"                              if $housenumber;
                       printf "StreetDesc=%s\n", convert_string($waytag{"addr:street"}) if $waytag{"addr:street"};
                       if ( $city ) {
                           print "CityName=".$cityname{$city}->[0]."\n";
                           print "RegionName=".$cityname{$city}->[1]."\n"           if $cityname{$city}->[1];
                           print "CountryName=".$cityname{$city}->[2]."\n"          if $cityname{$city}->[2];
                       } elsif ( $defaultcity ) {
                           print "CityName=$defaultcity\n";
                       }

                       print  "[END]\n\n\n";
                   }
               }
           }
       }
   }

} continue { $_ = <IN>; }

printf STDERR "%d roads and %d coastlines loaded
                          $countlines lines and $countpolygons polygons dumped\n", scalar keys %rchain, scalar keys %schain;





####    Processing shorelines

if ($shorelines) {
    print "\n\n\n";
    print STDERR "Processing shorelines...  ";


    ##  merging
    my @keys = keys %schain;
    my $i = 0;
    while ($i < scalar @keys) {
        while ( $schain{$keys[$i]}  &&  $schain{$schain{$keys[$i]}->[-1]}  &&  $schain{$keys[$i]}->[-1] ne $keys[$i] ) {
            my $mnode = $schain{$keys[$i]}->[-1];
            pop  @{$schain{$keys[$i]}};
            push @{$schain{$keys[$i]}}, @{$schain{$mnode}};
            delete $schain{$mnode};
        }
        $i++;
    }


    ##  tracing bounds
    if ($bounds) {

        my @bound = @{$boundpoly->points};
        push @bound, $bound[0];

        my @tbound;
        my $pos = 0;
        for (my $i=0; $i<$#bound; $i++) {
            push @tbound, {type=>"bound", point=>$bound[$i], pos=>$pos};
            for my $sline (keys %schain) {
                my $p1 = [ reverse split /,/, $nodes{$schain{$sline}->[0]} ];
                my $p2 = [ reverse split /,/, $nodes{$schain{$sline}->[1]} ];
                my $ipoint = SegmentIntersection [$bound[$i],$bound[$i+1],$p1,$p2];
                $ipoint = $p2           if (!$ipoint && DistanceToSegment([$bound[$i],$bound[$i+1],$p2])==0 && !insidebounds($nodes{$schain{$sline}->[0]}));
                $ipoint = $p1           if (!$ipoint && DistanceToSegment([$bound[$i],$bound[$i+1],$p1])==0);
                if ($ipoint) {  unless ( grep { $_->{type} eq "end" && $_->{point}->[0]==$ipoint->[0] && $_->{point}->[1]==$ipoint->[1] } @tbound ) {
                    push @tbound, {type=>"start", point=>$ipoint, pos=>$pos+SegmentLength([$bound[$i],$ipoint]), line=>$sline };
                } else { 
                    @tbound = grep { !($_->{type} eq "end" && $_->{point}->[0]==$ipoint->[0] && $_->{point}->[1]==$ipoint->[1]) } @tbound;
                }}

                my $p1 = [ reverse split /,/, $nodes{$schain{$sline}->[-1]} ];
                my $p2 = [ reverse split /,/, $nodes{$schain{$sline}->[-2]} ];
                my $ipoint = SegmentIntersection [$bound[$i],$bound[$i+1],$p1,$p2];
                $ipoint = $p2           if (!$ipoint && DistanceToSegment([$bound[$i],$bound[$i+1],$p2])==0 && !insidebounds($nodes{$schain{$sline}->[-1]}));
                $ipoint = $p1           if (!$ipoint && DistanceToSegment([$bound[$i],$bound[$i+1],$p1])==0);
                if ($ipoint) {  unless ( grep { $_->{type} eq "start" && $_->{point}->[0]==$ipoint->[0] && $_->{point}->[1]==$ipoint->[1] } @tbound ) {
                    push @tbound, {type=>"end", point=>$ipoint, pos=>$pos+SegmentLength([$bound[$i],$ipoint]), line=>$sline };
                } else { 
                    @tbound = grep { !($_->{type} eq "start" && $_->{point}->[0]==$ipoint->[0] && $_->{point}->[1]==$ipoint->[1]) } @tbound;
                }}
            }
            $pos += SegmentLength [$bound[$i],$bound[$i+1]];
        }

#        use Data::Dump;
#        dd (sort {$a->{pos}<=>$b->{pos}} @tbound);
#        exit;

        my $tmp = (sort {$a->{pos}<=>$b->{pos}} grep {$_->{type} ne "bound"} @tbound)[0];
        if ( $tmp->{type} eq "end" ) {
            map {$_->{pos} += $pos} grep {$_->{pos} <= $tmp->{pos}} @tbound;
        }

        my $tmp = 0;
        for my $node (sort {$a->{pos}<=>$b->{pos}} @tbound) {
            my $ll = join ",", reverse @{$node->{point}};
            $nodes{$ll} = $ll;

            if ($node->{type} eq "start") {
                $tmp = $node;
                $schain{$tmp->{line}}->[0] = $ll;
            } 
            if ($node->{type} eq "bound" && $tmp) {
                unshift @{$schain{$tmp->{line}}}, ($ll);
            } 
            if ($node->{type} eq "end" && $tmp) {
                $schain{$node->{line}}->[-1] = $ll;
                if ($node->{line} eq $tmp->{line}) {
                    push @{$schain{$node->{line}}}, $schain{$node->{line}}->[0];
                } else {
                    push @{$schain{$node->{line}}}, @{$schain{$tmp->{line}}};
                    map { $_->{line} = $node->{line} } grep { $_->{line} eq $tmp->{line} } @tbound;
                }
                $tmp = 0;
            }
        }
    }



    ##  detecting lakes and islands
    my %loops;
    my @islands;

    for my $i (keys %schain) {


        if ($schain{$i}->[0] eq $schain{$i}->[-1]) {

            # FIXME   filtering huge polygons
            next if scalar @{$schain{$i}} > 30000;

            $loops{$i} = Math::Polygon->new( map {  [split ",",$nodes{$_}] } @{$schain{$i}}  );
            if ($loops{$i}->isClockwise) {
#                print "; island\n";
                push @islands, $i;
                delete $loops{$i};
            } else {
#                print "; lake\n";
            }
        }

#        print  "; merged coastline\n";
#        print  "[POLYLINE]\n";
#        print  "Type=0x15\n";
#        print  "EndLevel=4\n";
#        printf "Data0=(%s)\n",          join ("), (", @nodes{@{$schain{$i}}});
#        print  "[END]\n\n\n";
    }

    my $countislands = 0;
    for my $i (keys %loops) {
        print  "; lake\n";
        print  "[POLYGON]\n";
        print  "Type=0x3c\n";
        print  "EndLevel=4\n";
        printf "Data0=(%s)\n",          join ("), (", @nodes{@{$schain{$i}}});
        for my $j (@islands) {
            if ($loops{$i}->contains( [split ",", $nodes{$j}] )) {
                $countislands++;
                printf "Data0=(%s)\n",          join ("), (", @nodes{@{$schain{$j}}});
                # delete $loops{$i};
            }
        }
        print  "[END]\n\n\n";

    }

    printf STDERR "%d lakes, %d islands\n", scalar keys %loops, $countislands;
}




####    Detecting end nodes

my %enodes;
my %rstart;

while (my ($road, $pchain) = each %rchain) {
    $enodes{$pchain->[0]}  ++;
    $enodes{$pchain->[-1]} ++;
    push @{$rstart{$pchain->[0]}}, $road;
}





####    Merging roads

my %rmove;

if ($mergeroads) {
    print "\n\n\n";
    print STDERR "Merging roads...          ";

    my $countmerg = 0;
    my @keys = keys %rchain;

    my $i = 0;
    while ($i < scalar @keys) {

        my $r1 = $keys[$i];
        if ($rmove{$r1}) {      $i++;   next;   }
        my $p1 = $rchain{$r1};

        my @list = ();
        for my $r2 (@{$rstart{$p1->[-1]}}) {
            if ( $r1 ne $r2  &&  $rprops{$r2}
              && join(":",@{$rprops{$r1}})  eq  join(":",@{$rprops{$r2}})
              && $risin{$r1} eq $risin{$r2}
              && lcos($p1->[-2],$p1->[-1],$rchain{$r2}->[1]) > $mergecos ) {
                push @list, $r2
            }
        }

        if (scalar @list) {
            $countmerg ++;
            @list = sort { lcos($p1->[-2],$p1->[-1],$rchain{$b}->[1]) <=> lcos($p1->[-2],$p1->[-1],$rchain{$a}->[1]) }  @list;
            printf "; FIX: Road WayID=$r1 may be merged with %s at (%s)\n", join (", ", @list), $nodes{$p1->[-1]};

            my $r2 = $list[0];
            $rmove{$r2} = $r1;

            if ($restrictions) {
                while ( my ($relid, $tr) = each %trest )  {
                    if ( $tr->{fr_way} eq $r2 )  {
                        print "; FIX: RelID=$relid FROM moved from WayID=$r2 to WayID=$r1\n";
                        $tr->{fr_way} = $r1;
                        $tr->{fr_pos} += ( (scalar @{$rchain{$r1}}) - 1 );
                    }
                    if ( $tr->{to_way} eq $r2 )  {
                        print "; FIX: RelID=$relid FROM moved from WayID=$r2 to WayID=$r1\n";
                        $tr->{to_way} = $r1;
                        $tr->{to_pos} += ( (scalar @{$rchain{$r1}}) - 1 );
                    }
                }
            }

            $enodes{$rchain{$r2}->[0]} -= 2;
            push @{$rchain{$r1}}, @{$rchain{$r2}}[1..$#{$rchain{$r2}}];
            delete $rchain{$r2};
            delete $rprops{$r2};
            delete $risin{$r2};
            @{$rstart{$p1->[-1]}} = grep { $_ ne $r2 } @{$rstart{$p1->[-1]}};
        } else {
            $i ++;
        }
    }

    print STDERR "$countmerg merged\n";
}





####    Generating routing graph

my %rnodes;
my %nodid;
my %nodeways;

print STDERR "Detecting road nodes...   ";

while (my ($road, $pchain) = each %rchain) {
    for my $node (@{$pchain}) {
        $rnodes{$node} ++;
        push @{$nodeways{$node}}, $road         if ($nodetr{$node} || ($disableuturns && $enodes{$node}==2));
    }
}

my $nodcount = 1;
my $utcount  = 0;
for my $node (keys %rnodes) {
    if ( $rnodes{$node}>1 || $enodes{$node}>0 || $xnodes{$node} || (defined($nodetr{$node}) && scalar @{$nodetr{$node}}) ) {
#        printf "; NodID=$nodcount - NodeID=$node at (%s) - $rnodes{$node} roads, $enodes{$node} ends, X=$xnodes{$node}, %d trs\n", $nodes{$node}, (defined($nodetr{$node}) && scalar @{$nodetr{$node}});
        $nodid{$node} = $nodcount++;
    }
    if ($disableuturns && $rnodes{$node}==2 && $enodes{$node}==2) {
        if ( $rprops{$nodeways{$node}->[0]}[2] =~ /^.,.,0/ ) {
            my $dir = indexof ($rchain{$nodeways{$node}->[0]}, $node);
            $trest{"ut".$utcount++} = { node => $node,  type => "no",
                        fr_way => $nodeways{$node}->[0],
                        fr_dir => ($dir>0) ? 1 : -1,
                        fr_pos => $dir,
                        to_way => $nodeways{$node}->[0],
                        to_dir => ($dir>0) ? -1 : 1,
                        to_pos => $dir };
        }
        if ( $rprops{$nodeways{$node}->[1]}[2] =~ /^.,.,0/ ) {
            my $dir = indexof ($rchain{$nodeways{$node}->[1]}, $node);
            $trest{"ut".$utcount++} = { node => $node,  type => "no",
                        fr_way => $nodeways{$node}->[1],
                        fr_dir => ($dir>0) ? 1 : -1,
                        fr_pos => $dir,
                        to_way => $nodeways{$node}->[1],
                        to_dir => ($dir>0) ? -1 : 1,
                        to_pos => $dir };
        }
    }
}

printf STDERR "%d found\n", scalar keys %nodid;





####    Detecting duplicate road segments


if ($detectdupes) {

    my %segways;

    print STDERR "Detecting duplicates...   ";
    print "\n\n\n; ### Duplicate roads\n\n";

    while (my ($road, $pchain) = each %rchain) {
        for (my $i=0; $i<$#{$pchain}; $i++) {
            if ( $nodid{$pchain->[$i]} && $nodid{$pchain->[$i+1]} )   {
                push @{$segways{join(":",( sort {$a cmp $b} ($pchain->[$i],$pchain->[$i+1]) ))}}, $road;
            }
        }
    }

    my $countdupsegs  = 0;
    my $countduproads = 0;

    my %roadsegs;
    my %roadpos;

    for my $seg (keys %segways) {
        if ( $#{$segways{$seg}} > 0 ) {
            $countdupsegs ++;
            my $roads = join ", ", ( sort {$a cmp $b} @{$segways{$seg}} );
            $roadsegs{$roads} ++;
            my ($point) = split (":", $seg);
            $roadpos{$roads} = $nodes{$point};
        }
    }

    for my $road (keys %roadsegs) {
        $countduproads ++;
        printf "; ERROR: Roads $road has $roadsegs{$road} duplicate segments near ($roadpos{$road})\n";
    }

    printf STDERR "$countdupsegs segments, $countduproads roads\n";
}




####    Fixing self-intersections and long roads

if ($splitroads) {
    my $countself = 0;
    my $countlong = 0;
    print "\n\n\n";

    print STDERR "Splitting roads...        ";
    while (my ($road, $pchain) = each %rchain) {
        my $j = 0;
        my @breaks = ();
        my $break = 0;
        my $rnod = 1;
        for (my $i=1; $i < scalar @{$pchain}; $i++) {
            $rnod ++  if ( ${nodid{$pchain->[$i]}} );
            if (scalar (grep { $_ eq $pchain->[$i] } @{$pchain}[$break..$i-1]) > 0) {
                $countself ++;
#                print "; ERROR: WayID=$road has self-intersecton near (${nodes{$pchain->[$i]}})  ($i $j)\n";
                if ($pchain->[$i] ne $pchain->[$j]) {
                    $break = $j;
                    push @breaks, $break;
                } else {
                    $break = ($i + $j) >> 1;
                    push @breaks, $break;
                    $nodid{$pchain->[$break]} = $nodcount++;
                    printf "; FIX: Added NodID=%d for NodeID=%s at (%s)\n", $nodid{$pchain->[$break]}, $pchain->[$break], $nodes{$pchain->[$break]};
                }
                $rnod = 1;
            }
            if ($rnod == 60) {
                $countlong ++;
#                print "; ERROR: WayID=$road has too many nodes  ($i $j)\n";
                $break = $j;
                push @breaks, $break;
                $rnod = 1;
            }
            $j = $i             if ($nodid{$pchain->[$i]});
        }
        if (scalar @breaks > 0) {
            printf "; FIX: WayID=$road is splitted at %s\n", join (", ", @breaks);
            push @breaks, $#{$pchain};
            for (my $i=0; $i<$#breaks; $i++) {
                my $id = $road."/".($i+1);
                printf "; FIX: Added road %s, nodes from %d to %d\n", $id, $breaks[$i], $breaks[$i+1];
                $rchain{$id} = [ @{$pchain}[$breaks[$i] .. $breaks[$i+1]] ];
                $rprops{$id} = $rprops{$road};
                $risin{$id}  = $risin{$road};

                if ($restrictions) {
                    while ( my ($relid, $tr) = each %trest )  {
                        if ( $tr->{to_way} eq $road ) {
                            #print "; Rel=$relid   ". join (" : ", %{$tr}) ."\n";
                            if ( $tr->{to_pos} > $breaks[$i]-(1+$tr->{to_dir})/2 && $tr->{to_pos} <= $breaks[$i+1]-(1+$tr->{to_dir})/2 ) {
                                $tr->{to_way} = $id;
                                $tr->{to_pos} -= $breaks[$i];
                                print "; FIX: Turn restriction RelID=$relid moved to WayID=$id\n";
                                #print "; now Rel=$relid   ". join (" : ", %{$tr}) ."\n";
                            }
                        }
                        if ( $tr->{fr_way} eq $road ) {
                            #print "; Rel=$relid   ". join (" : ", %{$tr}) ."\n";
                            if ( $tr->{fr_pos} > $breaks[$i]+($tr->{fr_dir}-1)/2 && $tr->{fr_pos} <= $breaks[$i+1]+($tr->{fr_dir}-1)/2 ) {
                                $tr->{fr_way} = $id;
                                $tr->{fr_pos} -= $breaks[$i];
                                print "; FIX: Turn restriction RelID=$relid moved to WayID=$id\n";
                                #print "; now Rel=$relid   ". join (" : ", %{$tr}) ."\n";
                            }
                        }
                    }
                }
            }
            $#{$pchain} = $breaks[0];
        }
    }
    print STDERR "$countself self-intersections, $countlong long roads\n";
}





####    Fixing "too close nodes" error

if ($fixclosenodes) {
    my $countclose = 0;

    print "\n\n\n";
    print STDERR "Fixing close nodes...     ";
    while (my ($road, $pchain) = each %rchain) {
        my $cnode = $pchain->[0];
        for my $node (@{$pchain}[1..$#{$pchain}]) {
            if ($node ne $cnode && $nodid{$node}) {
                if (closenodes($cnode, $node)) {
                    print "; ERROR: too close nodes $cnode and $node, WayID=$road near (${nodes{$node}})\n";
                    $countclose ++;
                }
                $cnode = $node;
            }
        }
    }
    print STDERR "$countclose pairs fixed\n";
}






####    Dumping roads

my %roadid;

print STDERR "Writing roads...          ";
print "\n\n\n; ### Roads\n\n";

my $roadcount = 1;
while (my ($road, $pchain) = each %rchain) {
    my ($poly, $name, $rp) = @{$rprops{$road}};
    my @type = @{$polytype{$poly}};

    $roadid{$road} = $roadcount;

    #  @type == [ $mode, $type, $prio, $llev, $hlev, $rp ]
    print  "; WayID = $road\n";
    print  "; $poly\n";
    print  "[POLYLINE]\n";
    printf "Type=%s\n",        $type[1];
    printf "EndLevel=%d\n",    $type[4]             if ($type[4] > $type[3]);
    print  "Label=$name\n"                          if ($name);
    print  "StreetDesc=$name\n"                     if ($name && $navitel);
    print  "DirIndicator=1\n"                       if ((split /\,/, $rp)[2]);


    if ($risin{$road}) {
        my ($city, $region, $country) = @{$cityname{$risin{$road}}};
        print "CityName=$city\n";
        print "RegionName=$region\n"       if ($region);
        print "CountryName=$country\n"     if ($country);
    } elsif ($name && $defaultcity) {
        print "CityName=$defaultcity\n";
    }

    printf "Data%d=(%s)\n", $type[3], join ("), (", @nodes{@{$pchain}});
    printf "RoadID=%d\n", $roadcount++;
    printf "RouteParams=%s\n", $rp;

    my $nodcount=0;
    for (my $i=0; $i < scalar @{$pchain}; $i++) {
        my $node = $pchain->[$i];
        if ($nodid{$node}) {
            printf "Nod%d=%d,%d,%d\n", $nodcount++, $i, $nodid{$node}, $xnodes{$node};
        }
    }

    print  "[END]\n\n\n";
}

printf STDERR "%d written\n", $roadcount-1;





####    Background object (?)


if ($bounds && $background) {

    print "\n\n\n; ### Background\n\n";
    print  "[POLYGON]\n";
    print  "Type=0x4b\n";
    print  "EndLevel=9\n";
    print  "Data0=($minlat,$minlon), ($minlat,$maxlon), ($maxlat,$maxlon), ($maxlat,$minlon) \n";
    print  "[END]\n\n\n";

}




####    Writing turn restrictions

my $counttrest = 0;
if ($restrictions) {

    print "\n\n\n; ### Turn restrictions\n\n";

    print STDERR "Writing restrictions...   ";

    while ( my ($relid, $tr) = each %trest ) {

        print  "\n; Rel=$relid    ". join (" : ", %{$tr}) ."\n";
        my $err = 0;

        if  ($tr->{fr_dir} == 0)        {  $err=1;  print "; ERROR: RelID=$relid FROM road does'n have VIA end node\n";  }
        if  ($tr->{to_dir} == 0)        {  $err=1;  print "; ERROR: RelID=$relid TO road does'n have VIA end node\n"; }

        if ( !$err && $tr->{type} eq "no" ) {
            dumptrest ($tr);
        }

        if ( !$err && $tr->{type} eq "only") {
#            print  "; Roads:  " . join (", ", @{$nodeways{$trest{$relid}->{node}}}) . "\n";
            my %newtr = (
                    node    => $tr->{node},
                    fr_way  => $tr->{fr_way},
                    fr_dir  => $tr->{fr_dir},
                    fr_pos  => $tr->{fr_pos}
                );
            for my $road (@{$nodeways{$trest{$relid}->{node}}}) {
                print "; To road $road \n";
                $newtr{to_way} = $road;
                $newtr{to_pos} = indexof( $rchain{$road}, $tr->{node} );

                if ( $newtr{to_pos} > 0 && !($tr->{to_way} eq $road && $tr->{to_dir} eq -1) && ($rprops{$road}[2] !~ /^.,.,1/) ) {
                    $newtr{to_dir} = -1;
                    dumptrest (\%newtr);
                }
                if ( $newtr{to_pos} < $#{$rchain{$road}} && !($tr->{to_way} eq $road && $tr->{to_dir} eq 1) ) {
                    $newtr{to_dir} = 1;
                    dumptrest (\%newtr);
                }
            }
        }
    }

    print STDERR "$counttrest written\n";
}





print STDERR "All done!!\n\n";








####    Functions

use Encode;
use Text::Unidecode;

sub convert_string {            # String

   ##  Non-standard characters
   my %cmap = (
       # Romanian
       "\x{0218}" => "\x{015E}",        "\x{0219}" => "\x{015F}",       # S-comma
       "\x{021A}" => "\x{0162}",        "\x{021B}" => "\x{0163}",       # T-comma
   );

   my $str = decode("utf8", $_[0]);
   map { $str =~ s/$_/$cmap{$_}/g } keys %cmap  if !$translit;
   $str = unidecode($str)                       if $translit;
   $str = uc($str)                              if $upcase;
   $str = encode ( ($nocodepage ? "utf8" : "cp".$codepage), $str);

   $str =~ s/\&#(\d+)\;/chr($1)/ge;
   $str =~ s/\&amp\;/\&/gi;
   $str =~ s/\&apos\;/\'/gi;
   $str =~ s/\&quot\;/\"/gi;

   $str =~ s/[\?\"\<\>\*]/ /g;
   $str =~ s/[\x00-\x1F]//g;

   $str =~ s/^[ \;\.\,\!\-\+\_]+//;
   $str =~ s/ +/ /g;
   $str =~ s/\s+$//;
   
   return $str;
}



sub closenodes {                # NodeID1, NodeID2

    my ($lat1, $lon1) = split ",", $nodes{$_[0]};
    my ($lat2, $lon2) = split ",", $nodes{$_[1]};

    my ($clat, $clon) = ( ($lat1+$lat2)/2, ($lon1+$lon2)/2 );
    my ($dlat, $dlon) = ( ($lat2-$lat1), ($lon2-$lon1) );
    my $klon = cos ($clat*3.14159/180);

    my $ldist = $fixclosedist * 180 / 20_000_000;

    my $res = ($dlat**2 + ($dlon*$klon)**2) < $ldist**2;

    # fixing
    if ($res) {
        if ($dlon == 0) {
            $nodes{$_[0]} = ($clat - $ldist/2 * ($dlat==0 ? 1 : ($dlat <=> 0) )) . "," . $clon;
            $nodes{$_[1]} = ($clat + $ldist/2 * ($dlat==0 ? 1 : ($dlat <=> 0) )) . "," . $clon;
        } else {
            my $azim = $dlat / $dlon;
            my $ndlon = sqrt ($ldist**2 / ($klon**2 + $azim**2)) / 2;
            my $ndlat = $ndlon * abs($azim);

            $nodes{$_[0]} = ($clat - $ndlat * ($dlat <=> 0)) . "," . ($clon - $ndlon * ($dlon <=> 0));
            $nodes{$_[1]} = ($clat + $ndlat * ($dlat <=> 0)) . "," . ($clon + $ndlon * ($dlon <=> 0));
        }
    }
    return $res;
}



sub lcos {                      # NodeID1, NodeID2, NodeID3

    my ($lat1, $lon1) = split ",", $nodes{$_[0]};
    my ($lat2, $lon2) = split ",", $nodes{$_[1]};
    my ($lat3, $lon3) = split ",", $nodes{$_[2]};

    my $klon = cos (($lat1+$lat2+$lat3)/3*3.14159/180);

    my $xx = (($lat2-$lat1)**2+($lon2-$lon1)**2*$klon**2)*(($lat3-$lat2)**2+($lon3-$lon2)**2*$klon**2);
    return -1   if ( $xx == 0);
    return (($lat2-$lat1)*($lat3-$lat2)+($lon2-$lon1)*($lon3-$lon2)*$klon**2) / sqrt ($xx);
}



sub indexof {                   # \@array, $elem

    return -1   if ( !defined($_[0]) );
    for (my $i=0; $i < scalar @{$_[0]}; $i++)
        { return $i if ($_[0]->[$i] eq $_[1]); }
    return -1;
}


sub speedcode {                 # $speed
    return 7            if ($_[0] > 110);
    return 6            if ($_[0] > 90);
    return 5            if ($_[0] > 80);
    return 4            if ($_[0] > 60);
    return 3            if ($_[0] > 40);
    return 2            if ($_[0] > 20);
    return 1            if ($_[0] > 10);
    return 0;
}


sub insidebbox {                # $latlon
    my ($lat, $lon) = split /,/, $_[0];
    return 1    if ( $lat>$minlat && $lon>$minlon && $lat<$maxlat && $lon<$maxlon );
    return 0;
}


sub insidebounds {                # $latlon
    return $boundpoly->isinside([reverse split /,/, $_[0]]);
}


sub dumptrest {                 # \%trest

    $counttrest ++;

    my $tr = $_[0];

    my $i = $tr->{fr_pos} - $tr->{fr_dir};
    $i -= $tr->{fr_dir}         while ( !$nodid{$rchain{$tr->{fr_way}}->[$i]} && $i>=0 && $i < $#{$rchain{$tr->{fr_way}}} );
    my $j = $tr->{to_pos} + $tr->{to_dir};
    $j += $tr->{to_dir}         while ( !$nodid{$rchain{$tr->{to_way}}->[$j]} && $j>=0 && $j < $#{$rchain{$tr->{to_way}}} );

    unless ( ${nodid{$tr->{node}}} ) {
        print "; Restriction is outside boundaries\n";
        return;
    }
    
    print  "[Restrict]\n";
    printf "Nod=${nodid{$tr->{node}}}\n";
    print  "TraffPoints=${nodid{$rchain{$tr->{fr_way}}->[$i]}},${nodid{$tr->{node}}},${nodid{$rchain{$tr->{to_way}}->[$j]}}\n";
    print  "TraffRoads=${roadid{$tr->{fr_way}}},${roadid{$tr->{to_way}}}\n";
    print  "Time=\n";
    print  "[END-Restrict]\n\n";
}


sub centroid {

    my $slat = 0;
    my $slon = 0;
    my $ssq  = 0;

    for (my $i = 1; $i < scalar(@_) - 1; $i++ ) {
        my $tlat = ($_[0]->[0]+$_[$i]->[0]+$_[$i+1]->[0])/3;
        my $tlon = ($_[0]->[1]+$_[$i]->[1]+$_[$i+1]->[1])/3;

        my $tsq = (($_[$i]->[0]-$_[0]->[0])*($_[$i+1]->[1]-$_[0]->[1]) - ($_[$i+1]->[0]-$_[0]->[0])*($_[$i]->[1]-$_[0]->[1]));
        
        $slat += $tlat * $tsq;
        $slon += $tlon * $tsq;
        $ssq  += $tsq;
    }

    return ($slat/$ssq , $slon/$ssq);
}