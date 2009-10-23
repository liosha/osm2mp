#!/usr/bin/perl


##
##  Required packages: 
##    * Template-toolkit
##    * Getopt::Long
##    * Text::Unidecode
##    * List::MoreUtils
##    * Math::Polygon
##    * Math::Polygon::Tree
##    * Math::Geometry::Planar::GPC::Polygon
##
##  See http://cpan.org/ or use PPM (Perl package manager) or CPAN module
##

##
##  Licenced under GPL v2
##



use strict;

use Template;
use Getopt::Long;

use Encode;
use Text::Unidecode;

use Math::Polygon;
use Math::Geometry::Planar::GPC::Polygon qw{ new_gpc };
use Math::Polygon::Tree;

use List::Util qw{ first reduce };
use List::MoreUtils qw{ all any none first_index };

# debug
use Data::Dump qw{ dd };





####    Settings

my $version = '0.80a';

my $cfgpoi          = 'poi.cfg';
my $cfgpoly         = 'poly.cfg';
my $cfgheader       = 'header.tpl';

my $mapid           = '88888888';
my $mapname         = 'OSM';

my $codepage        = '1251';
my $nocodepage      = 0;

my $detectdupes     = 1;

my $routing         = 1;
my $mergeroads      = 1;
my $mergecos        = 0.2;
my $splitroads      = 1;
my $fixclosenodes   = 1;
my $fixclosedist    = 3.0;       # set 5.5 for cgpsmapper 0097 and earlier
my $maxroadnodes    = 60;
my $restrictions    = 1;
my $disableuturns   = 0;
my $destsigns       = 1;

my $upcase          = 0;
my $translit        = 0;
my $ttable          = q{};

my $bbox;
my $bpolyfile;
my $osmbbox         = 0;
my $background      = 1;

my $shorelines      = 0;
my $waterback       = 0;

my $navitel         = 0;
my $makepoi         = 1;

my $defaultcountry  = "Earth";
my $defaultregion   = "OSM";
my $defaultcity     = q{};

my $poiregion       = 0;        # sometimes causes mapsource crash?
my $poicontacts     = 1;

my $nametaglist     = "name,ref,int_ref,addr:housenumber,operator";

# ??? make command-line parameters?
my @housenamelist   = qw{ addr:housenumber addr:housename };
my @citynamelist    = qw{ place_name name };
my @regionnamelist  = qw{ addr:region is_in:region addr:state is_in:state };
my @countrynamelist = qw{ addr:country is_in:country_code is_in:country };


my %yesno = (
    'yes'            => '1',
    'true'           => '1',
    '1'              => '1',
    'permissive'     => '1',
    'designated'     => '1',
    'no'             => '0',
    'false'          => '0',
    '0'              => '0',
    'private'        => '0',
);

GetOptions (
    'cfgpoi=s'          => \$cfgpoi,
    'cfgpoly=s'         => \$cfgpoly,
    'header=s'          => \$cfgheader,
    'mapid=s'           => \$mapid,
    'mapname=s'         => \$mapname,
    'codepage=s'        => \$codepage,
    'nocodepage'        => \$nocodepage,
    'routing!'          => \$routing,
    'mergeroads!'       => \$mergeroads,
    'mergecos=f'        => \$mergecos,
    'detectdupes!'      => \$detectdupes,
    'splitroads!'       => \$splitroads,
    'fixclosenodes!'    => \$fixclosenodes,
    'fixclosedist=f'    => \$fixclosedist,
    'maxroadnodes=f'    => \$maxroadnodes,
    'restrictions!'     => \$restrictions,
    'destsigns!'        => \$destsigns,
    'defaultcountry=s'  => \$defaultcountry,
    'defaultregion=s'   => \$defaultregion,
    'defaultcity=s'     => \$defaultcity,
    'nametaglist=s'     => \$nametaglist,
    'upcase!'           => \$upcase,
    'translit!'         => \$translit,
    'ttable=s'          => \$ttable,
    'bbox=s'            => \$bbox,
    'bpoly=s'           => \$bpolyfile,
    'osmbbox!'          => \$osmbbox,
    'background!'       => \$background,
    'disableuturns!'    => \$disableuturns,
    'shorelines!'       => \$shorelines,
    'waterback!'        => \$waterback,
    'navitel!'          => \$navitel,
    'makepoi!'          => \$makepoi,
    'poiregion!'        => \$poiregion,
    'poicontacts!'      => \$poicontacts,
);

undef $codepage   if $nocodepage;

our %cmap;
if ( $ttable ) {
    open TT, '<', $ttable;
    my $code = "%cmap = ( " . join( q{}, <TT> ) . " );";
    close TT;

    eval $code;
}

my @nametagarray = split q{,}, $nametaglist;




####    Action

print STDERR "\n  ---|   OSM -> MP converter  $version   (c) 2008,2009  liosha, xliosha\@gmail.com\n\n";

usage() unless (@ARGV);




####    Reading configs

my %poitype;

open CFG, $cfgpoi;
while (<CFG>) {
    next   if (!$_) || /^\s*[\#\;]/;
    chomp;
    my ($k, $v, $type, $llev, $hlev, $mode) = split /\s+/;
    if ($type) {
        $llev = 0   unless defined $llev;
        $hlev = 1   unless defined $hlev;
        $poitype{"$k=$v"} = [ $type, $llev, $hlev, $mode ];
    }
}
close CFG;



my %polytype;

open CFG, $cfgpoly;
while (<CFG>) {
    next   if (!$_) || /^\s*[\#\;]/;
    chomp;
    my $prio = 0;
    my ($k, $v, $mode, $type, $llev, $hlev, $rp, @p) = split /\s+/;

    if ($type) {
        if ($type =~ /(.+),(\d)/) {
            $type = $1;
            $prio = $2;
        }
        $llev = 0   unless defined $llev;
        $hlev = 1   unless defined $hlev;

     $polytype{"$k=$v"} = [ $mode, $type, $prio, $llev, $hlev, $rp ];
   }
}
close CFG;




####    Header

my $tmpl = Template->new( { ABSOLUTE => 1 } );
$tmpl->process ($cfgheader, {
    mapid           => $mapid,
    mapname         => $mapname,
    codepage        => $codepage,
    routing         => $routing,
    defaultcountry  => $defaultcountry,
    defaultregion   => $defaultregion,
}) 
or die $tmpl->error();




####    Info

use POSIX qw{ strftime };
print "\n; Converted from OpenStreetMap data with  osm2mp $version  (" . strftime ("%Y-%m-%d %H:%M:%S", localtime) . ")\n\n";


my ($infile) = @ARGV;
open IN, $infile;
print STDERR "Processing file $infile\n\n";




####    Bounds

my $bounds;
my @bound;
my $boundtree;


if ($bbox) {
    $bounds = 1 ;
    my ($minlon, $minlat, $maxlon, $maxlat) = split q{,}, $bbox;
    @bound = ( [$minlon,$minlat], [$maxlon,$minlat], [$maxlon,$maxlat], [$minlon,$maxlat], [$minlon,$minlat] );
    $boundtree = Math::Polygon::Tree->new( \@bound );
}

if ($bpolyfile) {
    $bounds = 1;
    print STDERR "Initialising bounds...    ";

    open (PF, $bpolyfile) 
        or die "Could not open file: $bpolyfile: $!";

    ## ??? need advanced polygon?
    while (<PF>) {
        if (/^\d/) {
            @bound = ();
        } 
        elsif (/^\s+([0-9.E+-]+)\s+([0-9.E+-]+)/) {
            push @bound, [$1,$2];
        }
        elsif (/^END/) {
            @bound = reverse @bound     if  Math::Polygon->new( @bound )->isClockwise();
            $boundtree = Math::Polygon::Tree->new( \@bound );
        }
    }
    close (PF);
    printf STDERR "%d segments\n", scalar @bound;
}


####    1st pass 
###     loading nodes

my %node;
my ( $waypos, $relpos ) = ( 0, 0 );

print STDERR "Loading nodes...          ";

while ( my $line = <IN> ) {

    if ( $line =~ /<node.* id=["']([^"']+)["'].* lat=["']([^"']+)["'].* lon=["']([^"']+)["']/ ) {
        $node{$1} = "$2,$3";
        next;
    }

    if ( $osmbbox  &&  $line =~ /<bounds?/ ) {
        my ($minlat, $minlon, $maxlat, $maxlon);
        if ( $line =~ /<bounds/ ) {
            ($minlat, $minlon, $maxlat, $maxlon)
                = ( $line =~ /minlat=["']([^"']+)["'] minlon=["']([^"']+)["'] maxlat=["']([^"']+)["'] maxlon=["']([^"']+)["']/ );
        } 
        else {
            ($minlat, $minlon, $maxlat, $maxlon) 
                = ( $line =~ /box=["']([^"',]+),([^"',]+),([^"',]+),([^"']+)["']/ );
        }
        $bbox = join q{,}, ($minlon, $minlat, $maxlon, $maxlat);
        $bounds = 1     if $bbox;
        @bound = ( [$minlon,$minlat], [$maxlon,$minlat], [$maxlon,$maxlat], [$minlon,$maxlat], [$minlon,$minlat] );
        $boundtree = Math::Polygon::Tree->new( \@bound );
    }

    last    if $line =~ /<way/;
}
continue { $waypos = tell IN }


printf STDERR "%d loaded\n", scalar keys %node;


my $boundgpc = new_gpc();
$boundgpc->add_polygon ( \@bound, 0 )    if $bounds;





###     loading relations

# multipolygons
my %mpoly;
my %mphole;

# turn restrictions
my $counttrest = 0;
my $countsigns = 0;
my %trest;
my %nodetr;


print STDERR "Loading relations...      ";

my $relid;
my %reltag;
my %relmember;


while ( <IN> ) {
    last if /<relation/;
}
continue { $relpos = tell IN }
seek IN, $relpos, 0;


while ( my $line = <IN> ) {

    if ( $line =~ /<relation/ ) {
        ($relid)    =  $line =~ / id=["']([^"']+)["']/;
        %reltag     = ();
        %relmember  = ();
        next;
    }

    if ( $line =~ /<member/ ) {
        my ($mtype, $mid, $mrole)  = 
            $line =~ / type=["']([^"']+)["'].* ref=["']([^"']+)["'].* role=["']([^"']*)["']/;
        push @{ $relmember{"$mtype:$mrole"} }, $mid;
        next;
    }

    if ( $line =~ /<tag/ ) {
        my ($key, $val)  =  $line =~ / k=["']([^"']+)["'].* v=["']([^"']+)["']/;
        $reltag{$key} = $val;
        next;
    }

    if ( $line =~ /<\/relation/ ) {

        # simple multipolygons (one outer, many inners, all closed)
        if ( $reltag{'type'} eq 'multipolygon' ) {
            if ( $relmember{'way:'} ) {
                push @{ $relmember{'way:outer'} }, @{ $relmember{'way:'} };
            }
            unless ( $relmember{'way:outer'} ) {
                print "; ERROR: Multipolygon RelID=$relid doesn't have OUTER way\n";
                next;
            }
            unless ( $relmember{'way:inner'} ) {
                print "; ERROR: Multipolygon RelID=$relid has no INNER ways\n";
                next;
            }

            my $outer = $relmember{'way:outer'}->[0];
            my @inner = @{ $relmember{'way:inner'} };

            $mpoly{$outer} = [ @inner ];
            @mphole{@inner} = (1) x scalar @inner;
        }

        # turn restrictions
        if ( $routing  &&  $restrictions  &&  $reltag{'type'} eq 'restriction' ) {
            unless ( $relmember{'way:from'} ) {
                print "; ERROR: Turn restriction RelID=$relid doesn't have FROM way\n";
                next;
            }
            unless ( $relmember{'node:via'} ) {
                # VIA ways is not supported now
                print "; ERROR: Turn restriction RelID=$relid doesn't have VIA node\n";
                next;
            }
            if ( $reltag{'restriction'} eq 'no_u_turn'  &&  !$relmember{'way:to'} ) {
                $relmember{'way:to'} = $relmember{'way:from'};
            }
            unless ( $relmember{'way:to'} ) {
                print "; ERROR: Turn restriction RelID=$relid doesn't have TO way\n";
                next;
            }

            $counttrest ++;
            $trest{$relid} = { 
                node    => $relmember{'node:via'}->[0],
                type    => ($reltag{'restriction'} =~ /^only_/) ? 'only' : 'no',
                fr_way  => $relmember{'way:from'}->[0],
                fr_dir  => 0,
                fr_pos  => -1,
                to_way  => $relmember{'way:to'}->[0],
                to_dir  => 0,
                to_pos  => -1,
            };

            if ( exists $reltag{'except'} ) {
                $trest{$relid}->{param} = join q{,}, CalcAccessRules(
                    { map { $_ => 'yes' } split( /\s*[,;]\s*/, $reltag{'except'} ) },
                    [ 1,1,1,1,1,0,1,1 ]
                );
            }

            push @{$nodetr{ $relmember{'node:via'}->[0] }}, $relid;
        }

        # destination signs
        if ( $routing  &&  $destsigns  &&  $reltag{'type'} eq 'destination_sign' ) {
            unless ( $relmember{'way:from'} ) {
                print "; ERROR: Destination sign RelID=$relid has no FROM ways\n";
                next;
            }
            unless ( $relmember{'way:to'} ) {
                print "; ERROR: Destination sign RelID=$relid doesn't have TO way\n";
                next;
            }

            my $node = $relmember{'node:sign'}->[0]         if $relmember{'node:sign'};
            $node = $relmember{'node:intersection'}->[0]    if $relmember{'node:intersection'};
            unless ( $node ) {
                print "; ERROR: Destination sign RelID=$relid doesn't have SIGN or INTERSECTION node\n";
                next;
            }

            my $name = first { defined } @reltag{ qw{ destination label name } };
            unless ( $name ) {
                print "; ERROR: Destination sign RelID=$relid doesn't have label tag\n";
                next;
            }

            $countsigns ++;
            for my $from ( @{ $relmember{'way:from'} } ) {
                $trest{$relid} = { 
                    name    => convert_string( $name ),
                    node    => $node,
                    type    => 'sign',
                    fr_way  => $from,
                    fr_dir  => 0,
                    fr_pos  => -1,
                    to_way  => $relmember{'way:to'}->[0],
                    to_dir  => 0,
                    to_pos  => -1,
                };
            }

            push @{$nodetr{ $node }}, $relid;
        }
    }
}

printf STDERR "%d multipolygons\n", scalar keys %mpoly;
print  STDERR "                          $counttrest turn restrictions\n"     if $counttrest;
print  STDERR "                          $countsigns destination signs\n"     if $countsigns;





####    2nd pass
###     loading cities, multipolygon holes and checking node dupes

my %city;

print STDERR "Loading cities...         ";

my $wayid;
my %waytag;
my @chain;
my $dupcount;

seek IN, $waypos, 0;

while ( my $line = <IN> ) {

    if ( $line =~/<way / ) {
        ($wayid)  = $line =~ / id=["']([^"']+)["']/;
        @chain    = ();
        %waytag   = ();
        $dupcount = 0;
        next;
    }

    if ( $line =~ /<nd/ ) {
        my ($ref)  =  $line =~ / ref=["']([^"']+)["']/;
        if ( $node{$ref} ) {
            unless ( scalar @chain  &&  $ref eq $chain[-1] ) {
                push @chain, $ref;
            }
            else {
                print "; ERROR: WayID=$wayid has dupes at ($node{$ref})\n";
                $dupcount ++;
            }
        }
        next;
    }

   if ( $line =~ /<tag.* k=["']([^"']+)["'].* v=["']([^"']+)["']/ ) {
       $waytag{$1} = $2;
       next;
   }

   if ( $line =~ /<\/way/ ) {

       ##       this way is multipolygon inner
       if ( $mphole{$wayid} ) {
           $mphole{$wayid} = [ @chain ];
       }

       ##       this way is city bound
       if ( exists $waytag{'place'} && ( $waytag{'place'} eq 'city' || $waytag{'place'} eq 'town' ) ) { 
           my $name = convert_string ( first {defined} @waytag{@citynamelist} );

           if ( $name  &&  $chain[0] eq $chain[-1] ) {
               print "; Found city: WayID=$wayid - $name\n";
               $city{$wayid} = {
                    name        =>  $name,
                    region      =>  convert_string( first {defined} @waytag{@regionnamelist} ),
                    country     =>  convert_string( first {defined} @waytag{@countrynamelist} ),
                    bound       =>  Math::Polygon::Tree->new( [ map { [ split q{,}, $node{$_} ] } @chain ] ),
               };
           } else {
               print "; ERROR: City without name WayID=$wayid\n"            unless  $name;
               print "; ERROR: City polygon WayID=$wayid is not closed\n"   if  $chain[0] ne $chain[-1];
           }
       }
       next;
   }

   last  if $line =~ /<relation/;
}

printf STDERR "%d loaded\n", scalar keys %city;





####    3rd pass
###     writing POIs

my %barrier;
my %xnode;


print STDERR "Writing POIs...           ";

print "\n\n\n; ### Points\n\n";

my $countpoi = 0;
my $nodeid;
my %nodetag;

seek IN, 0, 0;

while ( my $line = <IN> ) {

    if ( $line =~ /<node/ ) {
        ($nodeid)  =  $line =~ / id=["']([^"']+)["']/;
        %nodetag   =  ();
        next;
    }

    if ( $line =~ /<tag/ ) {
        my ($key, $val)  =  $line =~ / k=["']([^"']+)["'].* v=["']([^"']+)["']/;
        $nodetag{$key}   =  $val;
        next;
    }

    if ( $line =~ /<\/node/ ) {

        ##  Barriers
        if ( $routing  &&  $nodetag{'barrier'} ) {
            AddBarrier({ nodeid => $nodeid,  tags => \%nodetag });
        }

        ##  Forced external nodes
        if ( $routing  &&  exists $nodetag{'garmin:extnode'}  &&  $yesno{$nodetag{'garmin:extnode'}} ) {
            $xnode{$nodeid} = 1;
        }

        ##  POI
        my $poitag = first { $poitype{"$_=$nodetag{$_}"} } keys %nodetag;
        next  unless  $poitag;
        next  unless  !$bounds || is_inside_bounds( $node{$nodeid} );

        $countpoi ++;
        my $poi = "$poitag=$nodetag{$poitag}";
        my ($type, $llev, $hlev, $poimode) = @{$poitype{$poi}};

        my %poiinfo = (
                nodeid      => $nodeid,
                comment     => $poi,
                type        => $type,
                level_l     => $llev,
                level_h     => $hlev,
                tags        => \%nodetag,
            );

        if ( $poimode eq 'city' ) {
            $poiinfo{City}          = 'Y';
            $poiinfo{add_region}    = 1;
        }
        elsif ( $poimode eq 'contacts' ) {
            $poiinfo{add_contacts}  = 1;
        }

        AddPOI ( \%poiinfo );
    }

    last  if  $line =~ /<way/;
}

printf STDERR "%d written\n", $countpoi;





####    Loading roads and coastlines, and writing other ways

my %road;
my %coast;

print STDERR "Processing ways...        ";

print "\n\n\n; ### Lines and polygons\n\n";

my $countlines    = 0;
my $countpolygons = 0;

my $city;
my @chainlist;
my $inbounds;

seek IN, $waypos, 0;

while ( my $line = <IN> ) {

    if ( $line =~ /<way/ ) {
        ($wayid)  =  $line =~ / id=["']([^"']+)["']/;

        %waytag       = ();
        @chain        = ();
        @chainlist    = ();
        $inbounds     = 0;
        $city         = 0;

        next;
    }

    if ( $line =~ /<nd/ ) {
        my ($ref)  =  $line =~ / ref=["']([^"']*)["']/;
        if ( $node{$ref}  &&  $ref ne $chain[-1] ) {
            push @chain, $ref;
            if ($bounds) {
                my $in = is_inside_bounds( $node{$ref} );
                if ( !$inbounds &&  $in )   { push @chainlist, ($#chain ? $#chain-1 : 0); }
                if (  $inbounds && !$in )   { push @chainlist, $#chain; }
                $inbounds = $in;
            }
        }
        next;
    }

    if ( $line =~ /<tag/ ) {
        $line =~ / k=["']([^"']*)["'].* v=["']([^"']*)["']/;
        $waytag{$1} = $2;
        next;
    }

    if ( $line =~ /<\/way/ ) {

        my $poly  =  reduce { $polytype{$a}->[2] > $polytype{$b}->[2]  ?  $a : $b }
                        grep { exists $polytype{$_} }
                        map {"$_=$waytag{$_}"}  keys %waytag;
        next  unless $poly;

        my ($mode, $type, $prio, $llev, $hlev, $rp) = @{$polytype{$poly}};

        my $name = convert_string( first {defined} @waytag{@nametagarray} );

        @chainlist = (0)            unless $bounds;
        push @chainlist, $#chain    unless ($#chainlist % 2);


        ##  this way is map line - dump it

        if ( $mode eq 'l'  ||  $mode eq 's'  || ( !$routing && $mode eq 'r' ) ) {
            if ( scalar @chain < 2 ) {
                print "; ERROR: WayID=$wayid has too few nodes at ($node{$chain[0]})\n";
                next;
            }

            for ( my $i = 0;  $i < $#chainlist+1;  $i += 2 ) {
                $countlines ++;

                print  "; WayID = $wayid\n";
                print  "; $poly\n";
                print  "[POLYLINE]\n";
                printf "Type=%s\n",         $type;
                printf "EndLevel=%d\n",     $hlev       if  $hlev > $llev;
                print  "Label=$name\n"                  if  $name;
                printf "Data%d=(%s)\n",     $llev, join( q{), (}, @node{@chain[$chainlist[$i]..$chainlist[$i+1]]} );
                print  "[END]\n\n\n";
            }
        }


        ##  this way is coastline - load it

        if ( $mode eq 's'  &&  $shorelines ) {
            if ( scalar @chain < 2 ) {
                print "; ERROR: WayID=$wayid has too few nodes at ($node{$chain[0]})\n";
            } 
            else {
                for ( my $i = 0;  $i < $#chainlist+1;  $i += 2 ) {
                    $coast{$chain[$chainlist[$i]]} = [ @chain[$chainlist[$i]..$chainlist[$i+1]] ];
                }
            }
        }


        ##  this way is map polygon - clip it and dump

        if ( $mode eq 'p' ) {
            if ( scalar @chain <= 3 ) {
                print "; ERROR: area WayID=$wayid has too few nodes near ($node{$chain[0]})\n";
                next;
            }

            if ( $chain[0] ne $chain[-1] ) {
                print "; ERROR: area WayID=$wayid is not closed at ($node{$chain[0]})\n";
            }

            if ( !$bounds  ||  scalar @chainlist ) {

                print  "; WayID = $wayid\n";
                print  "; $poly\n";

                my $polygon  = [ map { [reverse split q{,}, $node{$_}] } @chain ];
                my @plist    = ($polygon);

                if ( $mpoly{$wayid} ) {
                    for my $hole ( grep { ref $mphole{$_} } @{$mpoly{$wayid}} ) {
                        push @plist, [ map { [reverse split q{,}, $node{$_}] } @{$mphole{$hole}} ];
                    }
                }

                #   clip
                if ( $bounds  &&  !defined $boundtree->contains_polygon_rough( $polygon ) ) {
                    my $gpc = new_gpc();
                    $gpc->add_polygon( shift @plist, 0 );
                    
                    for my $hole ( @plist ) {
                        $gpc->add_polygon( $hole, 1 );
                    }

                    $gpc    =  $gpc->clip_to( $boundgpc, 'INTERSECT' );
                    @plist  =  sort  { $#{$b} <=> $#{$a} }  $gpc->get_polygons();
                }

                next    unless @plist;

                $countpolygons ++;

                print  "[POLYGON]\n";
                printf "Type=%s\n",        $type;
                printf "EndLevel=%d\n",    $hlev    if  $hlev > $llev;
                print  "Label=$name\n"              if  $name;


                if ( $navitel  ||  ($makepoi && $rp && $name) ) {
                    $city = $city{ FindCity( $chain[0] ) };
                }

                ## Navitel
                if ( $navitel ) {
                    my $housenumber = convert_string( first {defined} @waytag{@housenamelist} );
                    if ( $housenumber && $waytag{'addr:street'} ) {
                        print  "HouseNumber=$housenumber\n";
                        printf "StreetDesc=%s\n", convert_string( $waytag{'addr:street'} );
                        if ( $city ) {
                            print "CityName="    . $city->{name}      . "\n";
                            print "RegionName="  . $city->{region}    . "\n"      if $city->{region};
                            print "CountryName=" . $city->{country}   . "\n"      if $city->{country};
                        } 
                        elsif ( $defaultcity ) {
                            print "CityName=$defaultcity\n";
                        }
                    }
                }
            
                for my $polygon ( @plist ) {
                    printf "Data%d=(%s)\n", $llev, join( q{), (}, map {join( q{,}, reverse @{$_} )} @{$polygon} );
                }

                ## Rusa - floors
                if ( $waytag{'building:levels'} ) {
                    printf "Floors=%d\n",  0 + $waytag{'building:levels'};
                }
                if ( $waytag{'building:height'} ) {
                    printf "Floors=%d\n",  3 * $waytag{'building:height'};
                }
            
                print "[END]\n\n\n";
            

                if ( $makepoi && $rp && $name ) {
            
                    my ($poi, $pll, $phl) = split q{,}, $rp;
            
                    AddPOI ({
                            latlon       => ( join q{,}, centroid( @{$plist[0]} ) ),
                            comment      => "for area $poly WayID=$wayid",
                            type         => $poi,
                            tags         => \%waytag,
                            level_l      => $pll,
                            level_h      => $phl,
                            add_contacts => 1,
                        });
                }
                
            }
        }


        ##  this way is road - load

        if ( $mode eq 'r'  &&  $routing ) {
            if ( scalar @chain <= 1 ) {
                print "; ERROR: Road WayID=$wayid has too few nodes at ($node{$chain[0]})\n";
                next;
            }


            # set routing parameters and access rules
            # RouteParams=speed,class,oneway,toll,emergency,delivery,car,bus,taxi,foot,bike,truck
            my @rp = split q{,}, $rp;

            if ( $waytag{'maxspeed'} > 0 ) {
               $waytag{'maxspeed'} *= 1.61      if  $waytag{'maxspeed'} =~ /mph$/i;
               $rp[0]  = speed_code( $waytag{'maxspeed'} / 1.3 ); # real speed ?
            }
            if ( $waytag{'maxspeed:practical'} > 0 ) {
               $waytag{'maxspeed:practical'} *= 1.61        if  $waytag{'maxspeed:practical'} =~ /mph$/i;
               $rp[0]  = speed_code( $waytag{'maxspeed:practical'} );
            }

            $rp[2] = $yesno{$waytag{'oneway'}}      if exists $yesno{$waytag{'oneway'}};
            $rp[3] = $yesno{$waytag{'toll'}}        if exists $yesno{$waytag{'toll'}};
            @rp[4..11] = CalcAccessRules( \%waytag, [ @rp[4..11] ] );

            # determine city
            if ( $name ) {
                $city =  FindCity( $chain[0], $chain[-1] );
            }

            # load roads and external nodes
            for ( my $i = 0;  $i < $#chainlist;  $i += 2 ) {
                $road{"$wayid:$i"} = {
                    type    =>  $poly,
                    name    =>  $name,
                    chain   =>  [ @chain[$chainlist[$i]..$chainlist[$i+1]] ],
                    city    =>  $city,
                    rp      =>  join( q{,}, @rp ),
                };

                if ( $bounds ) {
                    if ( !is_inside_bounds( $node{$chain[$chainlist[$i]]} ) ) {
                        $xnode{ $chain[$chainlist[$i]]   }    = 1;
                        $xnode{ $chain[$chainlist[$i]+1] }    = 1;
                    }
                    if ( !is_inside_bounds( $node{$chain[$chainlist[$i+1]]} ) ) {
                        $xnode{ $chain[$chainlist[$i+1]]   }  = 1;
                        $xnode{ $chain[$chainlist[$i+1]-1] }  = 1;
                    }
                }
            }

            # process associated turn restrictions
            if ( $restrictions  ||  $destsigns ) {
                if ( $chainlist[0] == 0 ) {
                    for my $relid ( grep { $trest{$_}->{fr_way} eq $wayid } @{$nodetr{$chain[0]}} ) {
                        $trest{$relid}->{fr_way} = "$wayid:0";
                        $trest{$relid}->{fr_dir} = -1;
                        $trest{$relid}->{fr_pos} = 0;
                    }
                    for my $relid ( grep { $trest{$_}->{to_way} eq $wayid } @{$nodetr{$chain[0]}} ) {
                        $trest{$relid}->{to_way} = "$wayid:0";
                        $trest{$relid}->{to_dir} = 1;
                        $trest{$relid}->{to_pos} = 0;
                    }
                }
                if ( $chainlist[-1] == $#chain ) {
                    for my $relid ( grep { $trest{$_}->{fr_way} eq $wayid } @{$nodetr{$chain[-1]}} ) {
                        $trest{$relid}->{fr_way} = "$wayid:" . ($#chainlist-1);
                        $trest{$relid}->{fr_dir} = 1;
                        $trest{$relid}->{fr_pos} = $chainlist[-1] - $chainlist[-2];
                    }
                    for my $relid ( grep { $trest{$_}->{to_way} eq $wayid } @{$nodetr{$chain[-1]}} ) {
                        $trest{$relid}->{to_way} = "$wayid:" . ($#chainlist-1);
                        $trest{$relid}->{to_dir} = -1;
                        $trest{$relid}->{to_pos} = $chainlist[-1] - $chainlist[-2];
                    }
                }
            }
        } # if road
    } # </way>

    last  if $line =~ /<relation/;
}

print  STDERR "$countlines lines and $countpolygons polygons dumped\n";
printf STDERR "                          %d roads loaded\n",      scalar keys %road     if  $routing;
printf STDERR "                          %d coastlines loaded\n", scalar keys %coast    if  $shorelines;





####    Processing coastlines

if ( $shorelines ) {

    my $boundcross = 0;

    print "\n\n\n";
    print STDERR "Processing shorelines...  ";


    ##  merging
    my @keys = keys %coast;
    my $i = 0;
    while ($i < scalar @keys) {
        while (    $coast{$keys[$i]}
                && $coast{$coast{$keys[$i]}->[-1]}  
                && $coast{$keys[$i]}->[-1] ne $keys[$i] 
                && ( !$bounds  ||  is_inside_bounds( $node{$coast{$keys[$i]}->[-1]} ) ) ) {
            my $mnode = $coast{$keys[$i]}->[-1];
            pop  @{$coast{$keys[$i]}};
            push @{$coast{$keys[$i]}}, @{$coast{$mnode}};
            delete $coast{$mnode};
        }

#        if ( $coast{$keys[$i]} ) {
#            print  "; merged coastline $keys[$i]\n";
#            print  "[POLYLINE]\n";
#            print  "Type=0x15\n";
#            print  "EndLevel=4\n";
#            printf "Data0=(%s)\n",          join (q{), (}, @node{ @{ $coast{$keys[$i]} } });
#            print  "[END]\n\n\n";
#        }

        $i++;
    }


    ##  tracing bounds
    if ( $bounds ) {

        my @tbound;
        my $pos = 0;

        for my $i ( 0 .. $#bound-1 ) {

            push @tbound, {
                type    =>  'bound', 
                point   =>  $bound[$i], 
                pos     =>  $pos
            };

            for my $sline ( keys %coast ) {

                # check start of coastline
                my $p1      = [ reverse  split q{,}, $node{$coast{$sline}->[0]} ];
                my $p2      = [ reverse  split q{,}, $node{$coast{$sline}->[1]} ];
                my $ipoint  = segment_intersection( $bound[$i], $bound[$i+1], $p1, $p2 );

                if ( $ipoint ) {
                    if ( any { $_->{type} eq 'end'  &&  $_->{point} ~~ $ipoint } @tbound ) {
                        @tbound = grep { !( $_->{type} eq 'end'  &&  $_->{point} ~~ $ipoint ) } @tbound;
                    } 
                    else { 
                        $boundcross ++;
                        push @tbound, {
                            type    =>  'start', 
                            point   =>  $ipoint, 
                            pos     =>  $pos + segment_length( $bound[$i], $ipoint ), 
                            line    =>  $sline,
                        };
                    }
                }

                # check end of coastline
                $p1      = [ reverse  split q{,}, $node{$coast{$sline}->[-1]} ];
                $p2      = [ reverse  split q{,}, $node{$coast{$sline}->[-2]} ];
                $ipoint  = segment_intersection( $bound[$i], $bound[$i+1], $p1, $p2 );

                if ( $ipoint ) {
                    if ( any { $_->{type} eq 'start'  &&  $_->{point} ~~ $ipoint } @tbound ) {
                        @tbound = grep { !( $_->{type} eq 'start'  &&  $_->{point} ~~ $ipoint ) } @tbound;
                    } 
                    else { 
                        $boundcross ++;
                        push @tbound, {
                            type    =>  'end', 
                            point   =>  $ipoint, 
                            pos     =>  $pos + segment_length( $bound[$i], $ipoint ), 
                            line    =>  $sline,
                        };
                    }
                }
            }

            $pos += segment_length( $bound[$i], $bound[$i+1] );
        }

        # rotate if sea at $tbound[0]
        my $tmp  =  reduce { $a->{pos} < $b->{pos} ? $a : $b }  grep { $_->{type} ne 'bound' } @tbound;
        if ( $tmp->{type} eq 'end' ) {
            for ( grep { $_->{pos} <= $tmp->{pos} } @tbound ) {
                 $_->{pos} += $pos;
            }
        }

        # merge lines
        $tmp = 0;
        for my $node ( sort { $a->{pos}<=>$b->{pos} } @tbound ) {
            my $latlon = join q{,}, reverse @{$node->{point}};
            $node{$latlon} = $latlon;

            if ( $node->{type} eq 'start' ) {
                $tmp = $node;
                $coast{$tmp->{line}}->[0] = $latlon;
            } 
            if ( $node->{type} eq 'bound'  &&  $tmp ) {
                unshift @{$coast{$tmp->{line}}}, ($latlon);
            } 
            if ( $node->{type} eq 'end'  &&  $tmp ) {
                $coast{$node->{line}}->[-1] = $latlon;
                if ( $node->{line} eq $tmp->{line} ) {
                    push @{$coast{$node->{line}}}, $coast{$node->{line}}->[0];
                } else {
                    push @{$coast{$node->{line}}}, @{$coast{$tmp->{line}}};
                    for ( grep { $_->{line} eq $tmp->{line} } @tbound ) {
                        $_->{line} = $node->{line};
                    }
                }
                $tmp = 0;
            }
        }
    }


    ##  detecting lakes and islands
    my %lake;
    my %island;

    for my $loop ( grep { $coast{$_}->[0] eq $coast{$_}->[-1] } keys %coast ) {

        # filter huge polygons to avoid cgpsmapper's crash
        if ( scalar @{$coast{$loop}} > 100000 ) {
            printf "; WARNING: skipped too big coastline $loop (%d nodes)\n", scalar @{$coast{$loop}};
            next;
        }

        if ( Math::Polygon->new( map { [ split q{,}, $node{$_} ] } @{$coast{$loop}} )->isClockwise() ) {
            $island{$loop} = 1;
        } 
        else {
            $lake{$loop} = Math::Polygon::Tree->new( [ map { [ reverse split q{,}, $node{$_} ] } @{$coast{$loop}} ] );
        }
    }


    ##  adding sea background
    if ( $waterback && $bounds && !$boundcross ) {
        $lake{'background'} = $boundtree;
    }
    
    ##  writing
    my $countislands = 0;

    for my $sea ( sort { scalar @{$coast{$b}} <=> scalar @{$coast{$a}} } keys %lake ) {
        print  "; sea $sea\n";
        print  "[POLYGON]\n";
        print  "Type=0x3c\n";
        print  "EndLevel=4\n";

        printf "Data0=(%s)\n",  join( q{), (},  
            $sea eq 'background'  
                ?  map { join q{,}, reverse @{$_} } @bound
                :  @node{@{$coast{$sea}}}
            );
        
        for my $island  ( keys %island ) {
            if ( $lake{$sea}->contains( [ reverse split q{,}, $node{$island} ] ) ) {
                $countislands ++;
                printf "Data0=(%s)\n",  join( q{), (}, @node{@{$coast{$island}}} );
                delete $island{$island};
            }
        }
        
        print  "[END]\n\n\n";

    }

    printf STDERR "%d lakes, %d islands\n", scalar keys %lake, $countislands;

    undef %lake;
    undef %island;
}




####    Process roads

my %nodid;
my %roadid;
my %nodeways;

if ( $routing ) {

    ###     detecting end nodes

    my %enode;
    my %rstart;

    while ( my ($roadid, $road) = each %road ) {
        $enode{$road->{chain}->[0]}  ++;
        $enode{$road->{chain}->[-1]} ++;
        $rstart{$road->{chain}->[0]}->{$roadid} = 1;
    }



    ###     merging roads

    if ( $mergeroads ) {
        print "\n\n\n";
        print STDERR "Merging roads...          ";
    
        my $countmerg = 0;
        my @keys = keys %road;
    
        my $i = 0;
        while ($i < scalar @keys) {
            
            my $r1 = $keys[$i];

            unless ( exists $road{$r1} )        {  $i++;  next;  }

            my $p1 = $road{$r1}->{chain};
    
            my @list = ();
            for my $r2 ( keys %{$rstart{$p1->[-1]}} ) {
                if ( $r1 ne $r2  
                  && [ @{$road{$r1}}{qw{type name city rp}} ] ~~ [ @{$road{$r2}}{qw{type name city rp}} ]
                  && lcos( $p1->[-2], $p1->[-1], $road{$r2}->{chain}->[1] ) > $mergecos ) {
                    push @list, $r2;
                }
            }

            # merging
            if ( @list ) {
                $countmerg ++;
                @list  =  sort {  lcos( $p1->[-2], $p1->[-1], $road{$b}->{chain}->[1] ) 
                              <=> lcos( $p1->[-2], $p1->[-1], $road{$a}->{chain}->[1] )  }  @list;

                printf "; FIX: Road WayID=$r1 may be merged with %s at (%s)\n", join ( q{, }, @list ), $node{$p1->[-1]};
    
                my $r2 = $list[0];
    
                # process associated restrictions
                if ( $restrictions  ||  $destsigns ) {
                    while ( my ($relid, $tr) = each %trest )  {
                        if ( $tr->{fr_way} eq $r2 )  {
                            print "; FIX: RelID=$relid FROM moved from WayID=$r2($tr->{fr_pos})";
                            $tr->{fr_way}  = $r1;
                            $tr->{fr_pos} += $#{$road{$r1}->{chain}};
                            print " to WayID=$r1($tr->{fr_pos})\n";
                        }
                        if ( $tr->{to_way} eq $r2 )  {
                            print "; FIX: RelID=$relid TO moved from WayID=$r2($tr->{to_pos})";
                            $tr->{to_way}  = $r1;
                            $tr->{to_pos} += $#{$road{$r1}->{chain}};
                            print " to WayID=$r1($tr->{to_pos})\n";
                        }
                    }
                }
    
                $enode{$road{$r2}->{chain}->[0]} -= 2;
                pop  @{$road{$r1}->{chain}};
                push @{$road{$r1}->{chain}}, @{$road{$r2}->{chain}};
    
                delete $rstart{ $road{$r2}->{chain}->[0] }->{$r2};
                delete $road{$r2};
    
            } else {
                $i ++;
            }
        }
    
        print STDERR "$countmerg merged\n";
    }





    ###    generating routing graph

    my %rnode;

    print STDERR "Detecting road nodes...   ";

    while (my ($roadid, $road) = each %road) {
        for my $node (@{$road->{chain}}) {
            $rnode{$node} ++;
            push @{$nodeways{$node}}, $roadid
                if ( $nodetr{$node}  ||  $barrier{$node}  ||  ( $disableuturns && $enode{$node}==2 ) );
        }
    }

    my $nodcount = 1;
    my $utcount  = 0;

    for my $node ( keys %rnode ) {
        if (  $rnode{$node} > 1  ||  $enode{$node}  ||  $xnode{$node}  ||  $barrier{$node}
          ||  exists $nodetr{$node}  &&  scalar @{$nodetr{$node}} ) {
            $nodid{$node} = $nodcount++;
        }
        
        ##  Disable U-turns
        if ( $disableuturns  &&  $rnode{$node} == 2  &&  $enode{$node} == 2  &&  !$barrier{$node} ) {
            if ( $road{ $nodeways{$node}->[0] }->{rp}  =~  /^.,.,0/ ) {
                my $pos = first_index { $_ eq $node } @{$road{ $nodeways{$node}->[0] }->{chain}};
                $trest{ 'ut'.$utcount++ } = { 
                    node    => $node,
                    type    => 'no',
                    fr_way  => $nodeways{$node}->[0],
                    fr_dir  => $pos > 0  ?   1  :  -1,
                    fr_pos  => $pos,
                    to_way  => $nodeways{$node}->[0],
                    to_dir  => $pos > 0  ?  -1  :   1,
                    to_pos  => $pos,
                };
            }
            if ( $road{ $nodeways{$node}->[1] }->{rp}  =~  /^.,.,0/ ) {
                my $pos = first_index { $_ eq $node } @{$road{ $nodeways{$node}->[1] }->{chain}};
                $trest{ 'ut'.$utcount++ } = {
                    node    => $node,
                    type    => 'no',
                    fr_way  => $nodeways{$node}->[1],
                    fr_dir  => $pos > 0  ?   1  :  -1,
                    fr_pos  => $pos,
                    to_way  => $nodeways{$node}->[1],
                    to_dir  => $pos > 0  ?  -1  :   1,
                    to_pos  => $pos,
                };
            }
        }
    }

    undef %rnode;

    printf STDERR "%d found\n", scalar keys %nodid;





    ###    detecting duplicate road segments


    if ( $detectdupes ) {

        my %segway;
    
        print STDERR "Detecting duplicates...   ";
        
        print "\n\n\n; ### Duplicate roads\n\n";
    
        while ( my ($roadid, $road) = each %road ) {
            for my $i ( 0 .. $#{$road->{chain}} - 1 ) {
                if (  $nodid{ $road->{chain}->[$i] } 
                  &&  $nodid{ $road->{chain}->[$i+1] } ) {
                    my $seg = join q{:}, sort {$a cmp $b} ($road->{chain}->[$i], $road->{chain}->[$i+1]);
                    push @{$segway{$seg}}, $roadid;
                }
            }
        }
    
        my $countdupsegs  = 0;
    
        my %roadseg;
        my %roadpos;
    
        for my $seg ( grep { $#{$segway{$_}} > 0 }  keys %segway ) {
            $countdupsegs ++;
            my $roads    =  join q{, }, sort {$a cmp $b} @{$segway{$seg}};
            my ($point)  =  split q{:}, $seg;
            $roadseg{$roads} ++;
            $roadpos{$roads} = $node{$point};
        }
    
        for my $road ( keys %roadseg ) {
            printf "; ERROR: Roads $road has $roadseg{$road} duplicate segments near ($roadpos{$road})\n";
        }
    
        printf STDERR "$countdupsegs segments, %d roads\n", scalar keys %roadseg;
    }




    ####    fixing self-intersections and long roads

    if ( $splitroads ) {

        print STDERR "Splitting roads...        ";

        print "\n\n\n";
        
        my $countself = 0;
        my $countlong = 0;
        
        while ( my ($roadid, $road) = each %road ) {
            my $break   = 0;
            my @breaks  = ();
            my $rnod    = 1;
            my $prev    = 0;

            #   test for split conditions
            for my $i ( 1 .. $#{$road->{chain}} ) {
                $rnod ++    if  $nodid{ $road->{chain}->[$i] };

                if ( any { $_ eq $road->{chain}->[$i] } @{$road->{chain}}[$break..$i-1] ) {
                    $countself ++;
                    if ( $road->{chain}->[$i] ne $road->{chain}->[$prev] ) {
                        $break = $prev;
                        push @breaks, $break;
                    } else {
                        $break = ($i + $prev) >> 1;
                        push @breaks, $break;
                        $nodid{ $road->{chain}->[$break] }  =  $nodcount++;
                        printf "; FIX: Added NodID=%d for NodeID=%s at (%s)\n", 
                            $nodid{ $road->{chain}->[$break] },
                            $road->{chain}->[$break],
                            $node{$road->{chain}->[$break]};
                    }
                    $rnod = 2;
                }

                if ( $rnod == $maxroadnodes ) {
                    $countlong ++;
                    $break = $prev;
                    push @breaks, $break;
                    $rnod = 2;
                }

                $prev = $i      if  $nodid{ $road->{chain}->[$i] };
            }



            #   split
            if ( @breaks ) {
                printf "; FIX: WayID=$roadid is splitted at %s\n", join( q{, }, @breaks );
                push @breaks, $#{$road->{chain}};

                for my $i ( 0 .. $#breaks - 1 ) {
                    my $id = $roadid.'/'.($i+1);
                    printf "; FIX: Added road %s, nodes from %d to %d\n", $id, $breaks[$i], $breaks[$i+1];
                    
                    $road{$id} = {
                        chain   => [ @{$road->{chain}}[$breaks[$i] .. $breaks[$i+1]] ],
                        type    => $road{$roadid}->{type},
                        name    => $road{$roadid}->{name},
                        city    => $road{$roadid}->{city},
                        rp      => $road{$roadid}->{rp},
                    };

                    #   update nod->road list
                    for my $nod ( grep { exists $nodeways{$_} } @{$road{$id}->{chain}} ) {
                        push @{$nodeways{$nod}}, $id;
                    }

                    #   move restrictions
                    if ( $restrictions  ||  $destsigns ) {
                        while ( my ($relid, $tr) = each %trest )  {
                            if (  $tr->{to_way} eq $roadid 
                              &&  $tr->{to_pos} >  $breaks[$i]   - (1 + $tr->{to_dir}) / 2 
                              &&  $tr->{to_pos} <= $breaks[$i+1] - (1 + $tr->{to_dir}) / 2 ) {
                                print "; FIX: Turn restriction RelID=$relid TO moved from $roadid($tr->{to_pos})";
                                $tr->{to_way}  =  $id;
                                $tr->{to_pos}  -= $breaks[$i];
                                print " to $id($tr->{to_pos})\n";
                            }
                            if (  $tr->{fr_way} eq $roadid 
                              &&  $tr->{fr_pos} >  $breaks[$i]   + ($tr->{fr_dir} - 1) / 2
                              &&  $tr->{fr_pos} <= $breaks[$i+1] + ($tr->{fr_dir} - 1) / 2 ) {
                                print "; FIX: Turn restriction RelID=$relid FROM moved from $roadid($tr->{fr_pos})";
                                $tr->{fr_way} =  $id;
                                $tr->{fr_pos} -= $breaks[$i];
                                print " to $id($tr->{fr_pos})\n";
                            }
                        }
                    }
                }

                #   update nod->road list
                for my $nod ( grep { exists $nodeways{$_} } @{$road->{chain}}[$breaks[0]+1 ..$#{$road->{chain}}] ) {
                    $nodeways{$nod} = [ grep { $_ ne $roadid } @{$nodeways{$nod}} ];
                }

                $#{$road->{chain}} = $breaks[0];
            }
        }
        print STDERR "$countself self-intersections, $countlong long roads\n";
    }
    




    ###    fixing too close nodes

    if ( $fixclosenodes ) {
        
        print "\n\n\n";
        print STDERR "Fixing close nodes...     ";

        my $countclose = 0;
        
        while ( my ($roadid, $road) = each %road ) {
            my $cnode = $road->{chain}->[0];
            for my $node ( grep { $_ ne $cnode && $nodid{$_} } @{$road->{chain}}[1..$#{$road->{chain}}] ) {
                if ( fix_close_nodes( $cnode, $node ) ) {
                    $countclose ++;
                    print "; ERROR: too close nodes $cnode and $node, WayID=$roadid near (${node{$node}})\n";
                }
                $cnode = $node;
            }
        }
        print STDERR "$countclose pairs fixed\n";
    }




    ###    dumping roads


    print STDERR "Writing roads...          ";

    print "\n\n\n; ### Roads\n\n";

    my $roadcount = 1;
    
    while ( my ($roadid, $road) = each %road ) {

        my ($poly, $name, $rp) = ($road->{type}, $road->{name}, $road->{rp});
        my ($mode, $type, $prio, $llev, $hlev)  =  @{$polytype{$poly}};
        
        $roadid{$roadid} = $roadcount++;
        
        print  "; WayID = $roadid\n";
        print  "; $poly\n";
        print  "[POLYLINE]\n";
        printf "Type=%s\n",         $type;
        printf "EndLevel=%d\n",     $hlev       if  $hlev > $llev;
        print  "Label=$name\n"                  if  $name;
        print  "StreetDesc=$name\n"             if  $name  &&  $navitel;
        print  "DirIndicator=1\n"               if  $rp =~ /^.,.,1/;

        printf "Data%d=(%s)\n",     $llev, join( q{), (}, @node{@{$road->{chain}}} );
        printf "RoadID=%d\n",       $roadid{$roadid};
        printf "RouteParams=%s\n",  $rp;
        
        if ( $road->{city} ) {
            my $rcity = $city{$road->{city}};
            print "CityName=$rcity->{name}\n";
            print "RegionName=$rcity->{region}\n"       if ($rcity->{region});
            print "CountryName=$rcity->{country}\n"     if ($rcity->{country});
        } elsif ( $name  &&  $defaultcity ) {
            print "CityName=$defaultcity\n";
        }
        
        
        my $nodcount = 0;
        for my $i ( 0 .. $#{$road->{chain}} ) {
            my $node = $road->{chain}->[$i];
            if ( $nodid{$node} ) {
                printf "Nod%d=%d,%d,%d\n", $nodcount++, $i, $nodid{$node}, $xnode{$node};
            }
        }
        
        print  "[END]\n\n\n";
    }

    printf STDERR "%d written\n", $roadcount-1;

} # if $routing



####    Background object (?)


if ( $bounds && $background ) {

    print "\n\n\n; ### Background\n\n";
    print  "[POLYGON]\n";
    print  "Type=0x4b\n";
    print  "EndLevel=4\n";
    printf "Data0=(%s)\n",      join( q{), (},  map { join q{,}, reverse @{$_} } @bound );
    print  "[END]\n\n\n";

}




####    Writing turn restrictions


if ( $routing && ( $restrictions || $destsigns ) ) {

    print "\n\n\n; ### Turn restrictions and signs\n\n";

    print STDERR "Writing crossroads...     ";

    my $counttrest = 0;
    my $countsigns = 0;

    while ( my ($relid, $tr) = each %trest ) {

        unless ( $tr->{fr_dir} ) {
            print "; ERROR: RelID=$relid FROM road does'n have VIA end node\n";
            next;
        }
        unless ( $tr->{to_dir} ) {
            print "; ERROR: RelID=$relid TO road does'n have VIA end node\n";
            next;
        }

        print "\n; RelID = $relid (from $tr->{fr_way} $tr->{type} $tr->{to_way})\n\n";

        if ( $tr->{type} eq 'sign' ) {
            $countsigns ++;
            write_turn_restriction ($tr);
        }


        if ( $tr->{type} eq 'no' ) {
            $counttrest ++;
            write_turn_restriction ($tr);
        }

        if ( $tr->{type} eq 'only') {
            my %newtr = (
                    node    => $tr->{node},
                    type    => 'no',
                    fr_way  => $tr->{fr_way},
                    fr_dir  => $tr->{fr_dir},
                    fr_pos  => $tr->{fr_pos}
                );

            for my $roadid ( @{$nodeways{ $trest{$relid}->{node} }} ) {
                $newtr{to_way} = $roadid;
                $newtr{to_pos} = first_index { $_ eq $tr->{node} } @{$road{$roadid}->{chain}};

                if (  $newtr{to_pos} < $#{$road{$roadid}->{chain}} 
                  &&  !( $tr->{to_way} eq $roadid  &&  $tr->{to_dir} eq 1 ) ) {
                    print "; To road $roadid forward\n";
                    $newtr{to_dir} = 1;
                    $counttrest ++;
                    write_turn_restriction (\%newtr);
                }

                if (  $newtr{to_pos} > 0 
                  &&  !( $tr->{to_way} eq $roadid  &&  $tr->{to_dir} eq -1 ) 
                  &&  $road{$roadid}->{rp} !~ /^.,.,1/ ) {
                    print "; To road $roadid backward\n";
                    $newtr{to_dir} = -1;
                    $counttrest ++;
                    write_turn_restriction (\%newtr);
                }
            }
        }
    }

    ##  Barriers

    print "\n; ### Barriers\n\n";
    for my $node ( keys %barrier ) {
        print "; $barrier{$node}->{type}   NodeID = $node \n\n";
        my %newtr = (
            node    => $node,
            type    => 'no',
            param   => $barrier{$node}->{param},
        );
        for my $way_from ( @{$nodeways{$node}} ) {
            $newtr{fr_way} = $way_from;
            $newtr{fr_pos} = first_index { $_ eq $node } @{$road{ $way_from }->{chain}};
            
            for my $dir_from ( -1, 1 ) {
                
                next    if  $dir_from == -1  &&  $newtr{fr_pos} == $#{$road{ $way_from }->{chain}};
                next    if  $dir_from == 1   &&  $newtr{fr_pos} == 0;

                $newtr{fr_dir} = $dir_from;
                for my $way_to ( @{$nodeways{$node}} ) {
                    $newtr{to_way} = $way_to;
                    $newtr{to_pos} = first_index { $_ eq $node } @{$road{ $way_to }->{chain}};

                    for my $dir_to ( -1, 1 ) {
                        next    if  $dir_to == -1  &&  $newtr{to_pos} == 0;
                        next    if  $dir_to == 1   &&  $newtr{to_pos} == $#{$road{ $way_to }->{chain}};
                        next    if  $way_from == $way_to  &&  $dir_from == -$dir_to;

                        $newtr{to_dir} = $dir_to;
                        $counttrest ++;
                        write_turn_restriction (\%newtr);
                    }
                }
            }
        }
    }

    print STDERR "$counttrest restrictions, $countsigns signs\n";
}





print STDERR "All done!!\n\n";








####    Functions

sub convert_string {            # String

    my $str = decode('utf8', $_[0]);
    return $str     unless $str;
   
    unless ( $translit ) {
        for my $repl ( keys %cmap ) {
            $str =~ s/$repl/$cmap{$repl}/g;
        }
    }
    
    $str = unidecode($str)      if $translit;
    $str = uc($str)             if $upcase;
    
    $str = encode( ($nocodepage ? 'utf8' : 'cp'.$codepage), $str );
   
    $str =~ s/\&#(\d+)\;/chr($1)/ge;
    $str =~ s/\&amp\;/\&/gi;
    $str =~ s/\&apos\;/\'/gi;
    $str =~ s/\&quot\;/\"/gi;
    $str =~ s/\&[\d\w]+\;//gi;
   
    $str =~ s/[\?\"\<\>\*]/ /g;
    $str =~ s/[\x00-\x1F]//g;
   
    $str =~ s/^[ \`\'\;\.\,\!\-\+\_]+//;
    $str =~ s/ +/ /g;
    $str =~ s/\s+$//;
    
    return $str;
}



sub fix_close_nodes {                # NodeID1, NodeID2

    my ($lat1, $lon1) = split q{,}, $node{$_[0]};
    my ($lat2, $lon2) = split q{,}, $node{$_[1]};

    my ($clat, $clon) = ( ($lat1+$lat2)/2, ($lon1+$lon2)/2 );
    my ($dlat, $dlon) = ( ($lat2-$lat1),   ($lon2-$lon1)   );
    my $klon = cos( $clat * 3.14159 / 180 );

    my $ldist = $fixclosedist * 180 / 20_000_000;

    my $res = ($dlat**2 + ($dlon*$klon)**2) < $ldist**2;

    # fixing
    if ( $res ) {
        if ( $dlon == 0 ) {
            $node{$_[0]} = ($clat - $ldist/2 * ($dlat==0 ? 1 : ($dlat <=> 0) )) . q{,} . $clon;
            $node{$_[1]} = ($clat + $ldist/2 * ($dlat==0 ? 1 : ($dlat <=> 0) )) . q{,} . $clon;
        }
        else {
            my $azim  = $dlat / $dlon;
            my $ndlon = sqrt( $ldist**2 / ($klon**2 + $azim**2) ) / 2;
            my $ndlat = $ndlon * abs($azim);

            $node{$_[0]} = ($clat - $ndlat * ($dlat <=> 0)) . q{,} . ($clon - $ndlon * ($dlon <=> 0));
            $node{$_[1]} = ($clat + $ndlat * ($dlat <=> 0)) . q{,} . ($clon + $ndlon * ($dlon <=> 0));
        }
    }
    return $res;
}



sub lcos {                      # NodeID1, NodeID2, NodeID3

    my ($lat1, $lon1) = split q{,}, $node{$_[0]};
    my ($lat2, $lon2) = split q{,}, $node{$_[1]};
    my ($lat3, $lon3) = split q{,}, $node{$_[2]};

    my $klon = cos( ($lat1+$lat2+$lat3) / 3 * 3.14159 / 180 );

    my $xx = (($lat2-$lat1)**2+($lon2-$lon1)**2*$klon**2) * (($lat3-$lat2)**2+($lon3-$lon2)**2*$klon**2);

    return -1   if ( $xx == 0);
    return (($lat2-$lat1)*($lat3-$lat2)+($lon2-$lon1)*($lon3-$lon2)*$klon**2) / sqrt($xx);
}



sub speed_code {                        # $speed
    my ($spd) = @_;
    return 7        if $spd >= 110;
    return 6        if $spd >= 90;
    return 5        if $spd >= 80;
    return 4        if $spd >= 60;
    return 3        if $spd >= 40;
    return 2        if $spd >= 20;
    return 1        if $spd >= 10;
    return 0;
}



sub is_inside_bounds {                  # $latlon
    return $boundtree->contains( [ reverse split q{,}, $_[0] ] );
}



sub write_turn_restriction {            # \%trest

    my ($tr) = @_;

    my $i = $tr->{fr_pos} - $tr->{fr_dir};
    while ( !$nodid{ $road{$tr->{fr_way}}->{chain}->[$i] }  &&  $i >= 0  &&  $i < $#{$road{$tr->{fr_way}}->{chain}} ) {
        $i -= $tr->{fr_dir};
    }
    
    my $j = $tr->{to_pos} + $tr->{to_dir};
    while ( !$nodid{ $road{$tr->{to_way}}->{chain}->[$j] }  &&  $j >= 0  &&  $j < $#{$road{$tr->{to_way}}->{chain}} ) {
        $j += $tr->{to_dir};
    }

    unless ( ${nodid{$tr->{node}}} ) {
        print "; Outside boundaries\n";
        return;
    }

    if ( $tr->{type} eq 'sign' ) {
        print  "[Sign]\n";
        print  "SignPoints=${nodid{$road{$tr->{fr_way}}->{chain}->[$i]}},${nodid{$tr->{node}}},${nodid{$road{$tr->{to_way}}->{chain}->[$j]}}\n";
        print  "SignRoads=${roadid{$tr->{fr_way}}},${roadid{$tr->{to_way}}}\n";
        print  "SignParam=T,$tr->{name}\n";
        print  "[END-Sign]\n\n";
    } 
    else {
        print  "[Restrict]\n";
        print  "TraffPoints=${nodid{$road{$tr->{fr_way}}->{chain}->[$i]}},${nodid{$tr->{node}}},${nodid{$road{$tr->{to_way}}->{chain}->[$j]}}\n";
        print  "TraffRoads=${roadid{$tr->{fr_way}}},${roadid{$tr->{to_way}}}\n";
        print  "RestrParam=$tr->{param}\n"     if exists $tr->{param};
        print  "[END-Restrict]\n\n";
    }
}




sub usage  {

    my @onoff = ( "off", "on");

    my $usage = <<"END_USAGE";
Usage:  osm2mp.pl [options] file.osm > file.mp

Possible options [defaults]:

 --mapid <id>              map id            [$mapid]
 --mapname <name>          map name          [$mapname]

 --cfgpoi <file>           poi config        [$cfgpoi]
 --cfgpoly <file>          way config        [$cfgpoly]
 --header <file>           header template   [$cfgheader]

 --bbox <bbox>             comma-separated minlon,minlat,maxlon,maxlat
 --osmbbox                 use bounds from .osm              [$onoff[$osmbbox]]
 --bpoly <poly-file>       use bounding polygon from .poly-file

 --background              create background object          [$onoff[$background]]

 --codepage <num>          codepage number                   [$codepage]
 --nocodepage              leave all labels in utf-8         [$onoff[$nocodepage]]
 --upcase                  convert all labels to upper case  [$onoff[$upcase]]
 --translit                tranliterate labels               [$onoff[$translit]]
 --ttable <file>           character conversion table

 --nametaglist <list>      comma-separated list of tags for Label    [$nametaglist]
 --defaultcountry <name>   default data for street indexing  [$defaultcountry]
 --defaultregion <name>                                      [$defaultregion]
 --defaultcity <name>                                        [$defaultcity]
 --navitel                 write addresses for polygons              [$onoff[$navitel]]

 --routing                 produce routable map                      [$onoff[$routing]]
 --mergeroads              merge same ways                           [$onoff[$mergeroads]]
 --mergecos <cosine>       maximum allowed angle between roads to merge      [$mergecos]
 --splitroads              split long and self-intersecting roads    [$onoff[$splitroads]]
 --fixclosenodes           enlarge distance between too close nodes  [$onoff[$fixclosenodes]]
 --fixclosedist <dist>     minimum allowed distance                  [$fixclosedist m]
 --maxroadnodes <dist>     maximum number of nodes in road segment   [$maxroadnodes]
 --detectdupes             detect road duplicates                    [$onoff[$detectdupes]]

 --restrictions            process turn restrictions                 [$onoff[$restrictions]]
 --disableuturns           disable u-turns on nodes with 2 links     [$onoff[$disableuturns]]
 --destsigns               process destination signs                 [$onoff[$destsigns]]

 --shorelines              process shorelines                        [$onoff[$shorelines]]
 --waterback               water background (for island maps)        [$onoff[$waterback]]
 --makepoi                 create POIs for polygons                  [$onoff[$makepoi]]
 --poiregion               write region info for settlements         [$onoff[$poiregion]]
 --poicontacts             write contact info for POIs               [$onoff[$poicontacts]]


You can use no<option> disable features (i.e --nomergeroads)
END_USAGE

    print $usage;
    exit;
}



###     geometry functions

sub segment_length {
  my ($p1,$p2) = @_;
  return sqrt( ($p2->[0] - $p1->[0])**2 + ($p2->[1] - $p1->[1])**2 );
}


sub segment_intersection {
    my ($p11, $p12, $p21, $p22) = @_;

    my $Z  = ($p12->[1]-$p11->[1]) * ($p21->[0]-$p22->[0]) - ($p21->[1]-$p22->[1]) * ($p12->[0]-$p11->[0]);
    my $Ca = ($p12->[1]-$p11->[1]) * ($p21->[0]-$p11->[0]) - ($p21->[1]-$p11->[1]) * ($p12->[0]-$p11->[0]);
    my $Cb = ($p21->[1]-$p11->[1]) * ($p21->[0]-$p22->[0]) - ($p21->[1]-$p22->[1]) * ($p21->[0]-$p11->[0]);

    return undef    if  $Z == 0;

    my $Ua = $Ca / $Z;
    my $Ub = $Cb / $Z;

    return undef    if  $Ua < 0  ||  $Ua > 1  ||  $Ub < 0  ||  $Ub > 1;

    return [ $p11->[0] + ( $p12->[0] - $p11->[0] ) * $Ub,
             $p11->[1] + ( $p12->[1] - $p11->[1] ) * $Ub ];
}


sub centroid {

    my $slat = 0;
    my $slon = 0;
    my $ssq  = 0;

    for my $i ( 1 .. $#_-1 ) {
        my $tlat = ( $_[0]->[0] + $_[$i]->[0] + $_[$i+1]->[0] ) / 3;
        my $tlon = ( $_[0]->[1] + $_[$i]->[1] + $_[$i+1]->[1] ) / 3;

        my $tsq = ( ( $_[$i]  ->[0] - $_[0]->[0] ) * ( $_[$i+1]->[1] - $_[0]->[1] ) 
                  - ( $_[$i+1]->[0] - $_[0]->[0] ) * ( $_[$i]  ->[1] - $_[0]->[1] ) );
        
        $slat += $tlat * $tsq;
        $slon += $tlon * $tsq;
        $ssq  += $tsq;
    }

#    return ($slat/$ssq , $slon/$ssq);
    return ($slon/$ssq , $slat/$ssq);
}




####    Exported functions

sub FindCity {
    my @nodes = @_;
    return first { 
            my $cbound = $city{$_}->{bound};
            # map { print ("; ", join (q{:}, (split q{,}, ( exists $node{$_} ? $node{$_} : $_ ))), "\n" ) } @nodes;
            all { $cbound->contains( [ split q{,}, ( exists $node{$_} ? $node{$_} : $_ ) ] ) } @nodes;
        } keys %city;
}


sub AddPOI {
    my %param = %{$_[0]};

    my %tag   = exists $param{tags} ? %{$param{tags}} : ();

    return      unless  exists $param{nodeid}  ||  exists $param{latlon}; 
    return      unless  exists $param{type};

    my $llev  =  exists $param{level_l} ? $param{level_l} : 0;
    my $hlev  =  exists $param{level_h} ? $param{level_h} : 1;

    print  "; NodeID = $param{nodeid}\n"    if  exists $param{nodeid};
    print  "; $param{comment}\n"            if  exists $param{comment};

    my ($type, $type2) = split q{:}, $param{type};

    my $data = "($node{$param{nodeid}})"    if  exists $param{nodeid};
    $data    = "($param{latlon})"           if  exists $param{latlon};    

    print  "[POI]\n";
    
    print  "Type=$type\n";
    my $label = convert_string( first { defined } @{$param{tags}}{@nametagarray} );
    printf "Label=%s\n", $label     if $label && !exists( $param{Label} ); 

    printf "Data%d=$data\n", $llev;
    print  "EndLevel=$hlev\n"       if  $hlev > $llev;

    # region and country - for cities
    if ( $poiregion  &&  $label  &&  $param{add_region} ) {
        my $region  = convert_string( first { defined } @{$param{tags}}{@regionnamelist} );
        print "RegionName=$region\n"        if $region;
        my $country = convert_string( first { defined } @{$param{tags}}{@countrynamelist} );
        print "CountryName=$country\n"      if $country;
    }

    # contact information: address, phone
    if ( $poicontacts  &&  $param{add_contacts} ) {
        my $city = $city{ FindCity( $param{nodeid} ) }  if  $param{nodeid};
        $city = $city{ FindCity( $param{latlon} ) }     if  $param{latlon};
        if ( $city ) {
            print "CityName=$city->{name}\n";
            print "RegionName=$city->{region}\n"        if  $city->{region};
            print "CountryName=$city->{country}\n"      if  $city->{country};
        }
        elsif ( $defaultcity ) {
            print "CityName=$defaultcity\n";
        }
                                                            
        my $housenumber = convert_string( first {defined} @waytag{@housenamelist} );
        print  "HouseNumber=$housenumber\n"     if $housenumber;
        printf "StreetDesc=%s\n",   convert_string($tag{'addr:street'})     if  $tag{'addr:street'};
        printf "Zip=%s\n",          convert_string($tag{'addr:postcode'})   if  $tag{'addr:postcode'};
        printf "Phone=%s\n",        convert_string($tag{'phone'})           if  $tag{'phone'};
    }

    # ADD: marine information

    # other parameters - capital first letter!
    for my $key ( grep { /^[A-Z]/ } keys %param ) {
        next    if  $param{$key} eq q{};
        printf "$key=%s\n", convert_string($param{$key});
    }

    print  "[END]\n\n";

    if ( $type2 ) {
        print  "[POI]\n";
        print  "Type=$type2\n";
        print  "Label=$label\n";
        printf "Data%d=$data\n", $llev;
        print  "EndLevel=$hlev\n"       if  $hlev > $llev;
        print  "[END]\n\n";
    }
}


sub AddBarrier {
    my %param = %{$_[0]};

    return  unless  exists $param{nodeid};
    return  unless  exists $param{tags};

    my $acc = [ 1,1,1,1,1,1,1,1 ];

    $acc = [ 1,1,1,1,1,0,1,1 ]      if  $param{tags}->{'barrier'} ~~ [ qw{ block stile chain } ];
    $acc = [ 1,1,1,1,1,0,0,1 ]      if  $param{tags}->{'barrier'} eq 'bollard';
    $acc = [ 1,1,1,0,1,0,0,1 ]      if  $param{tags}->{'barrier'} eq 'bus_trap';
    $acc = [ 0,0,0,0,0,0,0,0 ]      if  $param{tags}->{'barrier'} eq 'cattle_grid';

    my @acc = CalcAccessRules( $param{tags}, $acc );
    return  if  none { $_ } @acc;

    $barrier{$param{nodeid}} = {
        type    =>  $param{tags}->{'barrier'},
        param   =>  join q{,}, @acc,
    };
}


sub CalcAccessRules {
    my %tag = %{ $_[0] };
    my @acc = @{ $_[1] };

    # emergency, delivery, car, bus, taxi, foot, bike, truck
    @acc[0,1,2,3,4,5,6,7]  =  (1-$yesno{$tag{'access'}})        x 8   if exists $yesno{$tag{'access'}};
    @acc[          5,6, ]  =  (  $yesno{$tag{'motorroad'}})     x 8   if exists $yesno{$tag{'motorroad'}};

    @acc[0,1,2,3,4,  6,7]  =  (1-$yesno{$tag{'vehicle'}})       x 8   if exists $yesno{$tag{'vehicle'}};
    @acc[0,1,2,3,4,    7]  =  (1-$yesno{$tag{'motor_vehicle'}}) x 8   if exists $yesno{$tag{'motor_vehicle'}};
    @acc[0,1,2,3,4,    7]  =  (1-$yesno{$tag{'motorcar'}})      x 8   if exists $yesno{$tag{'motorcar'}};

    @acc[          5,   ]  =  (1-$yesno{$tag{'foot'}})          x 1   if exists $yesno{$tag{'foot'}};
    @acc[            6, ]  =  (1-$yesno{$tag{'bicycle'}})       x 1   if exists $yesno{$tag{'bicycle'}};
    @acc[      3,4,     ]  =  (1-$yesno{$tag{'psv'}})           x 2   if exists $yesno{$tag{'psv'}};
    @acc[        4,     ]  =  (1-$yesno{$tag{'taxi'}})          x 1   if exists $yesno{$tag{'taxi'}};
    @acc[      3,       ]  =  (1-$yesno{$tag{'bus'}})           x 1   if exists $yesno{$tag{'bus'}};
    @acc[             7,]  =  (1-$yesno{$tag{'hgv'}})           x 1   if exists $yesno{$tag{'hgv'}};
    @acc[  1,         7,]  =  (1-$yesno{$tag{'goods'}})         x 2   if exists $yesno{$tag{'goods'}};

    return @acc;
}     

