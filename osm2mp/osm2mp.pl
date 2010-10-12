#!/usr/bin/perl

##
##  Required packages: 
##    * Template-toolkit
##    * Getopt::Long
##    * YAML  
##    * Text::Unidecode
##    * List::MoreUtils
##    * Math::Polygon
##    * Math::Polygon::Tree
##    * Math::Geometry::Planar::GPC::Polygon
##    * Tree::R  
##
##  See http://cpan.org/ or use PPM (Perl package manager) or CPAN module
##

##
##  Licenced under GPL v2
##



use 5.0100;
use strict;

use POSIX;
use YAML;
use Template;
use Getopt::Long;
use File::Spec;

use Encode;
use Text::Unidecode;

use Math::Polygon;
use Math::Geometry::Planar::GPC::Polygon 'new_gpc';
use Math::Polygon::Tree;
use Tree::R;

use List::Util qw{ first reduce sum min max };
use List::MoreUtils qw{ all none any first_index last_index uniq };

# debug
use Data::Dump 'dd';





####    Settings

my $version = '0.90b';

my $config          = [ 'garmin.yml' ];

my $mapid           = '88888888';
my $mapname         = 'OSM';

my $codepage        = '1251';
my $upcase          = 0;
my $translit        = 0;
my $ttable          = q{};

my $oneway          = 1;
my $routing         = 1;
my $mergeroads      = 1;
my $mergecos        = 0.2;
my $splitroads      = 1;
my $fixclosenodes   = 1;
my $fixclosedist    = 3.0;       # set 5.5 for cgpsmapper 0097 and earlier
my $maxroadnodes    = 60;
my $restrictions    = 1;
my $barriers        = 1;
my $disableuturns   = 0;
my $destsigns       = 1;
my $detectdupes     = 1;

my $roadshields     = 1;
my $transportstops  = 1;
my $streetrelations = 1;
my $interchange3d   = 1;

my $bbox;
my $bpolyfile;
my $osmbbox         = 0;
my $background      = 1;
my $lessgpc         = 1;

my $shorelines      = 0;
my $hugesea         = 0;
my $waterback       = 0;
my $marine          = 1;

my $addressing      = 1;
my $navitel         = 0;
my $addrfrompoly    = 1;
my $makepoi         = 1;
my $country_list;
my $defaultcountry  = "Earth";
my $defaultregion   = "OSM";
my $defaultcity;
my $poiregion       = 1;
my $poicontacts     = 1;

my $transport_mode  = undef;


####    Global vars

my %yesno;
my %taglist;

my %node;
my %waychain;

my %city;
my $city_rtree = new Tree::R;
my %suburb;

my %poi;
my $poi_rtree = new Tree::R;


GetOptions (
    'config=s@'         => \$config,

    'mapid=s'           => \$mapid,
    'mapname=s'         => \$mapname,
    'codepage=s'        => \$codepage,
    'nocodepage'        => sub { undef $codepage },
    'upcase!'           => \$upcase,
    'translit!'         => \$translit,
    'ttable=s'          => \$ttable,
    
    'oneway!'           => \$oneway,
    'routing!'          => \$routing,
    'mergeroads!'       => \$mergeroads,
    'mergecos=f'        => \$mergecos,
    'detectdupes!'      => \$detectdupes,
    'splitroads!'       => \$splitroads,
    'maxroadnodes=f'    => \$maxroadnodes,
    'fixclosenodes!'    => \$fixclosenodes,
    'fixclosedist=f'    => \$fixclosedist,
    'restrictions!'     => \$restrictions,
    'barriers!'         => \$barriers,
    'disableuturns!'    => \$disableuturns,
    'destsigns!'        => \$destsigns,
    'roadshields!'      => \$roadshields,
    'transportstops!'   => \$transportstops,
    'streetrelations!'  => \$streetrelations,
    'interchange3d!'    => \$interchange3d,
    'transport=s'       => \$transport_mode,
    'notransport'       => sub { undef $transport_mode },

    'defaultcountry=s'  => \$defaultcountry,
    'defaultregion=s'   => \$defaultregion,
    'defaultcity=s'     => \$defaultcity,
    'countrylist=s'     => \$country_list,

    'bbox=s'            => \$bbox,
    'bpoly=s'           => \$bpolyfile,
    'osmbbox!'          => \$osmbbox,
    'background!'       => \$background,
    'lessgpc!'          => \$lessgpc,
    'shorelines!'       => \$shorelines,
    'hugesea=i'         => \$hugesea,
    'waterback!'        => \$waterback,
    'marine!'           => \$marine,

    'addressing!'       => \$addressing,
    'navitel!'          => \$navitel,
    'addrfrompoly!'     => \$addrfrompoly,
    'makepoi!'          => \$makepoi,
    'poiregion!'        => \$poiregion,
    'poicontacts!'      => \$poicontacts,

    'namelist=s%'       => sub { $taglist{$_[1]} = [ split /[ ,]+/, $_[2] ] },
    
    # deprecated
    'nametaglist=s'     => sub { $taglist{label} = [ split /[ ,]+/, $_[1] ] },
);


$codepage = 'utf8'  unless defined $codepage;
my $codepagenum = ( $codepage =~ /^cp\-?(\d+)$/i ) ? $1 : $codepage;
$codepage = "cp$codepagenum"    if $codepagenum =~ /^\d+$/;

our %cmap;
if ( $ttable ) {
    open TT, '<', $ttable;
    my $code = '%cmap = ( ' . join( q{}, <TT> ) . " );";
    close TT;

    eval $code;
}

my %transport_code = (
    emergency   => 0,
    police      => 0,
    delivery    => 1,
    car         => 2,
    motorcar    => 2,
    bus         => 3,
    taxi        => 4,
    foot        => 5,
    pedestrian  => 5,
    bike        => 6,
    bicycle     => 6,
    truck       => 7,
);
$transport_mode = $transport_code{ $transport_mode }
    if exists  $transport_code{ $transport_mode };

my %country_code;
if ( $country_list ) {
    open CL, '<:encoding(utf8)', $country_list;
    while ( my $line = <CL> ) {
        chomp $line;
        next if $line =~ /^#/;
        next if $line =~ /^\s+$/;
        my ($code, $name) = split /\s\s\s+/, $line;
        $country_code{uc $code} = $name;
    }
    close CL;
}




####    Action

print STDERR "\n  ---|   OSM -> MP converter  $version   (c) 2008-2010  liosha, xliosha\@gmail.com\n\n";

usage() unless (@ARGV);




####    Reading configs

my %config;
print STDERR "Loading configuration...  ";

while ( my $cfgfile = shift @$config ) {
    my %cfgpart = YAML::LoadFile $cfgfile;
    while ( my ( $key, $item ) = each %cfgpart ) {
        if ( $key eq 'load' && ref $item ) {
            my ( $vol, $dir, undef ) = File::Spec->splitpath( $cfgfile );
            for my $addcfg ( @$item ) {
                push @$config, File::Spec->catpath( $vol, $dir, $addcfg );
            }
        }
        elsif ( $key eq 'yesno' ) {
            %yesno = %{ $item };
        }
        elsif ( $key eq 'taglist' ) {
            while ( my ( $key, $val ) = each %$item ) {
                next if exists $taglist{$key};
                $taglist{$key} = $val;
            }
        }
        elsif ( $key eq 'nodes' || $key eq 'ways' ) {
            for my $rule ( @$item ) {
                if ( exists $rule->{id} 
                        &&  (my $index = first_index { exists $_->{id} && $_->{id} eq $rule->{id} } @{$config{$key}}) >= 0 ) {
                    $config{$key}->[$index] = $rule;
                }
                else {
                    push @{$config{$key}}, $rule;
                }
            }
        }
        else {
            %config = ( %config, $key => $item );
        }
    }
}

print STDERR "Ok\n\n";





####    Header

$defaultcountry = convert_string( $country_code{uc $defaultcountry} )   
    if exists $country_code{uc $defaultcountry};

my $tmpl = Template->new();
$tmpl->process (\$config{header}, {
    mapid           => $mapid,
    mapname         => $mapname,
    codepage        => $codepagenum,
    routing         => $routing,
    defaultcountry  => $defaultcountry,
    defaultregion   => $defaultregion,
}) 
or die $tmpl->error();




####    Info

print "\n; #### Converted from OpenStreetMap data with  osm2mp $version  (" . strftime ("%Y-%m-%d %H:%M:%S", localtime) . ")\n\n\n";


my ($infile) = @ARGV;
open IN, '<', $infile;
print STDERR "Processing file $infile\n\n";




####    Bounds

my $bounds = 0;
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
            push @bound, [ $1+0, $2+0 ];
        }
        elsif (/^END/) {
            @bound = reverse @bound     if  Math::Polygon->new( @bound )->isClockwise();
            $boundtree = Math::Polygon::Tree->new( \@bound );
            last;
        }
    }
    close (PF);
    printf STDERR "%d segments\n", scalar @bound;
}


####    1st pass 
###     loading nodes

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
my %ampoly; #advanced

# turn restrictions
my $counttrest = 0;
my $countsigns = 0;
my %trest;
my %nodetr;

# transport
my $countroutes = 0;
my %trstop;

# streets
my %street;
my $count_streets = 0;

# roads numbers
my %road_ref;
my $count_ref_roads = 0;


print STDERR "Loading relations...      ";

my $relid;
my %reltag;
my %relmember;


while ( <IN> ) {
    last if /<relation/;
}
continue { $relpos = tell IN }
seek IN, $relpos, 0;


while ( my $line = decode 'utf8', <IN> ) {

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
        my ($key, undef, $val)  =  $line =~ / k=["']([^"']+)["'].* v=(["'])(.+)\2/;
        $reltag{$key} = $val    unless exists $config{skip_tags}->{$key};
        next;
    }

    if ( $line =~ /<\/relation/ ) {

        # multipolygon
        if ( $reltag{'type'} eq 'multipolygon'  ||  $reltag{'type'} eq 'boundary' ) {

            push @{$relmember{'way:outer'}}, @{$relmember{'way:'}}
                if exists $relmember{'way:'};
            push @{$relmember{'way:outer'}}, @{$relmember{'way:exclave'}}
                if exists $relmember{'way:exclave'};
            push @{$relmember{'way:inner'}}, @{$relmember{'way:enclave'}}
                if exists $relmember{'way:enclave'};

            unless ( exists $relmember{'way:outer'} ) {
                print "; ERROR: Multipolygon RelID=$relid doesn't have OUTER way\n";
                next;
            }

            $ampoly{$relid} = {
                outer   =>  $relmember{'way:outer'},
                inner   =>  $relmember{'way:inner'},
                tags    =>  { %reltag },
            };

            next    unless exists $relmember{'way:inner'} && @{$relmember{'way:outer'}}==1;

            # old simple multipolygon
            my $outer = $relmember{'way:outer'}->[0];
            my @inner = @{ $relmember{'way:inner'} };

            $mpoly{$outer} = [ @inner ];
        }

        # turn restrictions
        if ( $routing  &&  $restrictions  &&  $reltag{'type'} eq 'restriction' ) {
            unless ( $relmember{'way:from'} ) {
                print "; ERROR: Turn restriction RelID=$relid doesn't have FROM way\n";
                next;
            }
            if ( $relmember{'way:via'} ) {
                print "; WARNING: VIA ways is still not supported (RelID=$relid)\n";
                next;
            }
            unless ( $relmember{'node:via'} ) {
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

            my @acc = ( 0,0,0,0,0,1,0,0 );      # foot
            @acc = CalcAccessRules( { map { $_ => 'no' } split( /\s*[,;]\s*/, $reltag{'except'} ) }, \@acc )
                if  exists $reltag{'except'};
            $trest{$relid}->{param} = join q{,}, @acc
                if  any { $_ } @acc;

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

            my $node;
            $node = $relmember{'node:sign'}->[0]            if $relmember{'node:sign'};
            $node = $relmember{'node:intersection'}->[0]    if $relmember{'node:intersection'};
            unless ( $node ) {
                print "; ERROR: Destination sign RelID=$relid doesn't have SIGN or INTERSECTION node\n";
                next;
            }

            my $name = name_from_list( 'destination', \%reltag );
            unless ( $name ) {
                print "; ERROR: Destination sign RelID=$relid doesn't have label tag\n";
                next;
            }

            $countsigns ++;
            for my $from ( @{ $relmember{'way:from'} } ) {
                $trest{$relid} = { 
                    name    => $name,
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

        # transport stops
        if ( $transportstops
                &&  $reltag{'type'} eq 'route'  
                &&  $reltag{'route'} ~~ [ qw{ bus } ]  
                &&  exists $reltag{'ref'}  ) {
            $countroutes ++;
            for my $role ( keys %relmember ) {
                next unless $role =~ /^node:.*stop/;
                for my $stop ( @{ $relmember{$role} } ) {
                    push @{ $trstop{$stop} }, $reltag{'ref'};
                }
            }
        }

        # road refs
        if ( $roadshields
                &&  $reltag{'type'}  eq 'route'  
                &&  $reltag{'route'} eq 'road'
                &&  ( exists $reltag{'ref'}  ||  exists $reltag{'int_ref'} )  ) {
            $count_ref_roads ++;
            for my $role ( keys %relmember ) {
                next unless $role =~ /^way:/;
                for my $way ( @{ $relmember{$role} } ) {
                    push @{ $road_ref{$way} }, $reltag{'ref'}       if exists $reltag{'ref'};
                    push @{ $road_ref{$way} }, $reltag{'int_ref'}   if exists $reltag{'int_ref'};
                }
            }
        }

        # streets
        if ( $streetrelations
                &&  $reltag{'type'}  ~~ [ qw{ street associatedStreet } ]
                &&  name_from_list( 'street', \%reltag ) ) {
            $count_streets ++;
            my $street_name = name_from_list( 'street', \%reltag );
            for my $role ( keys %relmember ) {
                next unless $role =~ /:(house|address)/;
                my ($obj) = $role =~ /(.+):/;
                for my $member ( @{ $relmember{$role} } ) {
                    $street{ "$obj:$member" } = $street_name;
                }
            }
        }

    }
}

printf STDERR "%d multipolygons\n", scalar keys %ampoly;
print  STDERR "                          $counttrest turn restrictions\n"       if $restrictions;
print  STDERR "                          $countsigns destination signs\n"       if $destsigns;
print  STDERR "                          $countroutes transport routes\n"       if $transportstops;
print  STDERR "                          $count_ref_roads numbered roads\n"     if $roadshields;
print  STDERR "                          $count_streets streets\n"              if $streetrelations;




####    2nd pass
###     loading cities, multipolygon parts and checking node dupes


my %ways_to_load;
for my $mp ( values %ampoly ) {
    if ( $mp->{outer} ) {
        for my $id ( @{ $mp->{outer} } ) {
            $ways_to_load{$id} ++;
        }
    }
    if ( $mp->{inner} ) {
        for my $id ( @{ $mp->{inner} } ) {
            $ways_to_load{$id} ++;
        }
    }
}


print STDERR "Loading necessary ways... ";

my $wayid;
my %waytag;
my @chain;
my $dupcount;

seek IN, $waypos, 0;

while ( my $line = decode 'utf8', <IN> ) {

    if ( $line =~/<way / ) {
        ($wayid)  = $line =~ / id=["']([^"']+)["']/;
        @chain    = ();
        %waytag   = ();
        $dupcount = 0;
        next;
    }

    if ( $line =~ /<nd / ) {
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
        $waytag{$1} = $2        unless exists $config{skip_tags}->{$1};
        next;
    }


    if ( $line =~ /<\/way/ ) {

        ##      part of multipolygon
        if ( $ways_to_load{$wayid} ) {
            $waychain{$wayid} = [ @chain ];
        }

        ##      address bound
        process_config( $config{address}, {
                type    => 'Way',
                id      => $wayid,
                tag     => { %waytag },
                outer   => [ [ @chain ] ],
            } )
            if $addressing && exists $config{address};

        next;
    }

    last  if $line =~ /<relation/;
}

printf STDERR "%d loaded\n", scalar keys %waychain;

undef %ways_to_load;




print STDERR "Processing multipolygons  ";

print "\n\n\n; ### Multipolygons\n\n";

# load addressing polygons
if ( $addressing && exists $config{address} ) {
    while ( my ( $mpid, $mp ) = each %ampoly ) {
        my $ampoly = merge_ampoly( $mpid );
        next unless exists $ampoly->{outer} && @{ $ampoly->{outer} };
        process_config( $config{address}, {
                type    => 'Rel',
                id      => $mpid,
                tag     => $mp->{tags},
                outer   => $ampoly->{outer},
            } );
    }
}

# draw that should be drawn
my $countpolygons = 0;
while ( my ( $mpid, $mp ) = each %ampoly ) {

    my $ampoly = merge_ampoly( $mpid );
    next unless exists $ampoly->{outer} && @{ $ampoly->{outer} };

    ## POI
    if ( $makepoi ) {
        process_config( $config{nodes}, {
                type    => "Rel",
                id      => $mpid,
                tag     => $mp->{tags},
                latlon  => ( join q{,}, centroid( map { [ split q{,}, $node{$_} ] } @{ $ampoly->{outer}->[0] } ) ),
            } );
    }

    ## Polygon
    my @alist;
    for my $area ( @{ $ampoly->{outer} } ) {
        push @alist, [ map { [reverse split q{,}, $node{$_}] } @$area ];
    }
    my @hlist;
    for my $area ( @{ $ampoly->{inner} } ) {
        push @hlist, [ map { [reverse split q{,}, $node{$_}] } @$area ];
    }

    process_config( $config{ways}, {
            type    => "Rel",
            id      => $mpid,
            tag     => $mp->{tags},
            areas   => \@alist,
            holes   => \@hlist,
        } );
}

printf STDERR "%d polygons written\n", $countpolygons;
printf STDERR "                          %d cities and %d suburbs loaded\n", scalar keys %city, scalar keys %suburb
    if $addressing;





####    3rd pass
###     loading and writing points

my %barrier;
my %xnode;
my %entrance;


print STDERR "Processing nodes...       ";

print "\n\n\n; ### Points\n\n";

my $countpoi = 0;
my $nodeid;
my %nodetag;

seek IN, 0, 0;

while ( my $line = decode 'utf8', <IN> ) {

    if ( $line =~ /<node/ ) {
        ($nodeid)  =  $line =~ / id=["']([^"']+)["']/;
        %nodetag   =  ();
        next;
    }

    if ( $line =~ /<tag/ ) {
        my ($key, undef, $val)  =  $line =~ / k=["']([^"']+)["'].* v=(["'])(.+)\2/;
        $nodetag{$key}   =  $val        unless exists $config{skip_tags}->{$key};
        next;
    }

    if ( $line =~ /<\/node/ ) {

        next unless scalar %nodetag;

        ##  Barriers
        if ( $routing  &&  $barriers  &&  $nodetag{'barrier'} ) {
            AddBarrier({ nodeid => $nodeid,  tags => \%nodetag });
        }

        ##  Forced external nodes
        if ( $routing  &&  exists $nodetag{'garmin:extnode'}  &&  $yesno{$nodetag{'garmin:extnode'}} ) {
            $xnode{$nodeid} = 1;
        }

        ##  Building entrances
        if ( $navitel  &&  exists $nodetag{'building'}  &&  $nodetag{'building'} eq 'entrance' ) {
            $entrance{$nodeid} = name_from_list( 'entrance', \%nodetag);
        }

        ##  POI
        process_config( $config{nodes}, {
                type    => 'Node',
                id      => $nodeid,
                tag     => \%nodetag,
            } );

    }

    last  if  $line =~ /<way/;
}

printf STDERR "%d POIs written\n", $countpoi;
printf STDERR "                          %d POIs loaded\n", sum map { scalar @$_ } values %poi
    if $addrfrompoly;
printf STDERR "                          %d barriers loaded\n", scalar keys %barrier
    if $barriers;




####    Loading roads and coastlines, and writing other ways

my %road;
my %coast;
my %hlevel;

print STDERR "Processing ways...        ";

print "\n\n\n; ### Lines and polygons\n\n";

my $countlines  = 0;
$countpolygons  = 0;

my $city;
my @chainlist;
my $inbounds;

seek IN, $waypos, 0;

while ( my $line = decode 'utf8', <IN> ) {

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
        my ($key, undef, $val)  =  $line =~ / k=["']([^"']+)["'].* v=(["'])(.+)\2/;
        $waytag{$key} = $val        unless exists $config{skip_tags}->{$key};
        next;
    }

    if ( $line =~ /<\/way/ ) {

        my $name = name_from_list( 'label', \%waytag);

        @chainlist = (0)            unless $bounds;
        push @chainlist, $#chain    unless ($#chainlist % 2);

        if ( scalar @chain < 2 ) {
            print "; ERROR: WayID=$wayid has too few nodes at ($node{$chain[0]})\n";
            next;
        }

        next unless scalar keys %waytag;
        next unless scalar @chainlist;

        my @list = @chainlist;
        my @clist = ();
        push @clist, [ (shift @list), (shift @list) ]  while @list;

        ## Way config
        process_config( $config{ways}, {
                type    => "Way",
                id      => $wayid,
                chain   => \@chain,
                clist   => \@clist,
                tag     => \%waytag,
            } );

        ## POI config
        if ( $makepoi ) {
            process_config( $config{nodes}, {
                    type    => "Way",
                    id      => $wayid,
                    latlon  => ( join q{,}, centroid( map { [ split q{,}, $node{$_} ] } @chain ) ),
                    tag     => \%waytag,
                } );
        }
    } # </way>

    last  if $line =~ /<relation/;
}

print  STDERR "$countlines lines and $countpolygons polygons dumped\n";
printf STDERR "                          %d roads loaded\n",      scalar keys %road     if  $routing;
printf STDERR "                          %d coastlines loaded\n", scalar keys %coast    if  $shorelines;

undef %waychain;


####    Writing non-addressed POIs

if ( %poi ) {
    print "\n\n\n; ### Non-addressed POIs\n\n";
    while ( my ($id,$list) = each %poi ) {
        for my $poi ( @$list ) {
            WritePOI( $poi );
        }
    }
    undef %poi;
}



####    Processing coastlines

if ( $shorelines ) {

    my $boundcross = 0;

    print "\n\n\n; ### Sea areas generated from coastlines\n\n";
    print STDERR "Processing shorelines...  ";


    ##  merging
    my @keys = keys %coast;
    for my $line_start ( @keys ) {
        next  unless  $coast{ $line_start };

        my $line_end = $coast{ $line_start }->[-1];
        next  if  $line_end eq $line_start;
        next  unless  $coast{ $line_end };
        next  unless  ( !$bounds  ||  is_inside_bounds( $node{$line_end} ) );

        pop  @{$coast{$line_start}};
        push @{$coast{$line_start}}, @{$coast{$line_end}};
        delete $coast{$line_end};
        redo;
    }


    ##  tracing bounds
    if ( $bounds ) {

        my @tbound;
        my $pos = 0;

        for my $i ( 0 .. $#bound-1 ) {

            push @tbound, {
                type    =>  'bound', 
                point   =>  $bound[$i], 
                pos     =>  $pos,
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
                    delete $coast{$tmp->{line}};
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

    while ( my ($loop,$chain_ref) = each %coast ) {
    
        if ( $chain_ref->[0] ne $chain_ref->[-1] ) {

            printf "; %s: Possible coastline break at (%s) or (%s)\n\n", 
                    ( $bounds ? 'ERROR' : 'WARNING' ), 
                    @node{ @$chain_ref[0,-1] }
                unless  $#$chain_ref < 3;

#            print  "; merged coastline $loop\n";
#            print  "[POLYLINE]\n";
#            print  "Type=$config{types}->{coastline}->{type}\n";
#            print  "EndLevel=$config{types}->{coastline}->{endlevel}\n";
#            printf "Data0=(%s)\n",          join (q{),(}, @node{ @$chain_ref });
#            print  "[END]\n\n\n";

            next;
        }

        # filter huge polygons to avoid cgpsmapper's crash
        if ( $hugesea && scalar @$chain_ref > $hugesea ) {
            printf "; WARNING: skipped too big coastline $loop (%d nodes)\n", scalar @$chain_ref;
            next;
        }

        if ( Math::Polygon->new( map { [ split q{,}, $node{$_} ] } @$chain_ref )->isClockwise() ) {
            $island{$loop} = 1;
        } 
        else {
            $lake{$loop} = Math::Polygon::Tree->new( [ map { [ reverse split q{,}, $node{$_} ] } @$chain_ref ] );
        }
    }

    my @lakesort = sort { scalar @{$coast{$b}} <=> scalar @{$coast{$a}} } keys %lake;

    ##  adding sea background
    if ( $waterback && $bounds && !$boundcross ) {
        $lake{'background'} = $boundtree;
        splice @lakesort, 0, 0, 'background';
    }

    ##  writing
    my $countislands = 0;

    for my $sea ( @lakesort ) {
        my %objinfo = ( 
                type    => $config{types}->{sea}->{type},
                level_h => $config{types}->{sea}->{endlevel},
                comment => "sea $sea",
                areas   => $sea eq 'background'
                    ?  [ \@bound ]
                    :  [[ map { [ reverse split q{,} ] } @node{@{$coast{$sea}}} ]],
            );

        for my $island  ( keys %island ) {
            if ( $lake{$sea}->contains( [ reverse split q{,}, $node{$island} ] ) ) {
                $countislands ++;
                push @{$objinfo{holes}}, [ map { [ reverse split q{,} ] } @node{@{$coast{$island}}} ];
                delete $island{$island};
            }
        }

        WritePolygon( \%objinfo );
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

    print "\n\n\n; ### Roads\n\n";

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
        print STDERR "Merging roads...          ";
    
        my $countmerg = 0;
        my @keys = keys %road;
    
        my $i = 0;
        while ($i < scalar @keys) {
            
            my $r1 = $keys[$i];

            unless ( exists $road{$r1} ) {
                $i++;
                next;
            }

            my $p1 = $road{$r1}->{chain};
    
            my @list = ();
            for my $r2 ( keys %{$rstart{$p1->[-1]}} ) {
                my @plist = qw{ type name city rp level_l level_h };
                push @plist, grep { /^_*[A-Z]/ } ( keys %{$road{$r1}}, keys %{$road{$r2}} );
                if ( $r1 ne $r2  
                  && all { exists $road{$r1}->{$_} && exists $road{$r2}->{$_} && $road{$r1}->{$_} eq $road{$r2}->{$_} } @plist
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
        }
    }

    my $nodcount = 1;

    for my $node ( keys %rnode ) {
        $nodid{$node} = $nodcount++
            if  $rnode{$node} > 1
                ||  $enode{$node}
                ||  $xnode{$node}
                ||  $barrier{$node}
                ||  ( exists $nodetr{$node}  &&  scalar @{$nodetr{$node}} );
    }


    while (my ($roadid, $road) = each %road) {
        for my $node (@{$road->{chain}}) {
            push @{$nodeways{$node}}, $roadid       if  $nodid{$node};
        }
    }


    undef %rnode;

    printf STDERR "%d found\n", scalar keys %nodid;





    ###    detecting duplicate road segments


    if ( $detectdupes ) {

        my %segway;
    
        print STDERR "Detecting duplicates...   ";
        print "\n\n\n";
        
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
            printf "; ERROR: Roads $road have $roadseg{$road} duplicate segments near ($roadpos{$road})\n";
        }
    
        printf STDERR "$countdupsegs segments, %d roads\n", scalar keys %roadseg;
    }




    ####    fixing self-intersections and long roads

    if ( $splitroads ) {

        print STDERR "Splitting roads...        ";
        print "\n\n\n";
        
        my $countself = 0;
        my $countlong = 0;
        my $countrest = 0;
        
        while ( my ($roadid, $road) = each %road ) {
            my $break   = 0;
            my @breaks  = ();
            my $rnod    = 1;
            my $prev    = 0;

            #   test for split conditions
            for my $i ( 1 .. $#{$road->{chain}} ) {
                my $cnode = $road->{chain}->[$i];
                $rnod ++    if  $nodid{ $cnode };

                if ( any { $_ eq $cnode } @{$road->{chain}}[$break..$i-1] ) {
                    $countself ++;
                    if ( $cnode ne $road->{chain}->[$prev] ) {
                        $break = $prev;
                        push @breaks, $break;
                    } else {
                        $break = ($i + $prev) >> 1;
                        push @breaks, $break;

                        my $bnode = $road->{chain}->[$break];
                        $nodid{ $bnode }  =  $nodcount++;
                        $nodeways{ $bnode } = [ $roadid ];
                        printf "; FIX: Added NodID=%d for NodeID=%s at (%s)\n", 
                            $nodid{ $bnode },
                            $bnode,
                            $node{ $bnode };
                    }
                    $rnod = 2;
                }

                elsif ( $rnod == $maxroadnodes ) {
                    $countlong ++;
                    $break = $prev;
                    push @breaks, $break;
                    $rnod = 2;
                }

                elsif ( $i < $#{$road->{chain}}  &&  exists $barrier{ $cnode } ) {
                    # ||  (exists $nodetr{ $cnode }  &&  @{ $nodetr{ $cnode } } ) ) {
                    $countrest ++;
                    $break = $i;
                    push @breaks, $break;
                    $rnod = 1;
                }

                $prev = $i      if  $nodid{ $cnode };
            }



            #   split
            if ( @breaks ) {
                printf "; FIX: WayID=$roadid is splitted at %s\n", join( q{, }, @breaks );
                push @breaks, $#{$road->{chain}};

                for my $i ( 0 .. $#breaks - 1 ) {
                    my $id = $roadid.'/'.($i+1);
                    printf "; FIX: Added road %s, nodes from %d to %d\n", $id, $breaks[$i], $breaks[$i+1];
                    
                    $road{$id} = { %{$road{$roadid}} };
                    $road{$id}->{chain} = [ @{$road->{chain}}[$breaks[$i] .. $breaks[$i+1]] ];

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
                for my $nod ( @{ $road->{chain} } ) {
                    next unless exists $nodeways{$nod};
                    $nodeways{$nod} = [ grep { $_ ne $roadid } @{$nodeways{$nod}} ];
                }
                for my $nod ( @{ $road->{chain} }[ 0 .. $breaks[0] ] ) {
                    next unless exists $nodeways{$nod};
                    push @{ $nodeways{$nod} }, $roadid;
                }

                $#{$road->{chain}} = $breaks[0];
            }
        }
        print STDERR "$countself self-intersections, $countlong long roads, $countrest barriers\n";
    }


    ####    disable U-turns
    if ( $disableuturns ) {

        print STDERR "Removing U-turns...       ";
      
        my $utcount  = 0;
        
        for my $node ( keys %nodid ) {
            next  if $barrier{$node};
        
            # RouteParams=speed,class,oneway,toll,emergency,delivery,car,bus,taxi,foot,bike,truck
            my @auto_links = 
                map { $node eq $road{$_}->{chain}->[0] || $node eq $road{$_}->{chain}->[-1]  ?  ($_)  :  ($_,$_) } 
                    grep { $road{$_}->{rp} =~ /^.,.,.,.,.,.,0/ } @{ $nodeways{$node} };
        
            next  unless scalar @auto_links == 2;
            next  unless scalar( grep { $road{$_}->{rp} =~ /^.,.,0/ } @auto_links ) == 2;

            my $pos = first_index { $_ eq $node } @{ $road{$auto_links[0]}->{chain} };
            $trest{ 'ut'.$utcount++ } = { 
                node    => $node,
                type    => 'no',
                fr_way  => $auto_links[0],
                fr_dir  => $pos > 0  ?   1  :  -1,
                fr_pos  => $pos,
                to_way  => $auto_links[0],
                to_dir  => $pos > 0  ?  -1  :   1,
                to_pos  => $pos,
                param   => '0,0,0,0,0,1,0,0',
            };
            
            $pos = first_index { $_ eq $node } @{ $road{$auto_links[1]}->{chain} };
            $trest{ 'ut'.$utcount++ } = { 
                node    => $node,
                type    => 'no',
                fr_way  => $auto_links[1],
                fr_dir  => $pos < $#{ $road{$auto_links[1]}->{chain} }  ?  -1  :  1,
                fr_pos  => $pos,
                to_way  => $auto_links[1],
                to_dir  => $pos < $#{ $road{$auto_links[1]}->{chain} }  ?   1  : -1,
                to_pos  => $pos,
                param   => '0,0,0,0,0,1,0,0',
            };
            
        }
        print STDERR "$utcount restrictions added\n";
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
                    print "; ERROR: Too close nodes $cnode and $node, WayID=$roadid near (${node{$node}})\n";
                }
                $cnode = $node;
            }
        }
        print STDERR "$countclose pairs fixed\n";
    }




    ###    dumping roads


    print STDERR "Writing roads...          ";

    my $roadcount = 1;
    
    while ( my ($roadid, $road) = each %road ) {

        my ($name, $rp) = ( $road->{name}, $road->{rp} );
        my ($type, $llev, $hlev) = ( $road->{type}, $road->{level_l}, $road->{level_h} );
        
        $roadid{$roadid} = $roadcount++;

        $rp =~ s/^(.,.),./$1,0/     unless $oneway;

        my %objinfo = (
                comment     => "WayID = $roadid" . $road->{comment},
                type        => $type,
                name        => $name,
                chain       => [ @{$road->{chain}} ],
                roadid      => $roadid{$roadid},
                routeparams => $rp,
            );

        $objinfo{level_l}       = $llev       if $llev > 0;
        $objinfo{level_h}       = $hlev       if $hlev > $llev;

        $objinfo{StreetDesc}    = $name       if $name && $navitel;
        $objinfo{DirIndicator}  = 1           if $rp =~ /^.,.,1/;

        if ( $road->{city} ) {
            my $rcity = $city{$road->{city}};
            $objinfo{CityName}      = $rcity->{name};
            $objinfo{RegionName}    = $rcity->{region}  if $rcity->{region};
            $objinfo{CountryName}   = $rcity->{country} if $rcity->{country};
        } elsif ( $name  &&  $defaultcity ) {
            $objinfo{CityName}      = $defaultcity;
        }

        my @levelchain = ();
        my $prevlevel = 0;
        for my $i ( 0 .. $#{$road->{chain}} ) {
            my $node = $road->{chain}->[$i];

            if ( $interchange3d ) {
                if ( exists $hlevel{ $node } ) {
                    push @levelchain, [ $i-1, 0 ]   if  $i > 0  &&  $prevlevel == 0;
                    push @levelchain, [ $i,   $hlevel{$node} ];
                    $prevlevel = $hlevel{$node};
                }
                else {
                    push @levelchain, [ $i,   0 ]   if  $i > 0  &&  $prevlevel != 0;
                    $prevlevel = 0;
                }
            }

            next unless $nodid{$node};
            push @{$objinfo{nod}}, [ $i, $nodid{$node}, $xnode{$node} ];
        }

        $objinfo{HLevel0} = join( q{,}, map { "($_->[0],$_->[1])" } @levelchain)   if @levelchain;

        # the rest object parameters (capitals!)
        for my $key ( keys %$road ) {
            next unless $key =~ /^_*[A-Z]/;
            $objinfo{$key} = $road->{$key};
        }

        WriteLine( \%objinfo );
    }

    printf STDERR "%d written\n", $roadcount-1;

} # if $routing

####    Background object (?)


if ( $bounds && $background  &&  exists $config{types}->{background} ) {

    print "\n\n\n; ### Background\n\n";

    WritePolygon({
            type    => $config{types}->{background}->{type},
            level_h => $config{types}->{background}->{endlevel},
            areas   => [ \@bound ],
        });
}




####    Writing turn restrictions


if ( $routing && ( $restrictions || $destsigns || $barriers ) ) {

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
print "\n; ### That's all, folks!\n\n";







####    Functions

sub convert_string {            # String

    my $str = shift @_;
#    $str = decode('utf8', $str) unless @_;
    return $str     unless $str;

    
    unless ( $translit ) {
        for my $repl ( keys %cmap ) {
            $str =~ s/$repl/$cmap{$repl}/g;
        }
    }
    
    $str = unidecode($str)      if $translit;
    $str = uc($str)             if $upcase;
    
    $str = encode $codepage, $str;
   
    $str =~ s/\&#(\d+)\;/chr($1)/ge;
    $str =~ s/\&amp\;/\&/gi;
    $str =~ s/\&apos\;/\'/gi;
    $str =~ s/\&quot\;/\"/gi;
    $str =~ s/\&[\d\w]+\;//gi;
   
    $str =~ s/[\?\"\<\>\*]/ /g;
    $str =~ s/[\x00-\x1F]//g;
   
    $str =~ s/^[ \`\'\;\.\,\!\-\+\_]+//;
    $str =~ s/  +/ /g;
    $str =~ s/\s+$//;

    $str =~ s/~\[0X(\w+)\]/~[0x$1]/;
    
    return $str;
}

sub name_from_list {
    my ($list_name, $tag_ref) = @_;

    my $key = first { exists $tag_ref->{$_} } @{$taglist{$list_name}};

    my $name;
    $name = $tag_ref->{$key}            if  $key;
    $name = $country_code{uc $name}     if  $list_name eq 'country'  &&  exists $country_code{uc $name};
    return $name;
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



sub speed_code {
    my ($spd) = @_;
    return 7        if $spd > 120;  # no limit
    return 6        if $spd > 100;  # 110
    return 5        if $spd > 85;   # 90
    return 4        if $spd > 70;   # 80
    return 3        if $spd > 50;   # 60
    return 2        if $spd > 30;   # 40
    return 1        if $spd > 10;   # 20
    return 0;                       # 5
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
        print  "RestrParam=$tr->{param}\n"     if $tr->{param};
        print  "[END-Restrict]\n\n";
    }
}




sub usage  {

    my @onoff = ( "off", "on");

    my $usage = <<"END_USAGE";
Usage:  osm2mp.pl [options] file.osm > file.mp

Available options [defaults]:

 --config <file>           configuration file   [$config]
 --mapid <id>              map id               [$mapid]
 --mapname <name>          map name             [$mapname]

 --codepage <num>          codepage number                   [$codepage]
 --upcase                  convert all labels to upper case  [$onoff[$upcase]]
 --translit                tranliterate labels               [$onoff[$translit]]
 --ttable <file>           character conversion table
 --roadshields             shields with road numbers         [$onoff[$roadshields]]
 --namelist <key>=<list>   comma-separated list of tags to select names
 
 --addressing              use city polygons for addressing  [$onoff[$addressing]]
 --navitel                 write addresses for polygons      [$onoff[$navitel]]
 --addrfrompoly            get POI address from buildings    [$onoff[$addrfrompoly]]
 --makepoi                 create POIs for polygons          [$onoff[$makepoi]]
 --poiregion               write region info for settlements [$onoff[$poiregion]]
 --poicontacts             write contact info for POIs       [$onoff[$poicontacts]]
 --defaultcity <name>      default city for addresses        [$defaultcity]
 --defaultregion <name>            region                    [$defaultregion]
 --defaultcountry <name>           country                   [$defaultcountry]
 --countrylist <file>      replace country code by name

 --routing                 produce routable map                      [$onoff[$routing]]
 --oneway                  enable oneway attribute for roads         [$onoff[$oneway]]
 --mergeroads              merge same ways                           [$onoff[$mergeroads]]
 --mergecos <cosine>       max allowed angle between roads to merge  [$mergecos]
 --splitroads              split long and self-intersecting roads    [$onoff[$splitroads]]
 --maxroadnodes <dist>     maximum number of nodes in road segment   [$maxroadnodes]
 --fixclosenodes           enlarge distance between too close nodes  [$onoff[$fixclosenodes]]
 --fixclosedist <dist>     minimum allowed distance                  [$fixclosedist m]
 --restrictions            process turn restrictions                 [$onoff[$restrictions]]
 --barriers                process barriers                          [$onoff[$barriers]]
 --disableuturns           disable u-turns on nodes with 2 links     [$onoff[$disableuturns]]
 --destsigns               process destination signs                 [$onoff[$destsigns]]
 --detectdupes             detect road duplicates                    [$onoff[$detectdupes]]
 --interchange3d           navitel-style 3D interchanges             [$onoff[$interchange3d]]
 --transport <mode>        single transport mode

 --bbox <bbox>             comma-separated minlon,minlat,maxlon,maxlat
 --osmbbox                 use bounds from .osm                      [$onoff[$osmbbox]]
 --bpoly <poly-file>       use bounding polygon from .poly-file
 --background              create background object                  [$onoff[$background]]

 --shorelines              process shorelines                        [$onoff[$shorelines]]
 --waterback               water background (for island maps)        [$onoff[$waterback]]
 --marine                  process marine data (buoys etc)           [$onoff[$marine]]

You can use no<option> to disable features (i.e --norouting)
END_USAGE

    printf $usage;
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
        my $tlon = ( $_[0]->[0] + $_[$i]->[0] + $_[$i+1]->[0] ) / 3;
        my $tlat = ( $_[0]->[1] + $_[$i]->[1] + $_[$i+1]->[1] ) / 3;

        my $tsq = ( ( $_[$i]  ->[0] - $_[0]->[0] ) * ( $_[$i+1]->[1] - $_[0]->[1] ) 
                  - ( $_[$i+1]->[0] - $_[0]->[0] ) * ( $_[$i]  ->[1] - $_[0]->[1] ) );
        
        $slat += $tlat * $tsq;
        $slon += $tlon * $tsq;
        $ssq  += $tsq;
    }

    if ( $ssq == 0 ) {
        return ( 
            ((min map { $_->[0] } @_) + (max map { $_->[0] } @_)) / 2,
            ((min map { $_->[1] } @_) + (max map { $_->[1] } @_)) / 2 );
    }
    return ( $slon/$ssq , $slat/$ssq );
}




sub FindCity {
    return unless keys %city;
    my @nodes = map { ref( $_ )  ?  [ reverse @$_ ]  :  [ split q{,}, ( exists $node{$_} ? $node{$_} : $_ ) ] } @_;
    
    my @cities = ();
    for my $node ( @nodes ) {
        my @res;
        $city_rtree->query_point( @$node, \@res );
        @cities = ( @cities, @res );
    }

    return first { 
            my $cbound = $city{$_}->{bound};
            all { $cbound->contains( $_ ) } @nodes;
        } uniq @cities;
}

sub FindSuburb {
    return unless keys %suburb;
    my @nodes = map { ref( $_ )  ?  [ reverse @$_ ]  :  [ split q{,}, ( exists $node{$_} ? $node{$_} : $_ ) ] } @_;
    return first { 
            my $cbound = $suburb{$_}->{bound};
            all { $cbound->contains( $_ ) } @nodes;
        } keys %suburb;
}


sub AddPOI {
    if ( $addrfrompoly && exists $_[0]->{nodeid} && exists $_[0]->{add_contacts} ) {
        my $id = $_[0]->{nodeid};
        my @bbox = ( reverse split q{,}, $node{$id} ) x 2;
        push @{$poi{$id}}, $_[0];
        $poi_rtree->insert( $id, @bbox );
    }
    else {
        WritePOI( @_ );
    }
}


sub WritePOI {
    my %param = %{$_[0]};

    my %tag   = exists $param{tags} ? %{$param{tags}} : ();

    return      unless  exists $param{nodeid}  ||  exists $param{latlon}; 
    return      unless  exists $param{type};

    my $llev  =  exists $param{level_l} ? $param{level_l} : 0;
    my $hlev  =  exists $param{level_h} ? $param{level_h} : 0;

    print  "; $param{comment}\n"            if  exists $param{comment};
    while ( my ( $key, $val ) = each %tag ) {
        next unless exists $config{comment}->{$key} && $yesno{$config{comment}->{$key}};
        print encode $codepage, "; $key = $val\n";
    }

    my $data;
    $data = "($node{$param{nodeid}})"    if  exists $param{nodeid};
    $data = "($param{latlon})"           if  exists $param{latlon};
    return unless $data;

    my $label = exists $param{name} ? $param{name} : q{};
    
    if ( exists $param{add_elevation} && exists $tag{'ele'} ) {
        $label .= '~[0x1f]' . $tag{'ele'};
    }
    if ( $transportstops && exists $param{add_stops} ) {
        my @stops;
        @stops = ( @{ $trstop{$param{nodeid}} } )    
            if exists $param{nodeid}  &&  exists $trstop{$param{nodeid}};
        push @stops, split( /\s*[,;]\s*/, $tag{'route_ref'} )   if exists $tag{'route_ref'};
        $label .= q{ (} . join q{,}, uniq( sort { $a <=> $b or $a cmp $b } @stops ) . q{)}   if @stops; 
    }

    print  "[POI]\n";
    print  "Type=$param{type}\n";
    printf "Label=%s\n", convert_string( $label )
        if $label ne q{}  &&  !exists( $param{Label} ); 
    printf "Data%d=$data\n", $llev;
    print  "EndLevel=$hlev\n"       if  $hlev > $llev;

    # region and country - for cities
    if ( $poiregion  &&  $label  &&  $param{add_region} ) {
        my $region  = name_from_list( 'region', $param{tags});
        $region .= q{, }. $tag{'addr:district'}     if exists $tag{'addr:district'};
        printf "RegionName=%s\n", convert_string( $region )     if $region;
        my $country = convert_string( name_from_list( 'country', $param{tags}) );
        printf "CountryName=%s\n", convert_string( $country )   if $country;
    }

    # contact information: address, phone
    if ( $poicontacts  &&  $param{add_contacts} ) {
        my $city;
        $city = $city{ FindCity( $param{nodeid} || $param{latlon} ) };
        if ( $city ) {
            printf "CityName=%s\n", convert_string( $city->{name} );
            printf "RegionName=%s\n", convert_string( $city->{region} )        if  $city->{region};
            printf "CountryName=%s\n", convert_string( $city->{country} )      if  $city->{country};
        }
        elsif ( $defaultcity ) {
            printf "CityName=$defaultcity\n";
        }
                                                            
        my $housenumber = name_from_list( 'house', \%tag );
        $housenumber = $param{housenumber}
            if exists $param{housenumber} && !defined $housenumber;
        printf "HouseNumber=%s\n", convert_string( $housenumber )     if $housenumber;

        my $street = $tag{'addr:street'};
        $street = $param{street}
            if exists $param{street} && !defined $street;
        if ( $street ) {
            my $suburb = FindSuburb( $param{nodeid} || $param{latlon} );
            $street .= qq{ ($suburb{$suburb}->{name})}      if $suburb;
            printf "StreetDesc=%s\n", convert_string( $street );
        }

        printf "Zip=%s\n",          convert_string($tag{'addr:postcode'})   if exists $tag{'addr:postcode'};
        printf "Phone=%s\n",        convert_string($tag{'phone'})           if exists $tag{'phone'};
        printf "WebPage=%s\n",      convert_string($tag{'url'})             if exists $tag{'url'};
        printf "WebPage=%s\n",      convert_string($tag{'website'})         if exists $tag{'website'};
    }

    # marine data
    my %buoy_color = (
        # Region A
        lateral_port                            =>  '0x01',
        lateral_starboard                       =>  '0x02',
        lateral_preferred_channel_port          =>  '0x12',
        lateral_preferred_channel_starboard     =>  '0x11',
        safe_water                              =>  '0x10',
        cardinal_north                          =>  '0x06',
        cardinal_south                          =>  '0x0D',
        cardinal_east                           =>  '0x0E',
        cardinal_west                           =>  '0x0F',
        isolated_danger                         =>  '0x08',
        special_purpose                         =>  '0x03',
        lateral_port_preferred                  =>  '0x12',
        lateral_starboad_preferred              =>  '0x11',
    );
    my %light_color = (
        unlit   =>  0,
        red     =>  1,
        green   =>  2,
        white   =>  3,
        blue    =>  4,
        yellow  =>  5,
        violet  =>  6,
        amber   =>  7,
    );
    my %light_type = (
        fixed       =>  '0x01',
        F           =>  '0x01',
        isophase    =>  '0x02',
        flashing    =>  '0x03',
        Fl          =>  '0x03',
        occulting   =>  '0x03',
        Occ         =>  '0x03',
        Oc          =>  '0x03',
        quick       =>  '0x0C',
        Q           =>  '0x0C',
        # fill
    );

    ## Buoys
    if ( $marine  &&  $param{add_buoy} ) {
        if ( my $buoy_type = ( $tag{'buoy'} or $tag{'beacon'} ) ) {
            print "FoundationColor=$buoy_color{$buoy_type}\n";
        }
        if ( my $buoy_light = ( $tag{'light:colour'} or $tag{'seamark:light:colour'} ) ) {
            print "Light=$light_color{$buoy_light}\n";
        }
        if ( my $light_type = ( $tag{'light:character'} or $tag{'seamark:light:character'} ) ) {
            ( $light_type ) = split /[\(\. ]/, $light_type;
            print "LightType=$light_type{$light_type}\n";
        }
    }

    ## Lights
    if ( $marine  &&  $param{add_light} ) {
        my @sectors = 
            sort { $a->[1] <=> $b->[1] }
                grep { $_->[3] } 
                    map { [ split q{:}, $tag{$_} ] } 
                        grep { /seamark:light:\d/ } keys %tag;
        my $scount = scalar @sectors;
        for my $i ( 0 .. $scount-1 ) {
            if ( $sectors[$i]->[2] != $sectors[($i+1) % $scount]->[1] ) {
                push @sectors, [ 'unlit', $sectors[$i]->[2], $sectors[($i+1) % $scount]->[1], 0 ];
            }
        }
        
        printf "Light=%s\n", join( q{,}, 
            map { sprintf "(%s,%d,$_->[1])", ($light_color{$_->[0]} or '0'), $_->[3]/10 } 
                sort { $a->[1] <=> $b->[1] } @sectors
            );

        my $light_type = ( $tag{'light:character'} or $tag{'seamark:light:character'} or 'isophase' );
        ( $light_type ) = split /[\(\. ]/, $light_type;
        print "LightType=$light_type{$light_type}\n";

        for my $sector ( grep { /seamark:light:\d/ } keys %tag ) {
            print ";;; $sector -> $tag{$sector}\n";
        }
    }

    # other parameters - capital first letter!
    for my $key ( grep { /^_*[A-Z]/ } keys %param ) {
        next    if  $param{$key} eq q{};
        printf "$key=%s\n", convert_string($param{$key}, 1);
    }

    print  "[END]\n\n";
}


sub AddBarrier {
    my %param = %{$_[0]};

    return  unless  exists $param{nodeid};
    return  unless  exists $param{tags};

    my $acc = [ 1,1,1,1,1,1,1,1 ];

    $acc = [ split q{,}, $config{barrier}->{$param{tags}->{'barrier'}} ]
        if exists $config{barrier} 
        && exists $config{barrier}->{$param{tags}->{'barrier'}};

    my @acc = map { 1-$_ } CalcAccessRules( $param{tags}, $acc );
    return  if  all { $_ } @acc;

    $barrier{$param{nodeid}}->{type}  = $param{tags}->{'barrier'};
    $barrier{$param{nodeid}}->{param} = join q{,}, @acc
        if  any { $_ } @acc;
}


sub CalcAccessRules {
    my %tag = %{ $_[0] };
    my @acc = @{ $_[1] };

    return @acc     unless exists $config{transport};

    for my $rule ( @{$config{transport}} ) {
        next unless exists $tag{$rule->{key}};
        next unless exists $yesno{$tag{$rule->{key}}};

        my $val = 1-$yesno{$tag{$rule->{key}}};
        $val = 1-$val   if $rule->{mode} == -1;

        my @rule = split q{,}, $rule->{val};
        for my $i ( 0 .. 7 ) {
            next unless $rule[$i];
            $acc[$i] = $val;
        }
    }
    
    return @acc;
}     


sub WriteLine {

    my %param = %{$_[0]};
    my %tag   = exists $param{tags} ? %{$param{tags}} : ();

    return      unless  exists $param{chain}; 
    return      unless  exists $param{type};

    my $llev  =  exists $param{level_l} ? $param{level_l} : 0;
    my $hlev  =  exists $param{level_h} ? $param{level_h} : 0;

    printf encode $codepage, "; $param{comment}\n"      if  exists $param{comment};
    while ( my ( $key, $val ) = each %tag ) {
        next unless exists $config{comment}->{$key} && $yesno{$config{comment}->{$key}};
        print encode $codepage, "; $key = $val\n";
    }

    print  "[POLYLINE]\n";
    printf "Type=%s\n",         $param{type};
    printf "EndLevel=%d\n",     $hlev           if $hlev > $llev;
    printf "Data%d=(%s)\n",     $llev, join( q{),(}, @node{ @{ $param{chain} } } );

    printf "Label=%s\n", convert_string( $param{name} )
        if !exists $param{Label} && $param{name} ne q{};

    # road data
    printf "RoadID=$param{roadid}\n"            if exists $param{roadid};
    printf "RouteParams=$param{routeparams}\n"  if exists $param{routeparams};
    
    my $nodcount = 0;
    for my $nod ( @{$param{nod}} ) {
        printf "Nod%d=%d,%d,%d\n", $nodcount++, @$nod;
    }
    
    # the rest tags (capitals!)
    for my $key ( sort keys %param ) {
        next unless $key =~ /^_*[A-Z]/;
        next if $param{$key} eq q{};
        printf "$key=%s\n", convert_string( $param{$key} );
    }
    print  "[END]\n\n\n";
}


sub AddRoad {

    my %param = %{$_[0]};
    my %tag   = exists $param{tags} ? %{$param{tags}} : ();

    return      unless  exists $param{chain}; 
    return      unless  exists $param{type};

    my ($orig_id) = $param{id} =~ /^([^:]+)/;
    
    my $llev  =  exists $param{level_l} ? $param{level_l} : 0;
    my $hlev  =  exists $param{level_h} ? $param{level_h} : 0;

    my @rp = split q{,}, $param{routeparams};
    @rp[4..11] = CalcAccessRules( \%tag, [ @rp[4..11] ] );

    # determine city
    my $city = FindCity( 
        $param{chain}->[ floor $#{$param{chain}}/3 ], 
        $param{chain}->[ ceil $#{$param{chain}}*2/3 ] );
    
    # calculate speed class
    if ( $tag{'maxspeed'} > 0 ) {
       $tag{'maxspeed'} *= 1.61      if  $tag{'maxspeed'} =~ /mph$/i;
       $rp[0]  = speed_code( $tag{'maxspeed'} * 0.9 );
    }
    if ( $tag{'maxspeed:practical'} > 0 ) {
       $tag{'maxspeed:practical'} *= 1.61        if  $tag{'maxspeed:practical'} =~ /mph$/i;
       $rp[0]  = speed_code( $tag{'maxspeed:practical'} * 0.9 );
    }
    if ( $tag{'avgspeed'} > 0 ) {
       $tag{'avgspeed'} *= 1.61        if  $tag{'avgspeed'} =~ /mph$/i;
       $rp[0]  = speed_code( $tag{'avgspeed'} );
    }

    # navitel-style 3d interchanges
    if ( $interchange3d && exists $waytag{'layer'} && $waytag{'layer'} != 0 ) {
        my $layer = ( $tag{'layer'}<0 ? $tag{'layer'} : $tag{'layer'} * 2 );
        for my $node ( @{$param{chain}} ) {
            $hlevel{ $node } = $layer;
        }
        $layer = $layer - ( $layer < 0  ?  0  :  1 );
        $hlevel { $param{chain}->[0]  } = $layer;
        $hlevel { $param{chain}->[-1] } = $layer;
    }

    # determine suburb
    if ( $city && $param{name} ) {
        my $suburb;
        if ( exists $tag{'addr:suburb'}) { 
            $suburb = $tag{'addr:suburb'};
        }
        else {
            my $sub_ref = FindSuburb( 
               $param{chain}->[ floor $#{$param{chain}}/3 ], 
                $param{chain}->[ ceil $#{$param{chain}}*2/3 ]
            );
            $suburb = $suburb{$sub_ref}->{name} if $sub_ref;
        }

        $param{name} .= qq{ ($suburb)} if $suburb;
    }

    # road shield
    if ( $roadshields  &&  !$city ) {
        my @ref;
        @ref = @{ $road_ref{$orig_id} }     if exists $road_ref{$orig_id};
        push @ref, $tag{'ref'}              if exists $tag{'ref'};
        push @ref, $tag{'int_ref'}          if exists $tag{'int_ref'};
        
        if ( @ref ) {
            my $ref = join q{,}, sort( uniq( map { s/[\s\-]+//g; split /[,;]/, $_ } @ref ) );
            $param{name} = '~[0x06]' . $ref . ( $param{name} ? q{ } . $param{name} : q{} );
        }
    }

    # load road
    $road{$param{id}} = {
        #comment =>  $param{comment},
        type    =>  $param{type},
        name    =>  $param{name},
        chain   =>  $param{chain},
        level_l =>  $llev,
        level_h =>  $hlev,
        city    =>  $city,
        rp      =>  join( q{,}, @rp ),
    };

    # FIXME: buggy object comment
    while ( my ( $key, $val ) = each %tag ) {
        next unless exists $config{comment}->{$key} && $yesno{$config{comment}->{$key}};
        $road{$param{id}}->{comment} .= "\n; $key = $tag{$key}";
    }

    # the rest object parameters (capitals!)
    for my $key ( keys %param ) {
        next unless $key =~ /^_*[A-Z]/;
        $road{$param{id}}->{$key} = $param{$key};
    }

    # external nodes
    if ( $bounds ) {
        if ( !is_inside_bounds( $node{ $param{chain}->[0] } ) ) {
            $xnode{ $param{chain}->[0] } = 1;
            $xnode{ $param{chain}->[1] } = 1;
        }
        if ( !is_inside_bounds( $node{ $param{chain}->[-1] } ) ) {
            $xnode{ $param{chain}->[-1] } = 1;
            $xnode{ $param{chain}->[-2] } = 1;
        }
    }

    # process associated turn restrictions
    if ( $restrictions  ||  $destsigns ) {
        
        for my $relid ( grep { $trest{$_}->{fr_way} eq $orig_id } @{$nodetr{$param{chain}->[0]}} ) {
            $trest{$relid}->{fr_way} = $param{id};
            $trest{$relid}->{fr_dir} = -1;
            $trest{$relid}->{fr_pos} = 0;
        }
        for my $relid ( grep { $trest{$_}->{to_way} eq $orig_id } @{$nodetr{$param{chain}->[0]}} ) {
            $trest{$relid}->{to_way} = $param{id};
            $trest{$relid}->{to_dir} = 1;
            $trest{$relid}->{to_pos} = 0;
        }

        for my $relid ( grep { $trest{$_}->{fr_way} eq $orig_id } @{$nodetr{$param{chain}->[-1]}} ) {
            $trest{$relid}->{fr_way} = $param{id};
            $trest{$relid}->{fr_dir} = 1;
            $trest{$relid}->{fr_pos} = $#{ $param{chain} };
        }
        for my $relid ( grep { $trest{$_}->{to_way} eq $orig_id } @{$nodetr{$param{chain}->[-1]}} ) {
            $trest{$relid}->{to_way} = $param{id};
            $trest{$relid}->{to_dir} = -1;
            $trest{$relid}->{to_pos} = $#{ $param{chain} };
        }
    }
}


sub WritePolygon {

    my %param = %{$_[0]};

    my %tag   = exists $param{tags} ? %{$param{tags}} : ();

    return  unless  exists $param{areas}; 
    return  unless  @{$param{areas}}; 
    return  unless  exists $param{type};


    #   select endlevel
    my $llev  =  $param{level_l};
    my $hlev  =  $param{level_h};

    if ( ref $hlev ) {
        my $square = sum map { Math::Polygon::Calc::polygon_area( @$_ ) 
                                * cos( [centroid( @{$param{areas}->[0]} )]->[1] / 180 * 3.14159 )
                                * (40000/360)**2 } @{$param{areas}};
        $hlev = $llev + last_index { $square >= $_ } @$hlev;
        return if $hlev < $llev;
        $param{comment} .= "\n; area: $square km2 -> $hlev";
    }


    #   test if inside bounds
    my @inside = map { $bounds ? $boundtree->contains_polygon_rough( $_ ) : 1 } @{$param{areas}};
    return      if all { defined && $_==0 } @inside;

    if ( $bounds  &&  $lessgpc  &&  any { !defined } @inside ) {
        @inside = map { $boundtree->contains_points( @$_ ) } @{$param{areas}};
        return  if all { defined && $_==0 } @inside;
    }

    
    $param{holes} = []      unless $param{holes};
    my @plist = grep { scalar @$_ > 3 } ( @{$param{areas}}, @{$param{holes}} );

    # TODO: filter bad holes

    #   clip
    if ( $bounds  &&  any { !defined } @inside ) {
        my $gpc = new_gpc();

        for my $area ( @{$param{areas}} ) {
            $gpc->add_polygon( $area, 0 );
        }
        for my $hole ( @{$param{holes}} ) {
            $gpc->add_polygon( $hole, 1 );
        }

        $gpc    =  $gpc->clip_to( $boundgpc, 'INTERSECT' );
        @plist  =  sort  { $#{$b} <=> $#{$a} }  $gpc->get_polygons();
    }

    return    unless @plist;

    print  "; $param{comment}\n"            if  exists $param{comment};
    while ( my ( $key, $val ) = each %tag ) {
        next unless exists $config{comment}->{$key} && $yesno{$config{comment}->{$key}};
        print encode $codepage, "; $key = $val\n";
    }

    $countpolygons ++;

    print  "[POLYGON]\n";
    printf "Type=%s\n",        $param{type};
    printf "EndLevel=%d\n",    $hlev    if  $hlev > $llev;
    printf "Label=%s\n", convert_string( $param{name} )
        if !exists $param{Label} && $param{name} ne q{};

    ## Navitel
    if ( $navitel ) {
        my $housenumber = name_from_list( 'house', \%tag );
        my $street = $tag{'addr:street'};
        $street = $street{"way:$wayid"}     if exists $street{"way:$wayid"};

        if ( $housenumber && $street ) {
    
            my $city = $city{ FindCity( $plist[0]->[0] ) };
            my $suburb = FindSuburb( $plist[0]->[0] );
            $street .= qq{ ($suburb{$suburb}->{name})}      if $suburb;

            printf  "HouseNumber=%s\n", convert_string( $housenumber );
            printf  "StreetDesc=%s\n", convert_string( $street );
            if ( $city ) {
                printf "CityName=%s\n",     convert_string( $city->{name} );
                printf "RegionName=%s\n",   convert_string( $city->{region} )      if $city->{region};
                printf "CountryName=%s\n",  convert_string( $city->{country} )     if $city->{country};
            } 
            elsif ( $defaultcity ) {
                print "CityName=$defaultcity\n";
            }
        }

        # entrances
        for my $entr ( @{ $param{entrance} } ) {
            next unless !$bounds || is_inside_bounds( $entr->[0] );
            printf "EntryPoint=(%s),%s\n", $entr->[0], convert_string( $entr->[1] );
        }
    }

    for my $polygon ( @plist ) {
        printf "Data%d=(%s)\n", $llev, join( q{),(}, map {join( q{,}, reverse @{$_} )} @{$polygon} )
            if scalar @{$polygon} > 2;
    }

    ## Rusa - floors
    if ( $tag{'building:levels'} ) {
        printf "Floors=%d\n",  0 + $tag{'building:levels'};
    }
    if ( $tag{'building:height'} ) {
        printf "Floors=%d\n",  3 * $tag{'building:height'};
    }

    for my $key ( keys %param ) {
        next unless $key =~ /^_*[A-Z]/;
        next if $param{$key} eq q{};
        printf "$key=%s\n", convert_string($param{$key});
    }

    print "[END]\n\n\n";
}




####    Config processing

sub condition_matches {
    
    my ($condition, $obj) = @_;


    # tag =/!= value or * 
    if ( my ($key, $neg, $val) =  $condition =~ /(\S+)\s*(!?)=\s*(.+)/ ) {
        return( $neg xor
            ( exists $obj->{tag}->{$key}  
            && ( $val eq q{*} 
                || any { $_ =~ /^($val)$/ } split( /;/, $obj->{tag}->{$key} ) ) ) );
    }

    # and / or
    if ( ref $condition ) {
        if ( exists $condition->{or} ) {
            return any { condition_matches( $_, $obj ) } @{ $condition->{or} };
        }
        if ( exists $condition->{and} ) {
            return all { condition_matches( $_, $obj ) } @{ $condition->{and} };
        }
    }

    # inside_city (smart)
    if ( my ($neg) = $condition =~ /(~?)\s*inside_city/ ) {
        my $res;
        if ( $obj->{type} eq 'Node' ) {
            $res = FindCity( $obj->{id} );
        }
        elsif ( exists $obj->{latlon} ) {
            $res = FindCity( $obj->{latlon} );
        }
        elsif ( $obj->{type} eq 'Way' && exists $obj->{chain} ) {
            $res = FindCity( $obj->{chain}->[ floor $#{$obj->{chain}}/3 ] )
                && FindCity( $obj->{chain}->[ ceil $#{$obj->{chain}}*2/3 ] );
        }
        return( $neg xor $res );
    }

    # named
    if ( my ($neg) = $condition =~ /(~?)\s*named/ ) {
        return( $neg xor name_from_list( 'label', $obj->{tag} ));
    }

    # only_way etc
    if ( my ( $type ) = $condition =~ 'only_(\w+)' ) {
        return (uc $obj->{type}) eq (uc $type);
    }

    # no_way etc
    if ( my ( $type ) = $condition =~ 'no_(\w+)' ) {
        return (uc $obj->{type}) ne (uc $type);
    }
}


sub execute_action {

    my ($action, $obj, $condition) = @_;

    my %param = %{ $action };

    $param{name} = '%label'     unless exists $param{name};
    for my $key ( keys %param ) {
        $param{$key} =~ s/%(\w+)/ name_from_list( $1, $obj->{tag} ) /ge;
    }
    
    $param{region} .= q{, }. $obj->{tag}->{'addr:district'}
        if exists $param{region} && exists $obj->{tag}->{'addr:district'};

    my %objinfo = map { $_ => $param{$_} } grep { /^_*[A-Z]/ } keys %param;

    ##  Load area as city
    if ( $param{type} eq 'load_city' ) {

        if ( !$param{name} ) {
            print "; ERROR: City without name $obj->{type}ID=$obj->{id}\n\n";   
        }
        elsif ( $obj->{outer}->[0]->[0] ne $obj->{outer}->[0]->[-1] ) {
            print "; ERROR: City polygon $obj->{type}ID=$obj->{id} is not closed\n";
        }
        else {
            printf "; Found city: $obj->{type}ID=$obj->{id} - %s [ %s, %s ]\n\n",
                convert_string( $param{name}),
                convert_string( $param{country} ),
                convert_string( $param{region} );
            my $cityid = $obj->{type} . $obj->{id};
            $city{ $cityid } = {
                name        =>  $param{name},
                region      =>  $param{region},
                country     =>  $param{country},
                bound       =>  Math::Polygon::Tree->new( 
                        map { [ map { [ split q{,}, $node{$_} ] } @$_ ] } @{ $obj->{outer} }
                    ),
            };
            $city_rtree->insert( $cityid, ( Math::Polygon::Tree::polygon_bbox(
                map { map { [ split q{,}, $node{$_} ] } @$_ } @{ $obj->{outer} }
            ) ) );
        }
    }

    ##  Load area as suburb
    if ( $param{type} eq 'load_suburb' ) {

        if ( !$param{name} ) {
            print "; ERROR: Suburb without name $obj->{type}ID=$obj->{id}\n\n";   
        }
        elsif ( $obj->{outer}->[0]->[0] ne $obj->{outer}->[0]->[-1] ) {
            print "; ERROR: Suburb polygon $obj->{type}ID=$obj->{id} is not closed\n";
        }
        else {
            printf "; Found suburb: $obj->{type}ID=$obj->{id} - %s\n", convert_string( $param{name} );
            $suburb{ $obj->{type} . $obj->{id} } = {
                name        =>  $param{name},
                bound       =>  Math::Polygon::Tree->new( 
                        map { [ map { [ split q{,}, $node{$_} ] } @$_ ] } @{ $obj->{outer} } 
                    ),
            };

        }
    }

    ##  Write POI
    if ( $param{action} eq 'write_poi' ) {
        my %tag = %{ $obj->{tag} };

        return  unless  !$bounds 
            || $obj->{type} eq 'Node' && is_inside_bounds( $node{$obj->{id}} )
            || exists $obj->{latlon} && is_inside_bounds( $obj->{latlon} );
        #return  if  exists $tag{'layer'} && $tag{'layer'} < -1;

        $countpoi ++;

        %objinfo = ( %objinfo, (
                type        => $action->{type},
                name        => $param{name},
                tags        => \%tag,
                comment     => "$obj->{type}ID = $obj->{id}",
            ));

        $objinfo{nodeid}  = $obj->{id}      if $obj->{type} eq 'Node';
        $objinfo{latlon}  = $obj->{latlon}  if exists $obj->{latlon};
        $objinfo{level_l} = $action->{level_l}      if exists $action->{level_l};
        $objinfo{level_h} = $action->{level_h}      if exists $action->{level_h};

        if ( exists $action->{'city'} ) {
            $objinfo{City}          = 'Y';
            $objinfo{add_region}    = 1;
        }
        if ( exists $action->{'transport'} ) {
            $objinfo{add_stops}     = 1;
        }
        if ( exists $action->{'contacts'} ) {
            $objinfo{add_contacts}  = 1;
        }
        if ( exists $action->{'marine_buoy'} ) {
            $objinfo{add_buoy}      = 1;
        }
        if ( exists $action->{'marine_light'} ) {
            $objinfo{add_light}     = 1;
        }
        if ( exists $action->{'ele'} ) {
            $objinfo{add_elevation} = 1;
        }

        AddPOI ( \%objinfo );
    }

    ##  Load coastline
    if ( $param{action} eq 'load_coastline' && $shorelines ) {
        for my $part ( @{ $obj->{clist} } ) {
            my ($start, $finish) = @$part;
            $coast{$obj->{chain}->[$start]} = [ @{$obj->{chain}}[ $start .. $finish ] ];
        }
    }

    ##  Write line or load road
    if ( $param{action} ~~ [ qw{ write_line load_road modify_road } ] ) {

        %objinfo = ( %objinfo, (
                type        => $action->{type},
                name        => $param{name},
                tags        => $obj->{tag},
                comment     => "$obj->{type}ID = $obj->{id}",
            ));

        for my $option ( qw{ level_l level_h routeparams } ) {
            next unless exists $action->{$option};
            $objinfo{$option} = $action->{$option};
        }

        my $part_no = 0;
        for my $part ( @{ $obj->{clist} } ) {
            my ($start, $finish) = @$part;

            $objinfo{chain} = [ @{$obj->{chain}}[ $start .. $finish ] ];
            $objinfo{id}    = "$obj->{id}:$part_no";
            $part_no ++;

            if ( $routing && $param{action} eq 'load_road' ) {
                AddRoad( \%objinfo );
            }
            elsif ( $routing && $param{action} eq 'modify_road' && exists $road{ $objinfo{id} } ) {
                # reverse
                if ( exists $action->{reverse} ) {
                    $road{ $objinfo{id} }->{chain} = [ reverse @{ $road{ $objinfo{id} }->{chain} } ];
                }
                # routeparams
                if ( exists $action->{routeparams} ) {
                    my @rp  = split q{,}, $road{ $objinfo{id} }->{rp};
                    my @mrp = split q{,}, $action->{routeparams};
                    for my $p ( @rp ) {
                        my $mp = shift @mrp;
                        $p = $mp    if $mp =~ /^\d$/;
                        $p = 1-$p   if $mp eq q{~};
                        $p = $p+$1  if $p < 4 && $mp =~ /^\+(\d)$/;
                        $p = $p-$1  if $p > 0 && $mp =~ /^\-(\d)$/;
                    }
                    $road{ $objinfo{id} }->{rp} = join q{,}, @rp;
                }
                # the rest - just copy
                for my $key ( keys %objinfo ) {
                    next unless $key =~ /^_*[A-Z]/ or any { $key eq $_ } qw{ type level_l level_h };
                    next unless defined $objinfo{$key};
                    $road{ $objinfo{id} }->{$key} = $objinfo{$key};
                }
            }
            elsif ( $param{action} ne 'modify_road' ) {
                $countlines ++;
                WriteLine( \%objinfo );
            }
        }
    }

    ##  Write polygon
    if ( $param{action} eq 'write_polygon' ) {

        %objinfo = ( %objinfo, (
                type        => $action->{type},
                name        => $param{name},
                tags        => $obj->{tag},
                comment     => "$obj->{type}ID = $obj->{id}",
            ));

        $objinfo{level_l} = $action->{level_l}      if exists $action->{level_l};
        $objinfo{level_h} = $action->{level_h}      if exists $action->{level_h};

        $objinfo{areas} = $obj->{areas}     if exists $obj->{areas};
        $objinfo{holes} = $obj->{holes}     if exists $obj->{holes};

        if ( $obj->{type} eq 'Way' ) {
            if ( $obj->{chain}->[0] ne $obj->{chain}->[-1] ) {
                print "; ERROR: Area WayID=$obj->{id} is not closed at ($node{$obj->{chain}->[0]})\n";
                return;
            }

            $objinfo{areas} = [ [ map { [reverse split q{,}, $node{$_}] } @{$obj->{chain}} ] ];
            if ( $mpoly{$obj->{id}} ) {
                $objinfo{comment} .= sprintf "\n; multipolygon with %d holes", scalar @{$mpoly{$obj->{id}}};
                for my $hole ( grep { exists $waychain{$_} } @{$mpoly{$obj->{id}}} ) {
                    push @{$objinfo{holes}}, [ map { [reverse split q{,}, $node{$_}] } @{$waychain{$hole}} ];
                }
            }

            $objinfo{entrance} = [ map { [ $node{$_}, $entrance{$_} ] } grep { exists $entrance{$_} } @{$obj->{chain}} ];
        }

        WritePolygon( \%objinfo );
    }

    ##  Address loaded POI
    if ( $param{action} eq 'address_poi' && exists $obj->{chain} && $obj->{chain}->[0] eq $obj->{chain}->[-1] && exists $poi_rtree->{root} ) {

        my @bbox = Math::Polygon::Calc::polygon_bbox( map {[ reverse split q{,}, $node{$_} ]} @{$obj->{chain}} );
        my @poilist;

        $poi_rtree->query_completely_within_rect( @bbox, \@poilist );
        
        for my $id ( @poilist ) {
            next unless exists $poi{$id};
            next unless Math::Polygon::Tree::polygon_contains_point(
                    [ reverse split q{,}, $node{$id} ],
                    map {[ reverse split q{,}, $node{$_} ]} @{$obj->{chain}}
                );

            my %tag = %{ $obj->{tag} };
            my $housenumber = name_from_list( 'house', \%tag );
            my $street = $tag{'addr:street'};
            $street = $street{"way:$wayid"}     if exists $street{"way:$wayid"};

            for my $poiobj ( @{ $poi{$id} } ) {
                $poiobj->{street} = $street;
                $poiobj->{housenumber} = $housenumber;
                WritePOI( $poiobj );
            }

            delete $poi{$id};
        }
    }
}


sub process_config {
    
    my ($cfg, $obj) = @_;

    CFG:
    for my $cfg_item ( @$cfg ) {
        
        CONDITION:
        for my $cfg_condition ( @{ $cfg_item->{condition} } ) {
            next CFG    unless  condition_matches( $cfg_condition, $obj );
        }
 
        ACTION:
        for my $cfg_action ( @{ $cfg_item->{action} } ) {
            execute_action( $cfg_action, $obj, $cfg_item->{condition} );
        }

        # return;
    }
}


sub merge_ampoly {
    my ($mpid) = @_;
    my $mp = $ampoly{$mpid};

    my %res;

    for my $contour_type ( 'outer', 'inner' ) {
    
        my $list_ref = $mp->{$contour_type};
        my @list = grep { exists $waychain{$_} } @$list_ref;

        LIST:
        while ( @list ) {

            my $id = shift @list;
            my @contour = @{$waychain{$id}};

            CONTOUR:
            while ( 1 ) {
                # closed way
                if ( $contour[0] eq $contour[-1] ) {
                    push @{$res{$contour_type}}, [ @contour ];
                    next LIST;
                }

                my $add = first_index { $contour[-1] eq $waychain{$_}->[0] } @list;
                if ( $add > -1 ) {
                    $id .= ":$list[$add]";
                    pop @contour;
                    push @contour, @{$waychain{$list[$add]}};

                    splice  @list, $add, 1;
                    next CONTOUR;
                }
            
                $add = first_index { $contour[-1] eq $waychain{$_}->[-1] } @list;
                if ( $add > -1 ) {
                    $id .= ":r$list[$add]";
                    pop @contour;
                    push @contour, reverse @{$waychain{$list[$add]}};

                    splice  @list, $add, 1;
                    next CONTOUR;
                }

                printf "; %s Multipolygon's RelID=$mpid part WayID=$id is not closed\n\n",
                    ( ( all { exists $waychain{$_} } @$list_ref )
                        ? "ERROR:"
                        : "WARNING: Incomplete RelID=$mpid. " );
                last CONTOUR;
            }
        }
    }

    return \%res;
}


sub merge_polygon_chains {
    
    my @c1 = @{$_[0]};
    my @c2 = @{$_[1]};

    my %seg = map { join( q{:}, sort ( $c1[$_], $c1[$_+1] ) ) => $_ } ( 0 .. $#c1 - 1 );

    for my $j ( 0 .. scalar $#c2 - 1 ) {
        my $seg = join( q{:}, sort ( $c2[$j], $c2[$j+1] ) );
        if ( exists $seg{$seg} ) {
            my $i = $seg{$seg};

            pop @c1;
            @c1 = @c1[ $i+1 .. $#c1, 0 .. $i ]      if  $i < $#c1;
            @c1 = reverse @c1                       if  $c1[0] ne $c2[$j];

            # merge
            splice @c2, $j, 2, @c1;
            pop @c2;

            # remove jitters
            $i = 0;
            JITTER:
            while ( $i <= $#c2 ) {
                if ( $c2[$i] eq $c2[($i+1) % scalar @c2] ) {
                    splice @c2, $i, 1;
                    $i--    if $i > 0;
                    redo JITTER;
                }
                if ( $c2[$i] eq $c2[($i+2) % scalar @c2] ) {
                    splice @c2, ($i+1) % scalar @c2, 1;
                    $i--    if $i > $#c2;
                    splice @c2, $i, 1;
                    $i--    if $i > 0;
                    redo JITTER;
                }
                $i++;
            }
            push @c2, $c2[0];
            return \@c2;
        }
    }
    return undef;
}


