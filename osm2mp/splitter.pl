
use strict;

use POSIX;
use List::Util qw{first max sum};
use Bit::Vector;
use Getopt::Long;


my $MAXNODES    = 500_000_000;      # see current OSM values
my $MAXWAYS     =  50_000_000;
my $MAXRELS     =   1_000_000;


my $mapid           = "65430001";
my $max_tile_nodes  = 800_000;




print STDERR "\n    ---|  OSM tile splitter  v0.2    (c) liosha  2009\n\n";


##  reading command-line options

my $opts_result = GetOptions (
        "mapid=s"       => \$mapid,
        "maxnodes=i"    => \$max_tile_nodes,
    );

my $filename = $ARGV[0];
if (!$filename) {
    print "Usage:\n    splitter.pl [--mapid <mapid>] [--maxnodes <maxnodes>] <osm-file>\n\n";
    exit;
}

open IN, '<', $filename     or die $!;
binmode IN;




##  nodes 1st pass - initialising grid

my $lat_cell = 0.1;        # in degrees
my $lon_cell = 0.2;

my %grid;

my $way_pos = 0;
my $nodeid_min = $MAXNODES;
my $nodeid_max = 0;

my $area_init = {
        count   => 0,
        minlon  => 180,
        minlat  => 90,
        maxlon  => -180,
        maxlat  => -90,
    };

print STDERR "Initialising grid...          ";

while (my $line = <IN>) {
    my ($id, $lat, $lon) = $line =~ /<node.* id=["']([^"']+)["'].* lat=["']([^"']+)["'].* lon=["']([^"']+)["']/;

    last if $line =~ /<way /;
    $way_pos = tell IN;

    next unless $id;

    my $glat = floor( $lat / $lat_cell );
    my $glon = floor( $lon / $lon_cell );
    $grid{$glat}->{$glon} ++;

    $area_init->{minlat} = $lat      if $lat < $area_init->{minlat};
    $area_init->{maxlat} = $lat      if $lat > $area_init->{maxlat};
    $area_init->{minlon} = $lon      if $lon < $area_init->{minlon};
    $area_init->{maxlon} = $lon      if $lon > $area_init->{maxlon};
    $area_init->{count} ++;

    $nodeid_max = $id       if $id > $nodeid_max; 
    $nodeid_min = $id       if $id < $nodeid_min; 
}

printf STDERR "%d nodes -> %d cells\n", $area_init->{count}, sum map { scalar keys %$_ } values %grid;

my $maxcell = max map { max values %$_ } values %grid;
die "Use --maxnodes larger than $maxcell or decrease cell size"
    if $maxcell > $max_tile_nodes;

$area_init->{minlat} -= $lat_cell;
$area_init->{maxlat} += $lat_cell;
$area_init->{minlon} -= $lon_cell;
$area_init->{maxlon} += $lon_cell;




##  splitting

my @areas = ($area_init);
my @tiles = ();

print STDERR "Calculating...                ";

while ( my $area = shift @areas ) {

    if ( $area->{count} <= $max_tile_nodes ) {
        print STDERR '.';
        push @tiles, $area;
        next;
    }

    print STDERR '+';

    # 1 -> horisontal split, 0 -> vertical split
    my $hv = ( ($area->{maxlon}-$area->{minlon}) * cos( ($area->{maxlat}+$area->{minlat})/2 / 180 * 3.14159 )
             > ($area->{maxlat}-$area->{minlat}) );
    
    my $sumlat = 0;
    my $sumlon = 0;
    my $sumnod = 0;


    for my $glat ( grep { $_*$lat_cell >= $area->{minlat}  &&  $_*$lat_cell <= $area->{maxlat} } keys %grid ) {
        for my $glon ( grep { $_*$lon_cell >= $area->{minlon}  &&  $_*$lon_cell <= $area->{maxlon} } keys %{$grid{$glat}} ) {
            my $weight = sqrt( $grid{$glat}->{$glon} );
            $sumlat += $glat * $lat_cell * $weight;
            $sumlon += $glon * $lon_cell * $weight;
            $sumnod += $weight;
        }
    }

    my $avglon  =  $sumlon / $sumnod;
    my $avglat  =  $sumlat / $sumnod;

    # special case!
    $avglon     =  -32      if  $area->{minlon} < -100  &&  $area->{maxlon} > 0;

    my $new_area0 = {
        minlon  => $area->{minlon},
        minlat  => $area->{minlat},
        maxlon  => $hv  ?  $avglon          :  $area->{maxlon},
        maxlat  => $hv  ?  $area->{maxlat}  :  $avglat,
        count   => 0,
    };

    my $new_area1 = {
        minlon  => $hv  ?  $avglon          :  $area->{minlon},
        minlat  => $hv  ?  $area->{minlat}  :  $avglat,
        maxlon  => $area->{maxlon},
        maxlat  => $area->{maxlat},
        count   => 0,
    };

    for my $glat ( grep { $_*$lat_cell >= $area->{minlat}  &&  $_*$lat_cell <= $area->{maxlat} } keys %grid ) {
        for my $glon ( grep { $_*$lon_cell >= $area->{minlon}  &&  $_*$lon_cell <= $area->{maxlon} } keys %{$grid{$glat}} ) {
            if (  $hv  &&  $glon*$lon_cell <= $new_area0->{maxlon}
              || !$hv  &&  $glat*$lat_cell <= $new_area0->{maxlat} ) {
                $new_area0->{count} += $grid{$glat}->{$glon};
            }
            else {
                $new_area1->{count} += $grid{$glat}->{$glon};
            }

        }
    }

    push @areas, $new_area0;
    push @areas, $new_area1;
}

@tiles = sort { $a->{minlon} <=> $b->{minlon}  or  $b->{minlat} <=> $a->{minlat} } @tiles;

printf STDERR " %d tiles\n", scalar @tiles;




print STDERR "Reserving memory...           ";
for my $tile (@tiles) {
#    $tile->{nodes} = Bit::Vector->new($nodeid_max+1);
    $tile->{nodes} = Bit::Vector->new($MAXNODES);
    $tile->{ways}  = Bit::Vector->new($MAXWAYS);
    $tile->{rels}  = Bit::Vector->new($MAXRELS);
}
print STDERR "Ok\n";




##  nodes 2nd pass - loading

print STDERR "Loading nodes...              ";

seek IN, 0, 0;

while (my $line = <IN>) {
    my ($id, $lat, $lon) = $line =~ /<node.* id=["']([^"']+)["'].* lat=["']([^"']+)["'].* lon=["']([^"']+)["']/;
    next unless $id;

    for my $tile (@tiles) {
        if ( $lat >= $tile->{minlat}  &&  $lat <= $tile->{maxlat}
          && $lon >= $tile->{minlon}  &&  $lon <= $tile->{maxlon} ) {
            $tile->{nodes}->Bit_On($id);
            next;   # ???
        }
    }
}

print STDERR "Ok\n";





##  reading ways - 1st pass


seek IN, $way_pos, 0;
my $rel_pos = $way_pos;

my @chain = ();
my $reading_way = 0;
my $way_id;

my $count_ways = 0;

print STDERR "Reading ways...               ";

while ( my $line = <IN> ) {

    if ( my ($id) = $line =~ /<way.* id=["']([^"']+)["']/ ) {
        $way_id = $id;
        $reading_way = 1;
        @chain = ();
        next;
    }

    if ( $reading_way  &&  $line =~ /<\/way/ ) {
        $count_ways ++;
        for my $area (@tiles) {
            if ( first { $area->{nodes}->contains($_) } @chain ) {
                $area->{ways}->Bit_On($way_id);
            }
        }
        
        $reading_way = 0;
        next;
    }

    if ( $reading_way  &&  ( my ($nd) = $line =~ /<nd ref=["']([^"']+)["']/ ) ) {
        push @chain, $nd;
        next;
    }

    last if $line =~ /<relation /;
} 
continue { $rel_pos = tell IN; }

print STDERR "$count_ways processed\n";




##  reading ways - 2nd pass

seek IN, $way_pos, 0;

print STDERR "Redistributing nodes...       ";

while ( my $line = <IN> ) {

    if ( my ($id) = $line =~ /<way.* id=["']([^"']+)["']/ ) {
        $way_id = $id;
        $reading_way = 1;
        next;
    }

    if ( $reading_way  &&  $line =~ /<\/way/ ) {
        $reading_way = 0;
        next;
    }

    if ( $reading_way  &&  ( my ($nd) = $line =~ /<nd ref=["']([^"']+)["']/ ) ) {
        for my $area ( grep { $_->{ways}->contains($way_id) } @tiles ) {
            $area->{nodes}->Bit_On($nd);
        }
        next;
    }

    last if $line =~ /<relation /;
} 

print STDERR "Ok\n";




##  reading relations

seek IN, $rel_pos, 0;
my $reading_rel = 0;
my $rel_id;

my $count_rels = 0;

print STDERR "Reading relations...          ";

while ( my $line = <IN> ) {

    if ( my ($id) = $line =~ /<relation.* id=["']([^"']+)["']/ ) {
        $rel_id = $id;
        $reading_rel = 1;
        next;
    }

    if ( $reading_rel  &&  $line =~ /<\/relation/ ) {
        $count_rels ++;
        $reading_way = 0;
        next;
    }

    if ( $reading_rel  &&  ( my ($type, $ref) = $line =~ /<member type=["']([^"']+)["'].* ref=["']([^"']+)["']/ ) ) {
        for my $area (@tiles) {
            if ( $type eq "node"  &&  $area->{nodes}->contains($ref) 
              || $type eq "way"   &&  $area->{ways}->contains($ref)  ) {
                $area->{rels}->Bit_On($rel_id);
                next;
            }
        }
    }
} 

print STDERR "$count_rels processed\n";




print STDERR "Writing output files...       ";

##  creating output files

for my $area (@tiles) {
     open my $fh, '>', "$mapid.osm";
     $area->{file} = $fh;
     print  $fh "<?xml version='1.0' encoding='UTF-8'?>\n";
     print  $fh "<osm version='0.6' generator='Tile Splitter'>\n";
     printf $fh "  <bound box='%f,%f,%f,%f' origin='http://www.openstreetmap.org/api/0.6'/>\n", 
            $area->{minlat},
            $area->{minlon} < -180  ?  -180  :  $area->{minlon},
            $area->{maxlat},
            $area->{maxlon} >  180  ?   180  :  $area->{maxlon};

     $mapid ++;
}


##  writing results

seek IN, 0, 0;
my $reading_obj = 0;
my $obj_id;
my @write_areas = ();

while ( my $line = <IN> ) {

    if ( my ($obj, $id) = $line =~ /<(node|way|relation).* id=["']([^"']+)["']/ ) {
        $reading_obj = $obj;
        @write_areas = grep {  $obj eq "node"      &&  $_->{nodes}->contains($id)
                            || $obj eq "way"       &&  $_->{ways}->contains($id)
                            || $obj eq "relation"  &&  $_->{rels}->contains($id)  } @tiles;
    }

    if ( $reading_obj ) {
        for my $area (@write_areas) {
            my $fh = $area->{file};
            print $fh $line;
        }
    }

    if ( $reading_obj  &&  $line =~ /<\/$reading_obj/ ) {
        $reading_obj = 0;
    }
} 

##  closing files

for my $area (@tiles) {
     my $fh = $area->{file};
     print $fh "</osm>\n";
     close ($fh);
}


print STDERR "Ok\n";

print STDERR "All done\n";


##  log tiles data

for my $tile (@tiles) {      printf "%d  %d  %d\n", $tile->{nodes}->Norm(), $tile->{ways}->Norm(), $tile->{rels}->Norm();       }


