
use strict;

use List::Util qw{first};
use Bit::Vector;
use Getopt::Long;


my $MAXNODES    = 500_000_000;
my $MAXWAYS     =  50_000_000;
my $MAXRELS     =   1_000_000;

my $area_nodes  = 1_500_000;
my $mapid       = "65430001";




print STDERR "\n    ---|  OSM integrity-safe tile splitter  v0.1\n\n";




##  reading command-line options

my $opts_result = GetOptions (
        "mapid=s"       => \$mapid,
        "maxnodes=i"    => \$area_nodes,
    );

my $filename = $ARGV[0];
if (!$filename) {
    print "Usage:\n    splitter.pl [--mapid <mapid>] [--maxnodes <maxnodes>] <osm-file>\n\n";
    exit;
}

open IN, '<', $filename     or die $!;
binmode IN;




##  loading nodes

my %node;
keys %node = 8_000_000;

my $nodeid_min = $MAXNODES;
my $nodeid_max = 0;

print STDERR "Loading nodes...              ";

my $way_pos = 0;

my $area_init = {
        count   => 0,
        minlon  => 180,
        minlat  => 90,
        maxlon  => -180,
        maxlat  => -90,
        sumlat  => 0,
        sumlon  => 0,
        nodes   => Bit::Vector->new($MAXNODES),
    };

while (my $line = <IN>) {
    my ($id, $lat, $lon) = $line =~ /<node.* id=["']([^"']+)["'].* lat=["']([^"']+)["'].* lon=["']([^"']+)["']/;
    next unless $id;

    last if $line =~ /<way /;
    $way_pos = tell IN;

    $nodeid_max = $id       if $id > $nodeid_max; 
    $nodeid_min = $id       if $id < $nodeid_min; 

    $node{$id} = node_pack ($lat, $lon);
    $area_init->{nodes}->Bit_On($id);

    $area_init->{minlat} = $lat      if $lat < $area_init->{minlat};
    $area_init->{maxlat} = $lat      if $lat > $area_init->{maxlat};
    $area_init->{minlon} = $lon      if $lon < $area_init->{minlon};
    $area_init->{maxlon} = $lon      if $lon > $area_init->{maxlon};

    $area_init->{sumlat} += $lat;
    $area_init->{sumlon} += $lon;

    $area_init->{count} ++;
}

my @areas = ();
push @areas, $area_init;

print STDERR "$area_init->{count} loaded\n";
undef $area_init;




##  splitting

print STDERR "Splitting...                  ";

my @areas_ok = ();

while ( my $area = shift @areas ) {
    if ( $area->{count} <= $area_nodes ) {
        $area->{ways} = Bit::Vector->new($MAXWAYS),
        $area->{rels} = Bit::Vector->new($MAXRELS),
        push @areas_ok, $area;
        next;
    }

    my $hv;
    if ( $area->{maxlon}-$area->{minlon} > $area->{maxlat}-$area->{minlat} ) {
        $hv = 0;      # horisontal split
    }
    else {
        $hv = 1;      # vertical split
    }

    print STDERR "+";

    my $new_area0 = {
        minlon  => $area->{minlon},
        minlat  => $area->{minlat},
        maxlon  => $hv  ?  $area->{maxlon}                      :  ($area->{sumlon} / $area->{count}),
        maxlat  => $hv  ?  ($area->{sumlat} / $area->{count})   :  $area->{maxlat},
        sumlat  => 0,
        sumlon  => 0,
        count   => 0,
        nodes   => Bit::Vector->new($MAXNODES),
    };

    my $new_area1 = {
        minlon  => $hv  ?  $area->{minlon}                      :  ($area->{sumlon} / $area->{count}),
        minlat  => $hv  ?  ($area->{sumlat} / $area->{count})   :  $area->{minlat},
        maxlon  => $area->{maxlon},
        maxlat  => $area->{maxlat},
        sumlat  => 0,
        sumlon  => 0,
        count   => 0,
        nodes   => Bit::Vector->new($MAXNODES),
    };

    my $start = $nodeid_min;
    while ( my ($min,$max) = $area->{nodes}->Interval_Scan_inc($start) ) {
        $start = $max+2;
        for my $node ( $min..$max ) {
            my ($lat, $lon) = node_unpack($node{$node});
            if ( $hv  &&  $lat < $new_area0->{maxlat}   or   !$hv  &&  $lon < $new_area0->{maxlon} ) {
                $new_area0->{sumlat} += $lat;
                $new_area0->{sumlon} += $lon;
                $new_area0->{count} ++;
                $new_area0->{nodes}->Bit_On($node);
            }
            else {
                $new_area1->{sumlat} += $lat;
                $new_area1->{sumlon} += $lon;
                $new_area1->{count} ++;
                $new_area1->{nodes}->Bit_On($node);
            }
        }
    }

    push @areas, $new_area0;
    push @areas, $new_area1;
}

printf STDERR " %d tiles\n", scalar @areas_ok;

undef %node;

#for my $tile (@areas_ok) {      print "$tile->{count}\n";       }
#for my $tile (@areas_ok) {      print $tile->{nodes}->Norm()."\n";       }




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
        for my $area (@areas_ok) {
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
        for my $area ( grep { $_->{ways}->contains($way_id) } @areas_ok ) {
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
        for my $area (@areas_ok) {
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

for my $area (@areas_ok) {
     open my $fh, '>', "$mapid.osm";
     $area->{file} = $fh;
     print  $fh "<?xml version='1.0' encoding='UTF-8'?>\n";
     print  $fh "<osm version='0.6' generator='Integrity-safe Tile Splitter'>\n";
     printf $fh "  <bound box='%f,%f,%f,%f' origin='http://www.openstreetmap.org/api/0.6'/>\n", 
            $area->{minlat},
            $area->{minlon},
            $area->{maxlat},
            $area->{maxlon};

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
                            || $obj eq "relation"  &&  $_->{rels}->contains($id)  } @areas_ok;
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

for my $area (@areas_ok) {
     my $fh = $area->{file};
     print $fh "</osm>\n";
     close ($fh);
}


print STDERR "Ok\n";



print STDERR "All done\n";

for my $tile (@areas_ok) {      printf "%d  %d  %d\n", $tile->{nodes}->Norm(), $tile->{ways}->Norm(), $tile->{rels}->Norm();       }




##  Functions

sub node_pack {
    my ($lat, $lon) = @_;
    my $latlon = pack "ll", ($lat/180*2**32, $lon/360*2**32);
    return $latlon;
}

sub node_unpack {
    my ($latlon) = @_;
    my ($xlat, $xlon) = unpack "ll", $latlon;
    return ($xlat*180.0/2**32, $xlon*360/2**32);
}