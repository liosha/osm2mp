#!/usr/bin/perl


my $osm;
my %nod;
my %way;
my %rel;

my $obj;

my ($minlat, $minlon, $maxlat, $maxlon) = (90,180,-90,-180);

while (<>) {

#    if ( /<osm/ )                       { $osm = "";     $obj = \$osm;}
    if ( /<node.*\bid=['"]([^'"]+)['"]/ )        { $nod{$1} = ""; $obj = \$nod{$1}; }
    if ( /<way.*\bid=['"]([^'"]+)['"]/ )         { $way{$1} = ""; $obj = \$way{$1}; }
    if ( /<relation.*\bid=['"]([^'"]+)['"]/ )    { $rel{$1} = ""; $obj = \$rel{$1}; }

    if ( /<bounds?/ )   {
        my ($minlt, $minln, $maxlt, $maxln);
        if ( /\<bounds/ ) {
            ($minlt, $minln, $maxlt, $maxln) 
                = ( /minlat=["'](\-?\d+\.?\d*)["'] minlon=["'](\-?\d+\.?\d*)["'] maxlat=["'](\-?\d+\.?\d*)["'] maxlon=["'](\-?\d+\.?\d*)["']/ );
        } 
        else {
            ($minlt, $minln, $maxlt, $maxln) 
                = ( /box=["'](\-?\d+\.?\d*),(\-?\d+\.?\d*),(\-?\d+\.?\d*),(\-?\d+\.?\d*)["']/ );
        }
        $minlat = $minlt    if $minlt < $minlat;
        $minlon = $minln    if $minln < $minlon;
        $maxlat = $maxlt    if $maxlt > $maxlat;
        $maxlon = $maxln    if $maxln > $maxlon;
        next;
    }

    if ( /<\/?osm/ )                    { next; }
    if ( /<\?xml/ )                     { next; }

    ${$obj} .= $_;
}

print "<?xml version='1.0' standalone='yes'?>\n";
#print $osm;
print "<osm version=\"0.6\" generator=\"osmsort.pl\">\n";
print "  <bound box='$minlat,$minlon,$maxlat,$maxlon' origin='http://www.openstreetmap.org/api/0.6'/>\n"
    if $minlat < $maxlat;
print @nod{sort {$a<=>$b} keys %nod};
print @way{sort {$a<=>$b} keys %way};
print @rel{sort {$a<=>$b} keys %rel};
print "</osm>\n";
