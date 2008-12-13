

my $osm;
my %nod;
my %way;
my %rel;

my $obj;

while (<>) {

#    if ( /<osm/ )                       { $osm = "";     $obj = \$osm;}
    if ( /<node.*id=\"(\w*)\"/ )        { $nod{$1} = ""; $obj = \$nod{$1}; }
    if ( /<way.*id=\"(\w*)\"/ )         { $way{$1} = ""; $obj = \$way{$1}; }
    if ( /<relation.*id=\"(\w*)\"/ )    { $rel{$1} = ""; $obj = \$rel{$1}; }
    if ( /<\/?osm/ )                    { next; }
    if ( /<\?xml/ )                     { next; }

    ${$obj} .= $_;
}

print "<?xml version='1.0' standalone='yes'?>\n";
#print $osm;
print "<osm version=\"0.5\" generator=\"osmsort.pl\">\n";
print values %nod;
print values %way;
print values %rel;
print "</osm>\n";
