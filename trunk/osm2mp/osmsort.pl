#!/usr/bin/perl


my $osm;
my %nod;
my %way;
my %rel;

my $obj;

while (<>) {

#    if ( /<osm/ )                       { $osm = "";     $obj = \$osm;}
    if ( /<node.*\bid=['"]([^'"]+)['"]/ )        { $nod{$1} = ""; $obj = \$nod{$1}; }
    if ( /<way.*\bid=['"]([^'"]+)['"]/ )         { $way{$1} = ""; $obj = \$way{$1}; }
    if ( /<relation.*\bid=['"]([^'"]+)['"]/ )    { $rel{$1} = ""; $obj = \$rel{$1}; }
    if ( /<bound/ )                     { next; }
    if ( /<\/?osm/ )                    { next; }
    if ( /<\?xml/ )                     { next; }

    ${$obj} .= $_;
}

print "<?xml version='1.0' standalone='yes'?>\n";
#print $osm;
print "<osm version=\"0.6\" generator=\"osmsort.pl\">\n";
print @nod{sort {$a<=>$b} keys %nod};
print @way{sort {$a<=>$b} keys %way};
print @rel{sort {$a<=>$b} keys %rel};
print "</osm>\n";
