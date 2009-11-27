#!/usr/bin/perl

sub nround {
    my $acc = 24; 
    my ($n, $d) = @_;
    return int( $d + ($n+180) / 360 * 2**$acc );
#    return sprintf "%.4f", $_[0];
}




print STDERR "Loading roads...          ";

open IN, "<", $ARGV[0];
while (<IN>) {
    if ( ($id) = /<way id=["']([^"']+)["']/ ) {
        $wid = $id;
        @chain = ();
        @tags = ();
    }
    if ( ($id) = /<nd ref=["']([^"']+)["']/ ) {
        push @chain, $id;
    }
    if ( $wid && (($tag) = /<tag k=["']([^"']+)["']/) ) {
        push @tags, $tag;
    }
    if ( /<\/way>/ && (grep {/highway/} @tags)) {
        $roads{$wid} = [ @chain ];
        $wid = 0;
    }
}

printf STDERR "%d roads found\n", scalar keys %roads;




print STDERR "Processing routing...     ";

my %ncount;
my %ecount;

for $road (values %roads) {
    map { $ncount{$_}++ } @{$road};
    $ecount{$road->[0]}++;
    $ecount{$road->[-1]}++;
}

printf STDERR "%d cross-nodes, %d end-nodes\n", scalar (grep {$ncount{$_}>1} keys %ncount), scalar keys %ecount;



my %nod;

for ( grep { $ncount{$_}>1 } keys %ncount ) {
    $nod{$_} ++;
}
for ( keys %ecount ) {
    $nod{$_} ++;
}

print STDERR "                          ";
printf STDERR "%d nodes total\n", scalar keys %nod;




print STDERR "Loading necessary nodes...\n";

my %nodes;

seek IN,0,0;
while (<IN>) {
    if ( ($id,$lat,$lon) = /<node id=["']([^"']+)["'].* lat=["']([^"']+)["'] lon=["']([^"']+)["']/ ) {
        $nodes{$id} = [$lat,$lon]       if $nod{$id};
    }
}


print STDERR "Looking for errors...     ";


map {push @{$eps{nround($nodes{$_}->[0]).":".nround($nodes{$_}->[1])}}, $_} keys %nod;

#for $err (keys %eps) {  printf "$err -> [%s]\n", join (", ", @{$eps{$err}});  }

for $err (grep { scalar @{$eps{$_}} > 1 } keys %eps) {
    printf "Error near (%s), NodeIDs %s\n", join(",",@{$nodes{$eps{$err}->[0]}}), join(", ",@{$eps{$err}});
}

printf STDERR "%d found\n", scalar grep { scalar @{$eps{$_}} > 1 } keys %eps;
print STDERR "All done!\n";

