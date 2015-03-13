Scripts for converting configs to new format

## POI ##

```
use YAML;

open CFG, "poi.cfg";
while (<CFG>) {
    next   if (!$_) || /^\s*[\#\;]/;
    chomp;
    my $prio = 0;
    my ($k, $v, $type, $llev, $hlev, $mode) = split /\s+/;

    if ($type) {
        if ($type =~ /(.+),(\d+)/) {
            $type = $1;
            $prio = $2;
        }
        #$poitype{"$k=$v"} = [ $type, $llev, $hlev, $mode, $prio ];

        my %action = (
                '1action'       =>  'write_poi',
                '2type'         =>  $type,
                
            );
        $action{'3level_l'} = $llev     if $llev > 0;
        $action{'4level_h'} = $hlev     if $hlev > 0;
        $action{"$mode"}   = 'yes'      if $mode;

        push @poitype, {
                '1condition'    =>  [ "$k = $v" ],
                '2action'       =>  [ { %action } ],
            }
    }
}
close CFG;

my $cfg = YAML::Dump( 'nodes', \@poitype );
$cfg =~ s/\b[1-9](\w)/$1/g;

open CFG, '>', 'node-common.cfg';
print CFG $cfg;
close CFG;
```

---


## Poly ##

```
use YAML;

my %action_type = (
    l   =>  'write_line',
    s   =>  'load_coastline',
    p   =>  'write_polygon',
    r   =>  'load_road',
);

open CFG, "poly.cfg";
while (<CFG>) {
    next   if (!$_) || /^\s*[\#\;]/;
    chomp;
    my $prio = 0;
    my ($k, $v, $mode, $type, $llev, $hlev, $rp, @p) = split /\s+/;

    next unless $type;
    next if $type eq 'undef';

    ( $type ) = split /,/, $type;
    ( $v, my $city ) = split /\//, $v;

    my @condition = ( "$k = $v" );
    push @condition, "inside_city"     if $city;

    my %action = (
            '1action'       =>  $action_type{$mode},
            '2type'         =>  $type,
            
        );
    $action{'3level_l'} = $llev     if $llev > 0;
    $action{'4level_h'} = $hlev     if $hlev > 0;
    if ( $hlev =~ /\*(.*)/ ) {
        $action{'4level_h'}  = [ split /,/, $1 ];
    }
    $action{'5routeparams'} = $rp   if $rp && $mode eq 'r';

    push @polytype, {
            '1condition'    =>  \@condition,
            '2action'       =>  [ \%action ],
        }
}

close CFG;

my $cfg = YAML::Dump( 'ways', \@polytype );
$cfg =~ s/\b[1-9](\w)/$1/g;

open CFG, '>', 'ways-common-draft.yml';
print CFG $cfg;
close CFG;
```