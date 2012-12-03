#!/usr/bin/perl

##
##  osm2mp.pl - OpenStreetMap to 'polish' format converter
##

# $Id$

##
##  Required packages:
##    * Config::Std
##    * Getopt::Long
##    * YAML
##    * Encode::Locale
##    * List::MoreUtils
##    * Math::Polygon
##    * Math::Polygon::Tree
##    * Math::Geometry::Planar::GPC::Polygon
##    * Tree::R
##    * Geo::Openstreetmap::Parser
##    * Template
##
##  See http://search.cpan.org/ or use PPM (Perl package manager) or CPAN module
##

##
##  Licenced under GPL v2
##


use 5.010;
use strict;
use warnings;
use utf8;
use autodie;

our $VERSION = '1.02';



use Carp;
use FindBin qw{ $Bin };
use lib "$Bin/lib";

use Config::Std;
use Getopt::Long qw{ :config pass_through };
use YAML 0.72;

use File::Spec;
use POSIX;
use Encode;
use Encode::Locale;
use List::Util qw{ first reduce sum min max };
use List::MoreUtils qw{ all notall none any first_index last_index uniq };

use Math::Polygon;
use Math::Geometry::Planar::GPC::Polygon 'new_gpc';
use Math::Polygon::Tree  0.041  qw{ polygon_centroid };
use Tree::R;

use OSM;
use OsmAddress;
use LangSelect;
use FeatureConfig;
use Boundary;
use Coastlines;
use TransportAccess;





print STDERR "\n  ---|   OSM -> MP converter  $VERSION   (c) 2008-2012 liosha, xliosha\@gmail.com\n";



####    Settings

say STDERR "\nLoading configuration...";

my $config_file     = "$Bin/cfg/default.cfg";
my %files;

Encode::Locale::decode_argv();
GetOptions(
    'c|config=s'        => \$config_file,
    'load-settings=s@'  => sub { push @{ $files{settings} }, $_[1] },
    'load-features=s@'  => sub { push @{ $files{features} }, $_[1] }
);

read_config $config_file => my %settings;
my $flags  = $settings{Flags}  // {};
my $values = $settings{Values} // {};




my $bbox;
my $bpolyfile;
my $osmbbox;


####    Global vars

my %entrance;
my %main_entrance;

my %poi;
my $poi_rtree = Tree::R->new();

my $ft_config = FeatureConfig->new(
    actions => {
        load_city               => sub { _load_area( city => @_ ) },
        load_access_area        => \&action_load_access_area,
        load_barrier            => \&action_load_barrier,
        load_building_entrance  => \&action_load_building_entrance,
        write_poi               => \&action_write_poi,
        write_polygon           => \&action_write_polygon,
        write_line              => \&action_write_line,
        address_poi             => \&action_address_poi,
        load_road               => \&action_load_road,
        modify_road             => \&action_modify_road,
        load_coastline          => \&action_load_coastline,
        force_external_node     => \&action_force_external_node,
        load_main_entrance      => \&action_load_main_entrance,
        process_interpolation   => \&action_process_interpolation,
    },
    conditions => {
        named       => \&cond_is_named,
        only_rel    => \&cond_is_multipolygon,
        inside_city => \&cond_is_inside_city,
    },
);


####    Load YAML configuration files

my ($cfgvol, $cfgdir) = File::Spec->splitpath( $config_file );
my @load_items =
    map {
        my $sect = $_;
        my $cfg_sect = $settings{Load}{$sect};
        my $list = ref $cfg_sect ? $cfg_sect : $cfg_sect ? [ $cfg_sect ] : [];
        map {[ $sect => $_ ]}
        (
             ( map { File::Spec->catpath($cfgvol, $cfgdir, $_) } @$list ),
             @{ $files{$sect} || [] },
        )
    } qw/ settings features /;


for my $item ( @load_items ) {
    my ($type, $file) = @$item;
    my %cfgpart = do {
        no warnings 'once';
        local $YAML::LoadCode = 1;
        YAML::LoadFile $file;
    };
    while ( my ( $key, $data ) = each %cfgpart ) {
        $ft_config->add_rules( $key => $data )      if $type ~~ 'features';
        $settings{$key} = $data                     if $type ~~ 'settings';
    }
}

my $addresser = OsmAddress->new(
    $settings{country_name} ? (rename_country => $settings{country_name}) : (),
);

my $calc_access = TransportAccess->new( %settings );



# !!! aliases
my %taglist = %{ $settings{taglist} || {} }; 



####    Initializing writer

my $writer_class = $settings{Writer}->{module}
    or croak 'Writer is undefined';
eval "require $writer_class"
    or croak "Unable to initialize writer $writer_class: $@";

for my $key ( keys %{ $settings{Writer} } ) {
    next if $key !~ /_file$/xms;
    $settings{Writer}->{$key} = File::Spec->catpath($cfgvol, $cfgdir, $settings{Writer}->{$key});
}

my $writer = $writer_class->new( %{ $settings{Writer} }, version => $VERSION );




# Command-line second pass: tuning
GetOptions (
    _get_settings_getopt(),
    $writer->get_getopt(),
    LangSelect->get_getopt(),
    $addresser->get_getopt(),
    
#    'transport=s'       => \$transport_mode,

    'bbox=s'            => \$bbox,
    'bpoly=s'           => \$bpolyfile,
    'osmbbox!'          => \$osmbbox,

    'namelist|taglist=s%' => sub { $taglist{$_[1]} = [ split /[ ,]+/, $_[2] ] },

    # obsolete, for backward compatibility
    'nametaglist=s'     => sub { push @ARGV, '--namelist', "label=$_[1]" },
);


usage() unless (@ARGV);



my $lang_select = LangSelect->new(
    target_lang => $values->{target_lang},
    default_lang => $values->{default_lang},
);




####    Loading OSM data

my $infile = shift @ARGV;
my ($in, $stream_msg) = $infile ~~ '-'
    ? ( *STDIN, 'STDIN' )
    : ( do{ open( my $fh, '<', $infile ); $fh }, "file $infile" );

say STDERR "\nLoading OSM data from $stream_msg...";

my %extra_handlers;
if ($osmbbox) {
    %extra_handlers = (
        bound  => sub {  $bbox = join q{,}, @{[ split /,/, shift()->{attr}->{box} ]}[1,0,3,2]  },
        bounds => sub {  $bbox = join q{,}, @{ shift()->{attr}}{qw/ minlon minlat maxlon maxlat / } },
    );
}

my $osm = OSM->new(
    fh => $in,
    skip_tags => [ keys %{$settings{skip_tags}} ],
    handlers => \%extra_handlers,
);
$osm->merge_multipolygons();

close $in  if $infile ne q{-};

my ($nodes, $chains, $mpoly, $relations) = @$osm{ qw/ nodes chains mpoly relations / };
my ($nodetag, $waytag, $reltag) = @{$osm->{tags}}{ qw/ node way relation / };

printf STDERR "  Nodes: %s/%s\n", scalar keys %$nodes, scalar keys %$nodetag;
printf STDERR "  Ways: %s/%s\n", scalar keys %$chains, scalar(keys %$waytag) - scalar(keys %$mpoly);
printf STDERR "  Relations: %s\n", scalar keys %$reltag;
printf STDERR "  Multipolygons: %s\n", scalar keys %$mpoly;





####    Initializing bounds

my $bound;

if ($bbox || $bpolyfile) {
    my $bounds_msg = $bbox ? 'bbox' : "file $bpolyfile";
    say STDERR "\nInitialising bounds from $bounds_msg...";

    if ($bbox) {
        my ($minlon, $minlat, $maxlon, $maxlat) = split q{,}, $bbox;
        $bound = Boundary->new([ [$minlon,$minlat], [$maxlon,$minlat], [$maxlon,$maxlat], [$minlon,$maxlat], [$minlon,$minlat] ]);
    }
    elsif ($bpolyfile) {
        $bound = Boundary->new( $bpolyfile );
    }

    printf STDERR "  %d segments\n", scalar @{ $bound->get_points() } - 1;
}




##  Load address polygons
if ( $flags->{addressing} ) {
    say STDERR "\nLoading address areas...";
    while ( my ($id, $tags) = each %$waytag ) {
        $ft_config->process( address => {
                type    => 'Way', # !!!
                id      => $id,
                tag     => $tags,
                outer   => ( $mpoly->{$id} ? $mpoly->{$id}->[0] : [ $chains->{$id} ] ),
            } );
    }
    printf STDERR "  %d cities\n", $addresser->{areas}->{city} && $addresser->{areas}->{city}->{_count} // 0;
    printf STDERR "  %d restricted areas\n", $calc_access->{areas}->{_count} // 0;
}



my %road;
my %trest;
my %barrier;
my %xnode;
my %nodetr;
my %hlevel;
my %road_ref;
my %trstop;


##  Process relations

say STDERR "\nProcessing relations...";

if ( $flags->{routing}  &&  $flags->{restrictions} ) {
    while ( my ($relation_id, $members) = each %{ $relations->{restriction} || {} } ) {
        my $tags = $reltag->{$relation_id};

        my ($type) = ($tags->{restriction} // 'no') =~ /^(only | no)/xms;
        next if !$type;

        my $from_member = first { $_->{type} eq 'way' && $_->{role} eq 'from' } @$members;
        next if !$from_member;

        my $to_member = first { $_->{type} eq 'way' && $_->{role} eq 'to' } @$members;
        $to_member //= $from_member  if $tags->{restriction} ~~ 'no_u_turn';
        next if !$to_member;

        my $via_member = first { $_->{type} eq 'node' && $_->{role} eq 'via' } @$members;
        next if !$via_member;

        my %vtags = (
            foot => 'no',   # assume no foot restrictions
            ( $tags->{except} ? (map {( $_ => 'no' )} split /\s* [,;] \s*/x, $tags->{except}) : () ),
        );
        my $acc = $calc_access->get_tag_flags( \%vtags );
        next if all {$_} @$acc;
       
        my $node = $via_member->{ref};
        push @{$nodetr{$node}}, $relation_id;
        $trest{$relation_id} = {
            node    => $node,
            type    => $type,
            fr_way  => $from_member->{ref},
            fr_dir  => 0,
            fr_pos  => -1,
            to_way  => $to_member->{ref},
            to_dir  => 0,
            to_pos  => -1,
        };
        $trest{$relation_id}->{param} = join q{,}, @$acc  if any {$_} @$acc;
    }
    printf STDERR "  %d turn restrictions\n", scalar keys %trest;
}


if ( $flags->{routing} && $flags->{dest_signs} ) {
    my $crossroads_cnt = scalar keys %trest;
    while ( my ($relation_id, $members) = each %{ $relations->{destination_sign} || {} } ) {
        my $tags = $reltag->{$relation_id};

        my $from_member = first { $_->{type} eq 'way' && $_->{role} eq 'from' } @$members;
        next if !$from_member;

        my $to_member = first { $_->{type} eq 'way' && $_->{role} eq 'to' } @$members;
        next if !$to_member;

        my $via_member = first { $_->{type} eq 'node' && $_->{role} ~~ [qw/ sign intersection /] } @$members;
        next if !$via_member;

        my $name = name_from_list( destination => $tags );
        next if !$name;

        my $node = $via_member->{ref};
        $trest{$relation_id} = {
            name    => $name,
            node    => $node,
            type    => 'sign',
            fr_way  => $from_member->{ref},
            fr_dir  => 0,
            fr_pos  => -1,
            to_way  => $to_member->{ref},
            to_dir  => 0,
            to_pos  => -1,
        };
        push @{$nodetr{ $node }}, $relation_id;
    }
    printf STDERR "  %d destination signs\n", scalar(keys %trest) - $crossroads_cnt;
}


if ( $flags->{street_relations} ) {
    my $member_count = 0;
    for my $type ( qw{ street associatedStreet } ) {
        my $list = $relations->{$type};
        next if !$list;

        while ( my ($relation_id, $members) = each %$list ) {

            # EXPERIMENTAL: resolve addr:* roles
            for my $member ( @$members ) {
                my ($type, $ref, $role) = @$member{ qw/ type ref role / };
                next if $role !~ / ^ addr: /xms;

                my $tag_ref = $osm->{tags}->{$type}->{$ref};
                next if !$tag_ref;

                for my $k ( reverse sort keys %$tag_ref ) {    # 'name' before 'addr:*'!
                    (my $nk = $k) =~ s/^ name \b/$role/xms;
                    next if $nk !~ m/ ^ $role \b /xms;
                    $reltag->{$relation_id}->{$nk} = $tag_ref->{$k};
                }
            }

            # house tags: addr:* and street's name* as addr:street*
            my %house_tag;
            for my $k ( reverse sort keys %{$reltag->{$relation_id}} ) {    # 'name' before 'addr:*'!
                (my $nk = $k) =~ s/^ name \b/addr:street/xms;
                next if $nk !~ m/ ^ addr: /xms;
                $house_tag{$nk} = $reltag->{$relation_id}->{$k};
            }

            # street tags: all except 'type'
            my %street_tag = %{$reltag->{$relation_id}};
            delete $street_tag{type};

            # add relation tags to members
            for my $member ( @$members ) {
                $member_count ++;
                my ($type, $ref, $role) = @$member{ qw/ type ref role / };

                my $tag_ref = $osm->{tags}->{$type}->{$ref} ||= {};

                if ( %house_tag && $role ~~ [ qw/ house address / ] ) {
                    %$tag_ref = ( %$tag_ref, %house_tag );
                }
                elsif ( %street_tag && $role ~~ 'street' ) {
                    %$tag_ref = ( %$tag_ref, %street_tag );
                }
            }
        }
    }
    printf STDERR "  %d houses with associated street\n", $member_count;
}

if ( $flags->{road_shields} ) {
    while ( my ($relation_id, $members) = each %{ $relations->{route} || {} } ) {
        my $tags = $reltag->{$relation_id};
        next if !( $tags->{route} ~~ 'road' );

        my @ref = grep {$_} @$tags{'ref', 'int_ref'};
        next if !@ref;

        for my $member ( @$members ) {
            next if $member->{type} ne 'way';
            push @{$road_ref{$member->{ref}}}, @ref;
        }
    }
    printf STDERR "  %d road ways with ref\n", scalar keys %road_ref;
}


if ( $flags->{transport_stops} ) {
    while ( my ($relation_id, $members) = each %{ $relations->{route} || {} } ) {
        my $tags = $reltag->{$relation_id};
        next if !( $tags->{route} ~~ [ qw/ bus / ] );

        my $ref = $tags->{ref};
        next if !$ref;

        for my $member ( @$members ) {
            next if $member->{type} ne 'node';
            next if $member->{role} !~ /stop|platform/x;
            push @{ $trstop{$member->{ref}} }, $ref;
        }
    }
    printf STDERR "  %d transport stops\n", scalar keys %trstop;
}



my $coast = Coastlines->new( $bound ? $bound->get_points() : [] );



##  Process POI nodes

say STDERR "\nProcessing nodes...";

print_section( 'Simple objects' );
while ( my ($id, $tags) = each %$nodetag ) {
    $ft_config->process( nodes => {
            type    => 'Node',
            id      => $id,
            tag     => $tags,
        });
}
my $countpoi = $writer->{_count}->{point} // 0;
printf STDERR "  %d POI written\n", $countpoi;
printf STDERR "  %d POI loaded for addressing\n", scalar keys %poi      if $flags->{addr_from_poly};
printf STDERR "  %d building entrances loaded\n", scalar keys %entrance if $flags->{navitel};
printf STDERR "  %d main entrances loaded\n", scalar keys %main_entrance;


##  Process ways

say STDERR "\nProcessing ways...";

while ( my ($id, $tags) = each %$waytag ) {
    my $objinfo = {
        type    => "Way",
        id      => $id,
        tag     => $tags,
    };

    $ft_config->process( ways  => $objinfo );
    $ft_config->process( nodes => $objinfo )  if $flags->{make_poi};
}
printf STDERR "  %d POI written\n", ($writer->{_count}->{point} // 0) - $countpoi;
printf STDERR "  %d lines written\n", $writer->{_count}->{polyline} // 0;
printf STDERR "  %d polygons written\n", $writer->{_count}->{polygon} // 0;
printf STDERR "  %d roads loaded\n", scalar keys %road                      if $flags->{routing};
printf STDERR "  %d coastlines loaded\n", scalar keys %{$coast->{lines}}    if $flags->{shorelines};


####    Writing non-addressed POIs

if ( %poi ) {
    say STDERR "\nWriting rest POIs...";
    my $count = keys %poi;
    print_section( 'Non-addressed POIs' );
    while ( my ($id,$list) = each %poi ) {
        for my $poi ( @$list ) {
            output_poi( $poi );
        }
    }
    undef %poi;
    printf STDERR "  %d POI written\n", $count;
}



####    Processing coastlines

if ( $flags->{shorelines} ) {
    say STDERR "\nProcessing coastlines...";

    my @sea_areas = $coast->generate_polygons( water_background => !!$flags->{water_back} );

    print_section( 'Sea areas generated from coastlines' );
    for my $sea_poly ( @sea_areas ) {
        my %objinfo = (
            %{ $settings{types}->{sea} },
            areas   => [ shift @$sea_poly ],
            holes   => $sea_poly,
        );
        output_area( \%objinfo, undef, no_clip => 1 );
    }
    printf STDERR "  %d areas\n", scalar @sea_areas;
}





####    Process roads

my %nodid;
my %roadid;
my %nodeways;

if ( $flags->{routing} ) {

    print_section( 'Roads' );

    ###     detecting end nodes

    my %enode;
    my %rstart;

    while ( my ($roadid, $road) = each %road ) {
        $enode{$road->{chain}->[0]}  ++;
        $enode{$road->{chain}->[-1]} ++;
        $rstart{$road->{chain}->[0]}->{$roadid} = 1;
    }



    ###     merging roads

    if ( $flags->{merge_roads} ) {
        print STDERR "Merging roads...          ";

        my $countmerg = 0;
        my @keys = keys %road;

        my $i = 0;
        while ($i < scalar @keys) {

            my $r1 = $keys[$i];

            if ( !exists $road{$r1} ) {
                $i++;
                next;
            }

            my $p1 = $road{$r1}->{chain};

            my @list = ();
            for my $r2 ( keys %{$rstart{$p1->[-1]}} ) {
                my @plist = qw{ type name city rp level_l level_h };
                push @plist, grep { /^_*[A-Z]/ } ( keys %{$road{$r1}}, keys %{$road{$r2}} );

                if ( $r1 ne $r2
                  && ( all {
                        ( !exists $road{$r1}->{$_} && !exists $road{$r2}->{$_} ) ||
                        ( defined $road{$r1}->{$_} && defined $road{$r2}->{$_} && $road{$r1}->{$_} eq $road{$r2}->{$_} )
                    } @plist )
                  && lcos( $p1->[-2], $p1->[-1], $road{$r2}->{chain}->[1] ) > $values->{merge_cos} ) {
                    push @list, $r2;
                }
            }

            # merging
            if ( @list ) {
                $countmerg ++;
                @list  =  sort {  lcos( $p1->[-2], $p1->[-1], $road{$b}->{chain}->[1] )
                              <=> lcos( $p1->[-2], $p1->[-1], $road{$a}->{chain}->[1] )  }  @list;

                report( sprintf( "Road WayID=$r1 may be merged with %s at (%s)", join( q{, }, @list ), $nodes->{$p1->[-1]} ), 'FIX' );

                my $r2 = $list[0];

                # process associated restrictions
                if ( $flags->{restrictions}  ||  $flags->{dest_signs} ) {
                    while ( my ($relid, $tr) = each %trest )  {
                        if ( $tr->{fr_way} eq $r2 )  {
                            my $msg = "RelID=$relid FROM moved from WayID=$r2($tr->{fr_pos})";
                            $tr->{fr_way}  = $r1;
                            $tr->{fr_pos} += $#{$road{$r1}->{chain}};
                            report( "$msg to WayID=$r1($tr->{fr_pos})", 'FIX' );
                        }
                        if ( $tr->{to_way} eq $r2 )  {
                            my $msg = "RelID=$relid TO moved from WayID=$r2($tr->{to_pos})";
                            $tr->{to_way}  = $r1;
                            $tr->{to_pos} += $#{$road{$r1}->{chain}};
                            report( "$msg to WayID=$r1($tr->{to_pos})", 'FIX' );
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

    my $nodcount = $values->{first_nod_id} || 1;

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


    if ( $flags->{detect_dupes} ) {

        my %segway;

        print STDERR "Detecting duplicates...   ";

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
            $roadpos{$roads} = $nodes->{$point};
        }

        for my $road ( keys %roadseg ) {
            report( "Roads $road have $roadseg{$road} duplicate segments near ($roadpos{$road})" );
        }

        printf STDERR "$countdupsegs segments, %d roads\n", scalar keys %roadseg;
    }




    ####    fixing self-intersections and long roads

    if ( $flags->{split_roads} ) {

        print STDERR "Splitting roads...        ";

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
                        report( sprintf( "Added NodID=%d for NodeID=%s at (%s)", $nodid{$bnode}, $bnode, $nodes->{$bnode} ), 'FIX' );
                    }
                    $rnod = 2;
                }

                elsif ( $rnod == $values->{max_road_nodes} // 999 ) {
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
                report( sprintf( "WayID=$roadid is splitted at %s", join( q{, }, @breaks ) ), 'FIX' );
                push @breaks, $#{$road->{chain}};

                for my $i ( 0 .. $#breaks - 1 ) {
                    my $id = $roadid.'/'.($i+1);
                    report( sprintf( "Added road %s, nodes from %d to %d\n", $id, $breaks[$i], $breaks[$i+1] ), 'FIX' );

                    $road{$id} = { %{$road{$roadid}} };
                    $road{$id}->{chain} = [ @{$road->{chain}}[$breaks[$i] .. $breaks[$i+1]] ];

                    #   update nod->road list
                    for my $nod ( grep { exists $nodeways{$_} } @{$road{$id}->{chain}} ) {
                        push @{$nodeways{$nod}}, $id;
                    }

                    #   move restrictions
                    if ( $flags->{restrictions}  ||  $flags->{dest_signs} ) {
                        while ( my ($relid, $tr) = each %trest )  {
                            if (  $tr->{to_way} eq $roadid
                              &&  $tr->{to_pos} >  $breaks[$i]   - (1 + $tr->{to_dir}) / 2
                              &&  $tr->{to_pos} <= $breaks[$i+1] - (1 + $tr->{to_dir}) / 2 ) {
                                my $msg = "Turn restriction RelID=$relid TO moved from $roadid($tr->{to_pos})";
                                $tr->{to_way}  =  $id;
                                $tr->{to_pos}  -= $breaks[$i];
                                report( "$msg to $id($tr->{to_pos})", 'FIX' );
                            }
                            if (  $tr->{fr_way} eq $roadid
                              &&  $tr->{fr_pos} >  $breaks[$i]   + ($tr->{fr_dir} - 1) / 2
                              &&  $tr->{fr_pos} <= $breaks[$i+1] + ($tr->{fr_dir} - 1) / 2 ) {
                                my $msg = "Turn restriction RelID=$relid FROM moved from $roadid($tr->{fr_pos})";
                                $tr->{fr_way} =  $id;
                                $tr->{fr_pos} -= $breaks[$i];
                                report( "$msg to $id($tr->{fr_pos})", 'FIX' );
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
    if ( $flags->{disable_u_turns} ) {

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

    if ( $flags->{fix_close_nodes} ) {

        print STDERR "Fixing close nodes...     ";

        my $countclose = 0;

        while ( my ($roadid, $road) = each %road ) {
            my $cnode = $road->{chain}->[0];
            for my $node ( grep { $_ ne $cnode && $nodid{$_} } @{$road->{chain}}[1..$#{$road->{chain}}] ) {
                if ( fix_close_nodes( $cnode, $node ) ) {
                    $countclose ++;
                    report( "Too close nodes $cnode and $node, WayID=$roadid near ($nodes->{$node})" );
                }
                $cnode = $node;
            }
        }
        print STDERR "$countclose pairs fixed\n";
    }




    ###    dumping roads


    print STDERR "Writing roads...          ";

    my $roadcount = $values->{first_road_id} || 1;

    while ( my ($roadid, $road) = each %road ) {

        my ($name, $rp) = ( $road->{name}, $road->{rp} );
        my ($type, $llev, $hlev) = ( $road->{type}, $road->{level_l}, $road->{level_h} );

        $roadid{$roadid} = $roadcount++;

        $rp =~ s/^(.,.),./$1,0/     unless $flags->{oneway};

        my %objinfo = (
                comment     => "WayID = $roadid" . ( $road->{comment} // q{} ),
                type        => $type,
                name        => $name,
                chain       => [ @{$road->{chain}} ],
                roadid      => $roadid{$roadid},
                routeparams => $rp,
            );

        $objinfo{level_l}       = $llev       if $llev > 0;
        $objinfo{level_h}       = $hlev       if $hlev > $llev;

        $objinfo{DirIndicator}  = 1           if $rp =~ /^.,.,1/;

        if ( $road->{address} ) {
            _hash_merge( \%objinfo, $road->{address} );
        }

        my @levelchain = ();
        my $prevlevel = 0;
        for my $i ( 0 .. $#{$road->{chain}} ) {
            my $node = $road->{chain}->[$i];

            if ( $flags->{interchange_3d} ) {
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

}

####    Background object


if ( $bound && $flags->{background} && $settings{types}->{background} ) {

    my %bound_info = (
        %{ $settings{types}->{background} },
        areas   => [ $bound->get_points() ],
    );
    output_area( \%bound_info, undef, no_clip => 1 );
}




####    Writing turn restrictions


if ( $flags->{routing} && ( $flags->{restrictions} || $flags->{dest_signs} || $flags->{barriers} ) ) {

    print STDERR "Writing crossroads...     ";
    print_section( 'Turn restrictions and signs' );

    my $counttrest = 0;
    my $countsigns = 0;

    while ( my ($relid, $tr) = each %trest ) {

        unless ( $tr->{fr_dir} ) {
            report( "RelID=$relid FROM road does'n have VIA end node" );
            next;
        }
        unless ( $tr->{to_dir} ) {
            report( "RelID=$relid TO road does'n have VIA end node" );
            next;
        }

        $tr->{comment} = "RelID = $relid: from $tr->{fr_way} $tr->{type} $tr->{to_way}";

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
                    type    => 'no',
                );
            for my $key ( qw{ node fr_way fr_dir fr_pos param } ) {
                next unless exists $tr->{$key};
                $newtr{$key} = $tr->{$key};
            }

            for my $roadid ( @{$nodeways{ $trest{$relid}->{node} }} ) {
                $newtr{to_way} = $roadid;
                $newtr{to_pos} = first_index { $_ eq $tr->{node} } @{$road{$roadid}->{chain}};

                if (  $newtr{to_pos} < $#{$road{$roadid}->{chain}}
                  &&  !( $tr->{to_way} eq $roadid  &&  $tr->{to_dir} eq 1 ) ) {
                    $newtr{comment} = "$tr->{comment}\nSo restrict to $roadid forward";
                    $newtr{to_dir} = 1;
                    $counttrest ++;
                    write_turn_restriction (\%newtr);
                }

                if (  $newtr{to_pos} > 0
                  &&  !( $tr->{to_way} eq $roadid  &&  $tr->{to_dir} eq -1 )
                  &&  $road{$roadid}->{rp} !~ /^.,.,1/ ) {
                    $newtr{comment} = "$tr->{comment}\nSo restrict to $roadid backward";
                    $newtr{to_dir} = -1;
                    $counttrest ++;
                    write_turn_restriction (\%newtr);
                }
            }
        }
    }

    ##  Barriers

    print_section( 'Barriers' );

    for my $node ( keys %barrier ) {
        my %newtr = (
            node    => $node,
            type    => 'no',
            param   => $barrier{$node}->{param},
            comment => "NodeID = $node\nbarrier = $barrier{$node}->{type}",
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
                        next    if  $way_from eq $way_to  &&  $dir_from == -$dir_to;

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



print STDERR "\nAll done!!\n\n";

$writer->finalize();
exit 0;


#### The end







####    Functions

sub convert_string {

    my ($str) = @_;
    return q{}     unless $str;

    $str =~ s/[\?\"\<\>\*]/ /g;
    $str =~ s/[\x00-\x1F]//g;

    $str =~ s/^[ \`\'\;\.\,\!\-\+\_]+//;
    $str =~ s/  +/ /g;
    $str =~ s/\s+$//;

    return $str;
}

sub name_from_list {
    my ($list_name, $tags) = @_;

    my $list = $taglist{$list_name} || [ $list_name ];
    for my $base_tag ( @$list ) {
        my $result = $lang_select->get_value($base_tag, $tags);
        return $result  if $result;
    }

    return;
}


sub fix_close_nodes {                # NodeID1, NodeID2

    my ($id0, $id1) = @_;

    my ($lat1, $lon1) = split q{,}, $nodes->{$id0};
    my ($lat2, $lon2) = split q{,}, $nodes->{$id1};

    my ($clat, $clon) = ( ($lat1+$lat2)/2, ($lon1+$lon2)/2 );
    my ($dlat, $dlon) = ( ($lat2-$lat1),   ($lon2-$lon1)   );
    my $klon = cos( $clat * 3.14159 / 180 );

    my $ldist = $values->{fix_close_dist} * 180 / 20_000_000;

    my $res = ($dlat**2 + ($dlon*$klon)**2) < $ldist**2;

    # fixing
    if ( $res ) {
        if ( $dlon == 0 ) {
            $nodes->{$id0} = ($clat - $ldist/2 * ($dlat==0 ? 1 : ($dlat <=> 0) )) . q{,} . $clon;
            $nodes->{$id1} = ($clat + $ldist/2 * ($dlat==0 ? 1 : ($dlat <=> 0) )) . q{,} . $clon;
        }
        else {
            my $azim  = $dlat / $dlon;
            my $ndlon = sqrt( $ldist**2 / ($klon**2 + $azim**2) ) / 2;
            my $ndlat = $ndlon * abs($azim);

            $nodes->{$id0} = ($clat - $ndlat * ($dlat <=> 0)) . q{,} . ($clon - $ndlon * ($dlon <=> 0));
            $nodes->{$id1} = ($clat + $ndlat * ($dlat <=> 0)) . q{,} . ($clon + $ndlon * ($dlon <=> 0));
        }
    }
    return $res;
}



sub lcos {                      # NodeID1, NodeID2, NodeID3

    my ($id0, $id1, $id2) = @_;

    my ($lat1, $lon1) = split q{,}, $nodes->{$id0};
    my ($lat2, $lon2) = split q{,}, $nodes->{$id1};
    my ($lat3, $lon3) = split q{,}, $nodes->{$id2};

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
    my ($node) = @_;
    return 1 if !$bound;
    return $bound->contains( [ reverse split q{,}, $node ] );
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
        $writer->output( comment => { text => "$tr->{comment}\nOutside boundaries" } );
        return;
    }

    my %opts = (
        node_from   => $nodid{ $road{$tr->{fr_way}}->{chain}->[$i] },
        node_via    => $nodid{ $tr->{node} },
        node_to     => $nodid{ $road{$tr->{to_way}}->{chain}->[$j] },
        road_from   => $roadid{ $tr->{fr_way} },
        road_to     => $roadid{$tr->{to_way}},
    );

    if ( $tr->{type} eq 'sign' ) {
        $opts{param} = "T,$tr->{name}";
        $writer->output( destination_sign => { comment => $tr->{comment}, opts => \%opts } );
    }
    else {
        $opts{param} = $tr->{param}    if $tr->{param};
        $writer->output( turn_restriction => { comment => $tr->{comment}, opts => \%opts } );
    }

    return;
}



BEGIN {

my @available_flags = (
    [ routing           => 'produce routable map' ],
    [ oneway            => 'enable oneway attribute for roads' ],
    [ merge_roads       => 'merge same ways' ],
    [ split_roads       => 'split long and self-intersecting roads' ],
    [ fix_close_nodes   => 'enlarge distance between too close nodes' ],
    [ restrictions      => 'process turn restrictions' ],
    [ barriers          => 'create restrictions on barrier nodes' ],
    [ disable_u_turns   => 'disable u-turns on nodes with 2 links' ],
    [ dest_signs        => 'process destination signs' ],
    [ detect_dupes      => 'report road duplicates' ],
    [ road_shields      => 'write shields with road numbers' ],
    [ transport_stops   => 'write route refs on bus stops' ],
    [ street_relations  => 'use street relations for addressing' ],
    [ interchange_3d    => 'navitel-style 3D interchanges' ],
    [ background        => 'create background object' ],
    [ clip_areas        => 'clip polygons to map boundary' ],
    [ shorelines        => 'create sea areas from coastlines' ],
    [ water_back        => 'water background (for island maps)' ],
    [ marine            => 'process marine-specific data' ],
    [ addressing        => 'resolve addresses' ],
    [ navitel           => 'write addresses for house polygons' ],
    [ poi_contacts      => 'write contact info for POIs' ],
    [ addr_from_poly    => 'use building outlines for POI addressing' ],
    [ make_poi          => 'create POIs for polygons' ],
    [ addr_interpolation => 'create address points by interpolation' ],

    [ less_gpc          => undef ],
);

my @available_values = (
    [ merge_cos         => 'max angle between roads to merge (cosine)' ],
    [ max_road_nodes    => 'maximum number of nodes in road' ],
    [ fix_close_dist    => 'minimum allowed routing segment length (m)' ],
    [ target_lang       => 'desired map language' ],
    [ default_lang      => 'source language for default tags' ],

    [ huge_sea          => undef ],
    [ first_nod_id      => undef ],
    [ first_road_id     => undef ],
);    

my @onoff = ( "off", "on");


sub _get_getopt_key {
    my ($opt) = $_;
    my @results = ($opt);
    $opt =~ s/_/-/gx;
    push @results, $opt;
    $opt =~ s/-//gx;
    push @results, $opt;
    return join q{|}, uniq @results;
}


sub _get_settings_getopt {
    return (
        ( map {( _get_getopt_key($_) . q{!}  => \$flags->{$_}  )} map {$_->[0]} @available_flags ),
        ( map {( _get_getopt_key($_) . q{=s} => \$values->{$_} )} map {$_->[0]} @available_values ),
    );
}


sub _get_usage {
    my ($key, $descr, $val) = @_;
    $key =~ s/_/-/gx;
    return sprintf(' --%-23s %-42s', $key, $descr)
        . ( length $val ? sprintf(' [%s]', $val) : q{} )
        . "\n";
}

sub _get_flag_usage {
    my ($flag, $descr) = @_;
    return if !$descr;
    my $val = $onoff[!!$flags->{$flag}];
    return _get_usage($flag, $descr, $val);
}


sub _get_value_usage {
    my ($key, $descr) = @_;
    return if !$descr;
    my $val = $values->{$key};
    return _get_usage($key, $descr, $val);
}



sub usage  {

    my $usage = <<"END_USAGE";

Usage:  osm2mp.pl [options] file.osm

Available options [defaults]:

Configuration:
 --config <file>           main configuration file
 --load-settings <file>    extra settings
 --load-features <file>    extra features

Flags (use --no-<option> to disable):
${\( join q{}, map { _get_flag_usage(@$_) } @available_flags )}
Values:
${\( join q{}, map { _get_value_usage(@$_) } @available_values )}
Boundaries:
 --bpoly <poly-file>       use bounding polygon from .poly-file
 --bbox <bbox>             comma-separated minlon,minlat,maxlon,maxlat
 --osmbbox                 use bounds from .osm

Other options:
 --namelist <key>=<list>   comma-separated list of tags to select names
${\( join q{}, map { _get_usage(@$_) } $addresser->get_usage() )}

Writer options:
${\( join q{}, map { _get_usage(@$_) } $writer->get_usage() )}
Language options:
${\( join q{}, map { _get_usage(@$_) } LangSelect->get_usage() )}

END_USAGE

    print $usage;
    exit 1;
}
}



sub FindCity {
    my @points = map { ref( $_ )  ?  [ reverse @$_ ]  :  [ split q{,}, ( exists $nodes->{$_} ? $nodes->{$_} : $_ ) ] } @_;
    return $addresser->find_area( city => @points );
}



sub output_poi {
    my ($info) = @_;

    while ( my ( $key, $val ) = each %{ $info->{tags} } ) {
        next if !$settings{comment}->{$key}; # ???
        $info->{comment} .= "\n$key = $val";
    }

=disable
    if ( $info->{ele} && $info->{tags}->{ele} ) {
        $label .= '~[0x1f]' . $tag{'ele'};
    }

    if ( $flags->{transport_stops} && exists $param{transport} ) {
        my @stops;
        @stops = ( @{ $trstop{$param{nodeid}} } )
            if exists $param{nodeid}  &&  exists $trstop{$param{nodeid}};
        push @stops, split( /\s*[,;]\s*/, $tag{'route_ref'} )   if exists $tag{'route_ref'};
        @stops = uniq @stops;
        $label .= q{ (} . join( q{,}, sort {
                my $aa = extract_number($a);
                my $bb = extract_number($b);
                $aa && $bb  ?  $aa <=> $bb : $a cmp $b;
            } @stops ) . q{)}   if @stops;
    }
=cut

    if ( $flags->{poi_contacts} && $info->{contacts} ) {
        # full address
        $info->{address} = _get_address( $info, point => $info->{coords} );
    }
    elsif ( $info->{name} && $info->{city} && !$info->{contacts} ) {
        # just region and country
        $info->{address} = _get_address( $info, level => 'city' );
    }

    $writer->output( point => { data => $info } );

    return;

=disable
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
    if ( $flags->{marine}  &&  $param{marine_buoy} ) {
        if ( my $buoy_type = ( $tag{'buoy'} or $tag{'beacon'} ) ) {
            $opts{FoundationColor} = $buoy_color{$buoy_type};
        }
        if ( my $buoy_light = ( $tag{'light:colour'} or $tag{'seamark:light:colour'} ) ) {
            $opts{Light} = $light_color{$buoy_light};
        }
        if ( my $light_type = ( $tag{'light:character'} or $tag{'seamark:light:character'} ) ) {
            ( $light_type ) = split /[\(\. ]/, $light_type;
            $opts{LightType} = $light_type{$light_type};
        }
    }

    ## Lights
    if ( $flags->{marine}  &&  $param{marine_light} ) {
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

        $opts{Light} = join( q{,},
            map { sprintf "(%s,%d,$_->[1])", ($light_color{$_->[0]} or '0'), $_->[3]/10 }
                sort { $a->[1] <=> $b->[1] } @sectors
            );

        my $light_type = ( $tag{'light:character'} or $tag{'seamark:light:character'} or 'isophase' );
        ( $light_type ) = split /[\(\. ]/, $light_type;
        $opts{LightType} = $light_type{$light_type}     if $light_type{$light_type};

        for my $sector ( grep { /seamark:light:\d/ } keys %tag ) {
            $comment .= "\n$sector -> $tag{$sector}";
        }
    }

    # other parameters - capital first letter!
    for my $key ( keys %param ) {
        next unless $key =~ / ^ _* [A-Z] /xms;
        delete $opts{$key} and next     if  !defined $param{$key} || $param{$key} eq q{};
        $opts{$key} = convert_string($param{$key});
    }

    $writer->output( point => { comment => $comment, opts => \%opts } );
=cut

#    say Dump $info;
}


sub AddBarrier {
    my %param = %{$_[0]};

    return  unless  exists $param{nodeid};
    return  unless  exists $param{tags};

    my $acc = $calc_access->get_barrier_flags($param{tags});
    return  if  none { $_ } @$acc;
    
    my @tr_acc = map { 1-$_ } @$acc;

    $barrier{$param{nodeid}}->{type}  = $param{tags}->{'barrier'};
    $barrier{$param{nodeid}}->{param} = join q{,}, @tr_acc    if any { $_ } @tr_acc;

    return;
}




sub WriteLine {
    my %param = %{$_[0]};
    my %tag   = ref $param{tags} ? %{$param{tags}} : ();

    return unless $param{chain};

    my %opts = (
        lzoom   => $param{level_l} || '0',
        hzoom   => $param{level_h} || '0',
        Type    => $param{type},
    );

    my $comment = $param{comment} || q{};

    while ( my ( $key, $val ) = each %tag ) {
        next if !$settings{comment}->{$key};
        $comment .= "\n$key = $val";
    }

    $opts{chain} = [ map { [ split /\s*,\s*/xms ] } @$nodes{ @{ $param{chain} } } ];

    $opts{Label}       = convert_string( $param{name} )   if defined $param{name} && $param{name} ne q{};
    $opts{RoadID}      = $param{roadid}         if exists $param{roadid};
    $opts{RouteParams} = $param{routeparams}    if exists $param{routeparams};

    for my $nod ( @{$param{nod}} ) {
        push @{ $opts{nods} }, [ @$nod[0,1], $$nod[2] || '0' ];
    }

    for my $key ( keys %{ $param{extra_fields} } ) {
        next if !defined $param{extra_fields}->{$key} || $param{extra_fields}->{$key} eq q{};
        $opts{$key} = convert_string( $param{extra_fields}->{$key} );
    }
    for my $key ( sort keys %param ) {
        next unless $key =~ / ^ _* [A-Z] /xms;
        delete $opts{$key} and next if !defined $param{$key} || $param{$key} eq q{};
        $opts{$key} = convert_string( $param{$key} );
    }

    $writer->output( polyline => { comment => $comment, opts => \%opts } );
    return;
}


sub AddRoad {

    my %param = %{$_[0]};
    my %tag   = exists $param{tags} ? %{$param{tags}} : ();

    return      unless  exists $param{chain};
    return      unless  exists $param{type};

    my ($orig_id) = $param{id} =~ /^([^:]+)/;

    my $llev  =  exists $param{level_l} ? $param{level_l} : 0;
    my $hlev  =  exists $param{level_h} ? $param{level_h} : 0;

    # determine city
    my @smart_points =
        map { $param{chain}->[$_] }
        ( floor($#{$param{chain}}/3), ceil($#{$param{chain}}*2/3) );
    my $city = FindCity( @smart_points );

    # calculate access restrictions
    my @rp = split q{,}, $param{routeparams};
    my @points = map { [ split q{,}, $nodes->{$_} // $_ ] } @smart_points;
    my $acc = $calc_access->get_road_flags( \%tag, [ @rp[4..11] ], @points );
    @rp[4..11] = @$acc;

    my $mp_address;
    if ( $param{name} ) {
        my $address = _get_address( { type => 'way', id => $orig_id }, level => 'street',
            points => \@smart_points, city => $city, tag => \%tag, street => $param{name},
        );
        $mp_address = _get_mp_address($address);
        $param{name} = $mp_address->{StreetDesc} || $param{name};
    }

    # calculate speed class
    my %speed_coef = (
        maxspeed             => 0.9,
        'maxspeed:practical' => 0.9,
        avgspeed             => 1,
    );
    for my $speed_key ( keys %speed_coef ) {
        next unless $tag{$speed_key};
        my $speed = extract_number( $tag{$speed_key} );
        next unless $speed;
        $speed *= 1.61   if  $tag{$speed_key} =~ /mph$/ixms;
        $rp[0] = speed_code( $speed * $speed_coef{$speed_key} );
    }

    # navitel-style 3d interchanges
    if ( my $layer = $flags->{interchange_3d} && extract_number($waytag->{'layer'}) ) {
        $layer *= 2     if $layer > 0;
        for my $node ( @{$param{chain}} ) {
            $hlevel{ $node } = $layer;
        }
        $layer--        if $layer > 0;
        $hlevel{ $param{chain}->[0]  } = $layer;
        $hlevel{ $param{chain}->[-1] } = $layer;
    }

    # road shield
    if ( $flags->{road_shields}  &&  !$city ) {
        my @ref =
            map { my $s = $_; $s =~ s/[\s\-]//gx; split /[,;]/, $s }
            grep {$_} ( @{ $road_ref{$orig_id} || [] }, @tag{'ref', 'int_ref'} );
        $param{name} = '~[0x06]' . join( q{ }, grep {$_} ( join(q{-}, uniq sort @ref), $param{name} ) )  if @ref;
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
        ( $mp_address ? (address => $mp_address) : () ),
    };

    # FIXME: buggy object comment
    while ( my ( $key, $val ) = each %tag ) {
        next if !$settings{comment}->{$key};
        $road{$param{id}}->{comment} .= "\n$key = $tag{$key}";
    }

    # the rest object parameters (capitals!)
    for my $key ( keys %param ) {
        next unless $key =~ /^_*[A-Z]/;
        $road{$param{id}}->{$key} = $param{$key};
    }

    # external nodes
    if ( $bound ) {
        if ( !is_inside_bounds( $nodes->{ $param{chain}->[0] } ) ) {
            $xnode{ $param{chain}->[0] } = 1;
            $xnode{ $param{chain}->[1] } = 1;
        }
        if ( !is_inside_bounds( $nodes->{ $param{chain}->[-1] } ) ) {
            $xnode{ $param{chain}->[-1] } = 1;
            $xnode{ $param{chain}->[-2] } = 1;
        }
    }

    # process associated turn restrictions
    if ( $flags->{restrictions}  ||  $flags->{dest_signs} ) {

        for my $relid ( @{$nodetr{$param{chain}->[0]}} ) {
            next unless exists $trest{$relid};
            if ( $trest{$relid}->{fr_way} eq $orig_id ) {
                $trest{$relid}->{fr_way} = $param{id};
                $trest{$relid}->{fr_dir} = -1;
                $trest{$relid}->{fr_pos} = 0;
            }
            if ( $trest{$relid}->{to_way} eq $orig_id ) {
                $trest{$relid}->{to_way} = $param{id};
                $trest{$relid}->{to_dir} = 1;
                $trest{$relid}->{to_pos} = 0;
            }
        }

        for my $relid ( @{$nodetr{$param{chain}->[-1]}} ) {
            next unless exists $trest{$relid};
            if ( $trest{$relid}->{fr_way} eq $orig_id ) {
                $trest{$relid}->{fr_way} = $param{id};
                $trest{$relid}->{fr_dir} = 1;
                $trest{$relid}->{fr_pos} = $#{ $param{chain} };
            }
            if ( $trest{$relid}->{to_way} eq $orig_id ) {
                $trest{$relid}->{to_way} = $param{id};
                $trest{$relid}->{to_dir} = -1;
                $trest{$relid}->{to_pos} = $#{ $param{chain} };
            }
        }
    }
    return;
}


sub output_area {
    my ($param, $obj, %opt) = @_;

    return  if !$param->{areas} || !@{$param->{areas}};
    
    while ( my ( $key, $val ) = each %{ $param->{tags} } ) {
        next if !$settings{comment}->{$key}; # ???
        $param->{comment} .= "\n$key = $val";
    }

    #   area-dependent zoomlevel
    if ( ref $param->{level_h} )  {
        my $square = sum map {
                Math::Polygon::Calc::polygon_area( @$_ )
                * cos( [polygon_centroid( @{$param->{areas}->[0]} )]->[1] / 180 * 3.14159 )
                * (40000/360)**2
            } @{$param->{areas}};
        my $tholds = delete $param->{level_h};
        my $level_h = ($param->{level_l} || 0) + last_index { $square >= $_ } @$tholds;
        return if $level_h < ($param->{level_l} || 0);
        $param->{level_h} = $level_h;
        $param->{comment} .= "\narea: $square km2 -> $level_h";
    }

    #   test if inside bounds
    my @inside = map { $bound ? $bound->{tree}->contains_polygon_rough( $_ ) : 1 } @{$param->{areas}};
    return      if all { defined && $_==0 } @inside;

    if ( $bound  &&  $flags->{less_gpc}  &&  any { !defined } @inside ) {
        @inside = map { $bound->{tree}->contains_points( @$_ ) } @{$param->{areas}};
        return  if all { defined && $_ == 0 } @inside;
    }


    $param->{holes} = []      unless $param->{holes};
    my @plist = grep { scalar @$_ > 3 } ( @{$param->{areas}}, @{$param->{holes}} );

    #   clip
    if ( $bound && $flags->{clip_areas} && !$opt{no_clip} && any { !defined } @inside ) {
        my $gpc = new_gpc();

        for my $area ( @{$param->{areas}} ) {
            $gpc->add_polygon( $area, 0 );
        }
        for my $hole ( @{$param->{holes}} ) {
            $gpc->add_polygon( $hole, 1 );
        }

        $gpc    =  $gpc->clip_to( $bound->{gpc}, 'INTERSECT' );
        @plist  =  sort  { $#{$b} <=> $#{$a} }  $gpc->get_polygons();
    }

    return    unless @plist;

    ## Navitel polygon addressing
    if ( $flags->{navitel} && $param->{tags}->{'addr:housenumber'} ) {
        $param->{address} = _get_address($obj, point => $plist[0]->[0]);
    }

=disable
    ## Navitel entrances
    if ( $flags->{navitel} ) {
        for my $entr ( @{ $param->{entrance} } ) {
            next  if !is_inside_bounds( $entr->[0] );
            push @{$opts{EntryPoint}}, {
                coords => [ split /\s*,\s*/xms, $entr->[0] ],
                name => convert_string( $entr->[1] )
            };
        }
    }
=cut

    # !!! need rearranging contours
    $param->{contours} = [ grep { @$_ >= 3 } @plist ];

    $writer->output( polygon => { data => $param } );
    return;
}




####    Config processing

sub cond_is_inside_city {
    my ($obj) = @_;

    return FindCity($obj->{latlon})  if exists $obj->{latlon};
    return FindCity($obj->{id})      if $obj->{type} eq 'Node';
    
    if ( $obj->{type} eq 'Way' ) {
        my $id = $obj->{id};
        my $chain = $mpoly->{$id} ? $mpoly->{$id}->[0]->[0] : $chains->{$id};
        return FindCity($chain->[ floor($#$chain/3) ]) && FindCity($chain->[ ceil($#$chain*2/3) ]);
    }
    return;
}

sub cond_is_named {
    my ($obj) = @_;
    return !!( name_from_list( label => $obj->{tag} ) );
}

sub cond_is_multipolygon {
    my ($obj) = @_;
    return exists $mpoly->{$obj->{id}};
}




sub _get_line_parts_inside_bounds {
    my ($chain) = @_;
    return $chain  if !$bound;

    my @is_inside = map { is_inside_bounds($nodes->{$_}) } @$chain;
    my @begin = grep { $is_inside[$_] && ( $_ == 0        || !$is_inside[$_-1] ) } ( 0 .. $#$chain );
    my @end   = grep { $is_inside[$_] && ( $_ == $#$chain || !$is_inside[$_+1] ) } ( 0 .. $#$chain );
    return map {[ @$chain[($begin[$_] == 0 ? 0 : $begin[$_]-1) .. ($end[$_] == $#$chain ? $end[$_] : $end[$_]+1)] ]} (0 .. $#end);
}


sub action_load_coastline {
    my ($obj, $action) = @_;
    return if !$flags->{shorelines};

    my $chain = $chains->{$obj->{id}};
    return if !$chain;

    my @parts = _get_line_parts_inside_bounds( $chain );
    for my $part ( @parts ) {
        $coast->add_coastline([ map {[ reverse split /,/x, $nodes->{$_} ]} @$part ]);
    }
    return;
}


sub _get_field_content {
    my ($field, $obj) = @_;

    return $field if !defined $field || !length $field;

    for ( ref $field ) {
        # !!!
        return $field when 'ARRAY';

        when (q{}) {
            $field =~ s[%(\w+)][ name_from_list($1, $obj->{tag}) // q{} ]ge;
            return undef if !length $field;
            return $field;
        }
        when ('CODE') {
            return $field->($obj);
        }

        say STDERR Dump \@_;
        confess "Bad field type: $_";
    }
}


sub _get_result_object_params {
    my ($obj, $action) = @_;

    $action->{name} = '%label'  if !exists $action->{name};

    my %info = %$action;

    # requred fields
    for my $key ( qw/ name type / ) {
        next if !defined $info{$key};
        $info{$key} = _get_field_content($info{$key}, $obj);
    }

    my $extra = $info{extra_fields} = {};
    
    # preset fields
    my @presets = @{ $info{presets} || [] };
    push @presets, 'contacts'  if $info{contacts};
    for my $preset_name ( @presets ) {
        my $preset = $settings{presets}->{$preset_name}
            or croak "Unknown preset $preset_name";
        while ( my ($key, $val) = each %$preset ) {
            my $val = _get_field_content($val, $obj);
            next if !defined $val;
            $extra->{$key} = $val;
        }
    }
    # individual fields
    for my $key ( keys %{ $action->{extra_fields} || {} } ) {
        my $val = _get_field_content($action->{extra_fields}->{$key}, $obj);
        next if !defined $val;
        $extra->{$key} = $val;
    }
    # !!! old config compatibility, to remove
    for my $key ( keys %$action ) {
        next if $key !~ /^_?[A-Z]/;
        my $val = _get_field_content($action->{$key}, $obj);
        next if !defined $val;
        $extra->{$key} = $val;
    }

    $info{tags} = $obj->{tag};
    $info{comment} = "$obj->{type}ID = $obj->{id}";

    return \%info;
}


sub action_write_line {
    my ($obj, $action) = @_;
    my $id = $obj->{id};

    my @parts = map { _get_line_parts_inside_bounds($_) }
        ( $mpoly->{$id} ? ( map {@$_} @{$mpoly->{$id}} ) : ( $chains->{$id} ) );
    return if !@parts;

    my $info = _get_result_object_params($obj, $action);
    for my $part ( @parts ) {
        $info->{chain} = $part;
        WriteLine( $info );
    }
    return;
}


sub action_load_road {
    return action_write_line(@_)  if !$flags->{routing};

    my ($obj, $action) = @_;

    my $id = $obj->{id};
    my @parts = map { _get_line_parts_inside_bounds($_) }
        ( $mpoly->{$id} ? ( map {@$_} @{$mpoly->{$id}} ) : ( $chains->{$id} ) );
    return if !@parts;

    my $info = _get_result_object_params($obj, $action);
    for my $part_no ( 0 .. $#parts ) {
        $info->{chain} = $parts[$part_no];
        $info->{id}    = "$id:$part_no";
        AddRoad( $info );
    }
    return;
}


# !!! TODO: remove, it was bad idea
sub action_modify_road {
    my ($obj, $action) = @_;
    return  if !$flags->{routing};

    my $id = $obj->{id};
    my $part_no = 0;
    while (1) {
        my $road_id = "$id:$part_no";
        my $road = $road{$road_id};
        last if !$road;
        $part_no ++;

        while ( my ($key, $val) = each %$action ) {
            if ( $key eq 'reverse' ) {
                $road->{chain} = [ reverse @{ $road->{chain} } ];
            }
            elsif ( $key eq 'routeparams' ) {
                my @rp  = split q{,}, $road->{rp};
                my @mrp = split q{,}, $val;
                for my $p ( @rp ) {
                    my $mp = shift @mrp;
                    $p = $mp    if $mp =~ /^\d$/;
                    $p = 1-$p   if $mp eq q{~};
                    $p = $p+$1  if $p < 4 && $mp =~ /^\+(\d)$/;
                    $p = $p-$1  if $p > 0 && $mp =~ /^\-(\d)$/;
                }
                $road->{rp} = join q{,}, @rp;
            }
            else {
                $road->{$key} = _get_field_content($val, $obj);
            }
        }

    }
    return;
}


sub action_process_interpolation {
    my ($obj, $action) = @_;
    return if !$flags->{addr_interpolation};

    my $id = $obj->{id};

    my @parts = map { _get_line_parts_inside_bounds($_) }
        ( $mpoly->{$id} ? ( map {@$_} @{$mpoly->{$id}} ) : ( $chains->{$id} ) );
    return if !@parts;

    for my $part ( @parts ) {
        my @chain = grep { exists $nodetag->{$_}->{'addr:housenumber'} } @$part;
        next if @chain < 2;

        my $new_action = { %$action, action => 'write_poi' };
        for my $i ( 0 .. $#chain-1 ) {
            my ( $node1, $node2 ) = @chain[ $i, $i+1 ];
            my ( $house1, $house2 ) =
                map { my $x = $nodetag->{$_}->{'addr:housenumber'}; $x =~ s/^(\d+).*/$1/x; $x }
                ( $node1, $node2 );
            next if $house1 == $house2;

            my %tag = ( %{$nodetag->{$node2}}, %{$nodetag->{$node1}}, %{$obj->{tag}} );
            my $step = ( $tag{'addr:interpolation'} eq 'all' ? 1 : 2 );
            $step *= -1  if $house1 > $house2;

            my ($lat1, $lon1) = split q{,}, $nodes->{$node1};
            my ($lat2, $lon2) = split q{,}, $nodes->{$node2};
            my $steplat = ($lat2-$lat1) / ($house2-$house1);
            my $steplon = ($lon2-$lon1) / ($house2-$house1);

            for my $j ( 0 .. ($house2-$house1)/$step ) {
                next if $i > 0 && $j == 0;
                my $chouse = $house1 + $step * $j;
                my $clat = $lat1 + $steplat * $j * $step;
                my $clon = $lon1 + $steplon * $j * $step;

                my $new_obj = {
                    id      => $obj->{id},
                    type    => 'Way',
                    latlon  => join( q{,}, $clat, $clon ),
                    tag     => { %tag, 'addr:housenumber' => $chouse, },
                };

                action_write_poi($new_obj, $new_action);
            }
        }
    }
    return;
}


sub action_write_polygon {
    my ($obj, $action) = @_;

    my $info = _get_result_object_params($obj, $action);

    my $id = $obj->{id};
    if ( $mpoly->{$id} ) {
        $info->{areas} = [ map {[ map {[ reverse split q{,}, $nodes->{$_} ]} @$_ ]} @{$mpoly->{$id}->[0]} ];
        $info->{holes} = [ map {[ map {[ reverse split q{,}, $nodes->{$_} ]} @$_ ]} @{$mpoly->{$id}->[1]} ];
        $info->{entrance} = [
            map { [ $nodes->{$_}, $entrance{$_} ] }
            grep { exists $entrance{$_} }
            map { @$_ }
            map { @$_ } @{$mpoly->{$id}}
        ];
    }
    else {
        return if $chains->{$id}->[0] ne $chains->{$id}->[-1];
        
        $info->{areas} = [ [ map {[ reverse split q{,}, $nodes->{$_} ]} @{$chains->{$id}} ] ];
        $info->{entrance} = [
            map { [ $nodes->{$_}, $entrance{$_} ] }
            grep { exists $entrance{$_} }
            @{$chains->{$id}}
        ];
    }

    output_area( $info, $obj );
    return;
}


sub action_address_poi {
    my ($obj, $action) = @_;

    return if !exists $poi_rtree->{root};

    my $outer = $mpoly->{$obj->{id}} ? $mpoly->{$obj->{id}}->[0]->[0] : $chains->{$obj->{id}};
    my @bbox = Math::Polygon::Calc::polygon_bbox( map {[ reverse split q{,}, $nodes->{$_} ]} @$outer );

    my @poilist;
    $poi_rtree->query_completely_within_rect( @bbox, \@poilist );

    for my $id ( @poilist ) {
        next unless exists $poi{$id};
        next unless Math::Polygon::Tree::polygon_contains_point(
            [ reverse split q{,}, $nodes->{$id} ],
            map {[ reverse split q{,}, $nodes->{$_} ]} @$outer
        );

        my $house_address = $addresser->get_address_tags($obj->{tag});

        for my $poiobj ( @{ $poi{$id} } ) {
            $poiobj->{tags} = _hash_merge( $house_address, $poiobj->{tags} );
            $poiobj->{comment} .= "\nAddressed by $obj->{type}ID = $obj->{id}";
            output_poi( $poiobj );
        }
        delete $poi{$id};
    }
    return;
}


sub action_write_poi {
    my ($obj, $action) = @_;

    my $info = _get_result_object_params($obj, $action);

    my $latlon = $obj->{latlon} || ( $obj->{type} eq 'Node' && $nodes->{$obj->{id}} );

    # for areas: place poi at main entrance if exists
    if ( !$latlon && $obj->{type} eq 'Way' ) {
        my $outer = $mpoly->{$obj->{id}} ? $mpoly->{$obj->{id}}->[0]->[0] : $chains->{$obj->{id}};
        my $entrance_node = first { exists $main_entrance{$_} } @$outer;
        $latlon = $entrance_node
            ? $nodes->{$entrance_node}
            : join( q{,}, polygon_centroid( map {[ split /,/, $nodes->{$_} ]} @$outer ) );
    }

    return  unless $latlon && is_inside_bounds( $latlon );

    $info->{coords} = [ reverse split q{,}, $latlon ];

    if ( $flags->{addr_from_poly}
        && $obj->{type} eq 'Node' # ???
        && $info->{contacts}
        && (!defined $info->{inherit_address} || $info->{inherit_address})
    ) {
        # save poi for addressing
        my $id = $obj->{id};
        my @bbox = ( (@{ $info->{coords} }) x 2 );
        push @{$poi{$id}}, $info;
        $poi_rtree->insert( $id, @bbox );
    }
    else {
        output_poi( $info );
    }

    return;
}


sub action_load_access_area {
    my ($obj, $action) = @_;

    my @contours = map { [ map { [ split q{,}, $nodes->{$_} ] } @$_ ] } @{ $obj->{outer} };
    $calc_access->add_area( $obj->{tag}, @contours ); 
    
    return;
}


sub _load_area {
    my ($level, $obj, $action) = @_;
    
    return if $obj->{outer}->[0]->[0] ne $obj->{outer}->[0]->[-1];

    my $address_tags = $addresser->get_address_tags($obj->{tag}, level => $level);
    my @contours = map { [ map { [ split q{,}, $nodes->{$_} ] } @$_ ] } @{ $obj->{outer} };

    $addresser->load_area($level, $address_tags, @contours);
    return;
}


sub action_load_main_entrance {
    my ($obj, $action) = @_;
    $main_entrance{$obj->{id}} = 1;
    return;
}


sub action_load_building_entrance {
    my ($obj, $action) = @_;
    return if !$flags->{navitel};
    $entrance{$obj->{id}} = name_from_list(entrance => $obj->{tag}) // q{};
    return;
}


sub action_force_external_node {
    my ($obj, $action) = @_;
    return if !$flags->{routing};
    $xnode{$obj->{id}} = 1;
    return;
}


sub action_load_barrier {
    my ($obj, $action) = @_;
    return if !$flags->{routing} || !$flags->{barriers};
    AddBarrier({ nodeid => $obj->{id}, tags => $obj->{tag} });
    return;
}



sub report {
    my ( $msg, $type ) = @_;
    $type ||= 'ERROR';
    $writer->output( info => { text => "$type: $msg" } );
    return;
}


sub print_section {
    my ($title) = @_;
    $writer->output( section => { text => "### $title" } );
    return;
}


sub extract_number {
    my $str = shift;
    return unless defined $str;
    my ($number) = $str =~ /^ ( [-+]? \d+ ) /x;
    return $number;
}



=head2 _get_address( $obj, %opt )

Options:
  * city (don't search again)
  * point
  * tag (if no 'tag' field in $obj)
  * street (main street name)

Address base fields:
  * country
  * region
  * city
  * street
  * housenumber

Extra fields should be joined with main fields

=cut

sub _get_address {

    my ($obj, %opt) = @_;

    my $tags = $opt{tag} || $opt{tags} || $obj->{tag} || $obj->{tags} || {};

    # parent city
    my @point = grep { $_ } ( $opt{point}, @{ $opt{points} || [] } );
    my $city = $opt{city} || ( @point && FindCity(@point) );

    my $address_tags = _hash_merge( {},
        $addresser->get_address_tags($tags, level => $opt{level}),
        ($city || {}),
    );

    my $address = $addresser->get_lang_address($address_tags, $lang_select);

    return $address;
}


sub _get_mp_address {
    my ($address, %opt) = @_;

    my %mp_address;

    if ( $address->{housenumber} ) {
        $mp_address{HouseNumber} = convert_string( $address->{housenumber} );
    }

    if ( $address->{housenumber} || $address->{street} ) {
        my @fields = grep {$_} map { $address->{$_} } qw/ street quarter suburb /;
        push @fields, $address->{city}  if !@fields && $address->{city} && $address->{city};

        if ( @fields && ( my $street = join q{ }, shift(@fields), map {"($_)"} @fields ) ) {
            $mp_address{StreetDesc} = convert_string( $street );
        }
    }

    if ( $address->{city} ) {
        $mp_address{CityName} = convert_string( $address->{city} );
    }

    if ( $address->{region} ) {
        my $region = join q{ }, grep {$_}
            map { $address->{$_} && $address->{$_} } qw/ region district subdistrict /;
        $mp_address{RegionName} = convert_string( $region );
    }

    if ( $address->{country} ) {
        $mp_address{CountryName} = convert_string( $address->{country} );
    }

    if ( $address->{postcode} ) {
        $mp_address{Zip} = convert_string( $address->{postcode} );
    }

    return \%mp_address;
}



sub _hash_merge {
    my $target = shift;
    for my $hash_to_add ( @_ ) {
        for my $key ( keys %$hash_to_add ) {
            $target->{$key} = $hash_to_add->{$key};
        }
    }

    return $target;
}
