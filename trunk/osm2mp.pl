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
##    * Tree::R
##    * Geo::Openstreetmap::Parser
##
##  Optionally required: 
##    * Template
##    * Geo::Shapefile::Writer
##    * Math::Geometry::Planar::GPC::Polygon or Math::Geometry::Planar::GPC::PolygonXS
##    * Text::Unidecode
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

our $VERSION = '1.03' . do { my ($r) = '$Rev$' =~ /(\d+)/xms; $r ? "-$r" : q{} };



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
use List::MoreUtils qw{ all notall none any first_index last_value uniq };

use Math::Polygon;
use Math::Polygon::Tree  0.068  qw{ :all };
use Tree::R;

use OSM;
use OsmAddress;
use LangSelect;
use FeatureConfig;
use Boundary;
use Coastlines;
use TransportAccess;
use RouteGraph;
use AreaTree;
use Clipper;




print STDERR "\n  ---|   OSM -> MP converter  $VERSION   (c) 2008-2013 liosha, xliosha\@gmail.com\n";



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
my $use_osm_bbox;


####    Global vars

my %entrance;
my %main_entrance;

my %poi;
my $poi_rtree = Tree::R->new();

my $ft_config = FeatureConfig->new(
    actions => {
        load_access_area        => \&action_load_access_area,
        load_cityside_area      => \&action_load_cityside_area,
        load_barrier            => \&action_load_barrier,
        load_building_entrance  => \&action_load_building_entrance,
        write_poi               => \&action_write_poi,
        write_polygon           => \&action_write_polygon,
        write_line              => \&action_write_line,
        address_poi             => \&action_address_poi,
        load_road               => \&action_load_road,
        load_coastline          => \&action_load_coastline,
        force_external_node     => \&action_force_external_node,
        load_main_entrance      => \&action_load_main_entrance,
        process_interpolation   => \&action_process_interpolation,
    },
    conditions => {
        named       => \&cond_is_named,
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

my $addresser = OsmAddress->new( %settings );
my $calc_access = TransportAccess->new( %settings );
my $cityside_area = AreaTree->new();



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

    'bbox=s'            => sub { $bbox = [ split q{,}, $_[1] ] },
    'bpoly=s'           => \$bpolyfile,
    'osmbbox!'          => \$use_osm_bbox,

    'namelist|taglist=s%' => sub { $taglist{$_[1]} = [ split /[ ,]+/, $_[2] ] },

    # obsolete, for backward compatibility
    'nametaglist=s'     => sub { push @ARGV, '--namelist', "label=$_[1]" },
);


usage() unless (@ARGV);



my $lang_select = LangSelect->new(
    target_lang => $values->{target_lang},
    default_lang => $values->{default_lang} // $values->{target_lang},
);




####    Loading OSM data

my $infile = shift @ARGV;
my ($in, $stream_msg) = $infile ~~ '-'
    ? ( *STDIN, 'STDIN' )
    : ( do{ open( my $fh, '<', $infile ); $fh }, "file $infile" );

say STDERR "\nLoading OSM data from $stream_msg...";

my $osm = OSM->new(
    fh => $in,
    skip_tags => [ keys %{$settings{skip_tags}} ],
);

close $in  if $infile ne q{-};




####    Initializing bounds

my $bound;

$bbox = $osm->{bbox}  if $use_osm_bbox;
if ($bbox || $bpolyfile) {
    my $bounds_msg = $bbox ? 'bbox' : "file $bpolyfile";
    say STDERR "\nInitialising bounds from $bounds_msg...";

    if ($bbox) {
        my ($minlon, $minlat, $maxlon, $maxlat) = @$bbox;
        $bound = Boundary->new([
                [$minlon,$minlat],
                [$maxlon,$minlat],
                [$maxlon,$maxlat],
                [$minlon,$maxlat],
                [$minlon,$minlat],
            ]);
    }
    elsif ($bpolyfile) {
        $bound = Boundary->new( $bpolyfile );
    }

    printf STDERR "  %d segments\n", scalar @{ $bound->get_points() } - 1;
}




##  Preloading areas
say STDERR "\nLoading search areas...";

if ( $flags->{addressing} ) {
    my $addr_area_config = $addresser->get_area_ftconfig($osm);
    $ft_config->add_rules( areas => $addr_area_config );
}

$osm->iterate_ways( sub { $ft_config->process( areas => @_ ) } );

printf STDERR "  %d cities\n", $addresser->{areas}->{city} && $addresser->{areas}->{city}->{_count} // 0;
printf STDERR "  %d districts\n", $addresser->{areas}->{district} && $addresser->{areas}->{district}->{_count} // 0;
printf STDERR "  %d restricted areas\n", $calc_access->{areas}->{_count} // 0;
printf STDERR "  %d settlement areas\n", $cityside_area->{_count} // 0;



my $rgraph = RouteGraph->new();

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

if ( $flags->{routing} ) {
    my $add_restr_sub = sub {
        my ($relation_id, $members, $tags) = @_;

        my ($type) = ($tags->{restriction} // 'no') =~ /^(only | no)/xms;
        return if !$type;

        my $from_member = first { $_->{type} eq 'way' && $_->{role} eq 'from' } @$members;
        return if !$from_member;

        my $to_member = first { $_->{type} eq 'way' && $_->{role} eq 'to' } @$members;
        $to_member //= $from_member  if $tags->{restriction} ~~ 'no_u_turn';
        return if !$to_member;

        my $via_member = first { $_->{type} eq 'node' && $_->{role} eq 'via' } @$members;
        return if !$via_member;

        my %vtags = (
            foot => 'no',   # assume no foot restrictions
            ( $tags->{except} ? (map {( $_ => 'no' )} split /\s* [,;] \s*/x, $tags->{except}) : () ),
        );
        my $acc = $calc_access->get_tag_flags( \%vtags );
        return if all {$_} @$acc;
       
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

        return;
    };

    $osm->iterate_relations(restriction => $add_restr_sub);

    printf STDERR "  %d turn restrictions\n", scalar keys %trest;
}


if ( $flags->{routing} && $flags->{dest_signs} ) {
    my $add_sign_sub = sub {
        my ($relation_id, $members, $tags) = @_;

        my $from_member = first { $_->{type} eq 'way' && $_->{role} eq 'from' } @$members;
        return if !$from_member;

        my $to_member = first { $_->{type} eq 'way' && $_->{role} eq 'to' } @$members;
        return if !$to_member;

        my $via_member = first { $_->{type} eq 'node' && $_->{role} ~~ [qw/ sign intersection /] } @$members;
        return if !$via_member;

        my $name = name_from_list( destination => $tags );
        return if !$name;

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

        return;
    };

    my $crossroads_cnt = scalar keys %trest;
    $osm->iterate_relations(destination_sign => $add_sign_sub);

    printf STDERR "  %d destination signs\n", scalar(keys %trest) - $crossroads_cnt;
}


if ( $flags->{street_relations} ) {
    my $member_count = 0;

    my $add_street_sub = sub {
        my ($relation_id, $members, $tags) = @_;

        # EXPERIMENTAL: resolve addr:* roles
        for my $member ( @$members ) {
            my ($type, $ref, $role) = @$member{ qw/ type ref role / };
            next if $role !~ / ^ addr: /xms;

            my $tag_ref = $osm->get_tags($type => $ref);
            next if !$tag_ref;

            for my $k ( reverse sort keys %$tag_ref ) {    # 'name' before 'addr:*'!
                (my $nk = $k) =~ s/^ name \b/$role/xms;
                next if $nk !~ m/ ^ $role \b /xms;
                $tags->{$nk} = $tag_ref->{$k}; # !!! side effect?
            }
        }

        # house tags: addr:* and street's name* as addr:street*
        my %house_tag;
        for my $k ( reverse sort keys %$tags ) {    # 'name' before 'addr:*'!
            (my $nk = $k) =~ s/^ name \b/addr:street/xms;
            next if $nk !~ m/ ^ addr: /xms;
            $house_tag{$nk} = $tags->{$k};
        }

        # street tags: all except 'type'
        my %street_tag = %$tags;
        delete $street_tag{type};

        # add relation tags to members
        for my $member ( @$members ) {
            $member_count ++;
            my ($type, $ref, $role) = @$member{ qw/ type ref role / };
            ($type, $ref) = (way => "r$ref")  if $type eq 'relation';

            my $tag_ref = $osm->get_tags($type => $ref);
            next if !$tag_ref;

            if ( %house_tag && $role ~~ [ qw/ house address / ] ) {
                $osm->set_tags( $type, $ref, { %$tag_ref, %house_tag } );
            }
            elsif ( %street_tag && $role ~~ 'street' ) {
                $osm->set_tags( $type, $ref, { %$tag_ref, %street_tag } );
            }
        }
    };

    $osm->iterate_relations(street => $add_street_sub);
    $osm->iterate_relations(associatedStreet => $add_street_sub);

    printf STDERR "  %d houses with associated street\n", $member_count;
}

if ( $flags->{road_shields} ) {
    my $add_shield_sub = sub {
        my ($relation_id, $members, $tags) = @_;

        return if !( $tags->{route} ~~ 'road' );

        my @ref = grep {$_} @$tags{'ref', 'int_ref'};
        return if !@ref;

        for my $member ( @$members ) {
            next if $member->{type} ne 'way';
            push @{$road_ref{$member->{ref}}}, @ref;
        }
    };
    
    $osm->iterate_relations(route => $add_shield_sub);
    printf STDERR "  %d road ways with ref\n", scalar keys %road_ref;
}


if ( $flags->{transport_stops} ) {
    my $add_stop_sub = sub {
        my ($relation_id, $members, $tags) = @_;

        return if !( $tags->{route} ~~ [ qw/ bus / ] );

        my $ref = $tags->{ref};
        return if !$ref;

        for my $member ( @$members ) {
            next if $member->{type} ne 'node';
            next if $member->{role} !~ /stop|platform/x;
            push @{ $trstop{$member->{ref}} }, $ref;
        }
    };
    
    $osm->iterate_relations(route => $add_stop_sub);
    printf STDERR "  %d transport stops\n", scalar keys %trstop;
}



my $coast = Coastlines->new( $bound ? $bound->get_points() : [] );



##  Process POI nodes

say STDERR "\nProcessing nodes...";

print_section( 'Simple objects' );
$osm->iterate_nodes( sub { $ft_config->process( nodes => @_ ) } );

my $countpoi = $writer->{_count}->{point} // 0;
printf STDERR "  %d POI written\n", $countpoi;
printf STDERR "  %d POI loaded for addressing\n", scalar keys %poi      if $flags->{addr_from_poly};
printf STDERR "  %d building entrances loaded\n", scalar keys %entrance if $flags->{navitel};
printf STDERR "  %d main entrances loaded\n", scalar keys %main_entrance;


##  Process ways

say STDERR "\nProcessing ways...";

$osm->iterate_ways( sub {
        $ft_config->process( ways => @_ );
        $ft_config->process( nodes => @_ )  if $flags->{make_poi};
    } );

printf STDERR "  %d POI written\n", ($writer->{_count}->{point} // 0) - $countpoi;
printf STDERR "  %d lines written\n", $writer->{_count}->{polyline} // 0;
printf STDERR "  %d polygons written\n", $writer->{_count}->{polygon} // 0;
printf STDERR "  %d roads loaded\n", scalar keys %road                      if $flags->{routing};
printf STDERR "  %d coastlines loaded\n", scalar keys %{$coast->{lines}}    if $flags->{shorelines};


####    Writing non-addressed POIs

undef $poi_rtree;
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
                if ( $r1 ne $r2 && _are_roads_same( $road{$r1}, $road{$r2} )
                    && lcos( $p1->[-2], $p1->[-1], $road{$r2}->{chain}->[1] ) > $values->{merge_cos} )
                {
                    push @list, $r2;
                }
            }

            # merging
            if ( @list ) {
                $countmerg ++;
                @list  =  sort {  lcos( $p1->[-2], $p1->[-1], $road{$b}->{chain}->[1] )
                              <=> lcos( $p1->[-2], $p1->[-1], $road{$a}->{chain}->[1] )  }  @list;

                my $r2 = $list[0];

                # process associated restrictions
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
                    }
                    $rnod = 2;
                }

                elsif ( $rnod == 1 + $values->{max_road_nodes} // 999 ) {
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
                    while ( my ($relid, $tr) = each %trest )  {
                        if (  $tr->{to_way} eq $roadid
                            &&  $tr->{to_pos} >  $breaks[$i]   - (1 + $tr->{to_dir}) / 2
                            &&  $tr->{to_pos} <= $breaks[$i+1] - (1 + $tr->{to_dir}) / 2
                        ) {
                            my $msg = "Turn restriction RelID=$relid TO moved from $roadid($tr->{to_pos})";
                            $tr->{to_way}  =  $id;
                            $tr->{to_pos}  -= $breaks[$i];
                            report( "$msg to $id($tr->{to_pos})", 'FIX' );
                        }
                        if (  $tr->{fr_way} eq $roadid
                            &&  $tr->{fr_pos} >  $breaks[$i]   + ($tr->{fr_dir} - 1) / 2
                            &&  $tr->{fr_pos} <= $breaks[$i+1] + ($tr->{fr_dir} - 1) / 2
                        ) {
                            my $msg = "Turn restriction RelID=$relid FROM moved from $roadid($tr->{fr_pos})";
                            $tr->{fr_way} =  $id;
                            $tr->{fr_pos} -= $breaks[$i];
                            report( "$msg to $id($tr->{fr_pos})", 'FIX' );
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

            # emergency,delivery,car,bus,taxi,foot,bike,truck
            my @auto_links =
                map { $node eq $road{$_}->{chain}->[0] || $node eq $road{$_}->{chain}->[-1]  ?  ($_)  :  ($_,$_) }
                grep { $road{$_}->{access_flags} =~ /^.,.,0/ } @{ $nodeways{$node} };

            next  unless scalar @auto_links == 2;
            next  unless scalar( grep { !$road{$_}->{oneway} } @auto_links ) == 2;

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
                }
                $cnode = $node;
            }
        }
        print STDERR "$countclose pairs fixed\n";
    }


    # numbering roads
    my $roadcount = $values->{first_road_id} || 1;
    while ( my ($roadid, $road) = each %road ) {
        $road->{road_id} = $roadid{$roadid} = $roadcount++;
        $road->{comment} = "WayID = $roadid" . ( "\n$road->{comment}" // q{} );
    }



    ###    dumping roads
    print STDERR "Writing roads...          ";

    while ( my ($roadid, $road) = each %road ) {
        my %road_info = %$road; 

        for my $i ( 0 .. $#{$road->{chain}} ) {
            my $node = $road->{chain}->[$i];
            next if !$nodid{$node};
            push @{$road_info{nod}}, [ $i, $nodid{$node}, $xnode{$node} ];
        }

=disable
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
=cut 

#        say Dump \%road_info; exit;
        output_road( \%road_info );


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


if ( $flags->{routing} ) {

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

                if ( $newtr{to_pos} > 0
                  &&  !( $tr->{to_way} eq $roadid  &&  $tr->{to_dir} eq -1 )
                  &&  !$road{$roadid}->{oneway} ) {
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

sub _are_roads_same {
    my ($r1, $r2) = @_;

    my @plist = qw/ type name oneway toll road_class speed access_flags road_ref refs address extra_fields /;
    push @plist, uniq grep { /^_*[A-Z]/ } ( keys %$r1, keys %$r2 );

    for my $param ( @plist ) {
        my ($p1, $p2) = ( $r1->{$param}, $r2->{$param} );
        next if !defined $p1 && !defined $p2;
        return 0 if ref $p1 ne ref $p2;

        if ( !ref $p1 || ref $p1 eq 'ARRAY' ) {
            return 0 if !( $p1 ~~ $p2 );
        }
        elsif ( ref $p1 eq 'HASH' ) {
            return 0 if !( [ sort keys %$p1 ] ~~ [ sort keys %$p2 ] );
            return 0 if !all { $p1->{$_} ~~ $p2->{$_} } keys %$p1;
        }
    }
    
    return 1;
}

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

    my ($lon1, $lat1) = @{ $osm->get_lonlat($id0) };
    my ($lon2, $lat2) = @{ $osm->get_lonlat($id1) };

    my ($clat, $clon) = ( ($lat1+$lat2)/2, ($lon1+$lon2)/2 );
    my ($dlat, $dlon) = ( ($lat2-$lat1),   ($lon2-$lon1)   );
    my $klon = cos( $clat * 3.14159 / 180 );

    my $ldist = $values->{fix_close_dist} * 180 / 20_000_000;

    my $res = ($dlat**2 + ($dlon*$klon)**2) < $ldist**2;

    # fixing
    if ( $res ) {
        if ( $dlon == 0 ) {
            $osm->set_lonlat( $id0, $clon, $clat - $ldist/2 * ($dlat==0 ? 1 : ($dlat <=> 0) ) );
            $osm->set_lonlat( $id1, $clon, $clat + $ldist/2 * ($dlat==0 ? 1 : ($dlat <=> 0) ) );
        }
        else {
            my $azim  = $dlat / $dlon;
            my $ndlon = sqrt( $ldist**2 / ($klon**2 + $azim**2) ) / 2;
            my $ndlat = $ndlon * abs($azim);

            $osm->set_lonlat( $id0, $clon - $ndlon * ($dlon <=> 0), $clat - $ndlat * ($dlat <=> 0) );
            $osm->set_lonlat( $id1, $clon + $ndlon * ($dlon <=> 0), $clat + $ndlat * ($dlat <=> 0) );
        }
    }
    return $res;
}



sub lcos {                      # NodeID1, NodeID2, NodeID3

    my ($id0, $id1, $id2) = @_;

    my ($lon1, $lat1) = @{ $osm->get_lonlat($id0) };
    my ($lon2, $lat2) = @{ $osm->get_lonlat($id1) };
    my ($lon3, $lat3) = @{ $osm->get_lonlat($id2) };

    my $klon = cos( ($lat1+$lat2+$lat3) / 3 * 3.14159 / 180 );

    my $xx = (($lat2-$lat1)**2+($lon2-$lon1)**2*$klon**2) * (($lat3-$lat2)**2+($lon3-$lon2)**2*$klon**2);

    return -1   if ( $xx == 0);
    return (($lat2-$lat1)*($lat3-$lat2)+($lon2-$lon1)*($lon3-$lon2)*$klon**2) / sqrt($xx);
}



sub is_inside_bounds {
    return 1 if !$bound;

    my ($point) = @_;
    return $bound->contains( ref $point ? $point : $osm->get_lonlat($point) );
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
    [ barriers          => 'create restrictions on barrier nodes' ],
    [ disable_u_turns   => 'disable u-turns on nodes with 2 links' ],
    [ dest_signs        => 'process destination signs' ],
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
    [ target_lang       => 'desired map language', 'tl' ],
    [ default_lang      => 'source language for default tags', 'dl' ],

    [ huge_sea          => undef ],
    [ first_nod_id      => undef ],
    [ first_road_id     => undef ],
);    

my @onoff = ( "off", "on");


sub _get_getopt_key {
    my @keys = @_;
    
    my @results;
    for my $key (@keys) {
        next if !$key;
        push @results, $key;
        $key =~ s/_/-/gx;
        push @results, $key;
        $key =~ s/-//gx;
        push @results, $key;
    }

    return join q{|}, uniq @results;
}


sub _get_settings_getopt {
    return (
        ( map {( _get_getopt_key($_->[0], $_->[2]) . q{!}  => \$flags->{$_->[0]}  )} @available_flags ),
        ( map {( _get_getopt_key($_->[0], $_->[2]) . q{=s} => \$values->{$_->[0]} )} @available_values  ),
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




sub output_poi {
    my ($info) = @_;

    while ( my ( $key, $val ) = each %{ $info->{tags} } ) {
        next if !$settings{comment}->{$key}; # ???
        $info->{comment} .= "\n$key = $val";
    }

=disable

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
        $info->{address} = _get_address( $info, level => 'city', point => $info->{coords} );
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



sub output_road {
    my ($info) = @_;
    my %param = %$info;

    return if !$info->{chain};
    return if @{$info->{chain}} < 2;

    $param{chain} = $osm->get_lonlat( $info->{chain} );

    $writer->output( road => { data => \%param } );
    return;
}


sub output_line {
    my ($info) = @_;
    my %param = %$info;

    while ( my ( $key, $val ) = each %{ $param{tags} } ) {
        next if !$settings{comment}->{$key}; # ???
        $param{comment} .= "\n$key = $val";
    }

    $param{chain} = $osm->get_lonlat( $info->{chain} );

    $writer->output( polyline => { data => \%param } );
    return;
}


sub AddRoad {
    my ($info) = @_;

    my $tags = $info->{tags};
    my %params = map {($_ => $info->{$_})} grep {exists $info->{$_}} (
        qw/ id name type chain level_h road_ref refs extra_fields /,
        grep {/_*[A-Z]/} keys %$info,
    );

    $params{chain} = [ reverse @{$params{chain}} ]  if $info->{reverted};

    my ($orig_id) = $info->{id} =~ /^([^:]+)/;
    $params{way_id} = $orig_id;

    # object comment (useless?)
    $params{comment} = join qq{\n}, map {"$_ = $tags->{$_}"} grep {$settings{comment}->{$_}} sort keys %$tags;

    # points to determine address areas
    my $chain_size = $#{ $info->{chain} };
    my @smart_nodes =
        map { $info->{chain}->[$_] } ( floor($chain_size/3), ceil($chain_size*2/3) );

    # extend routeparams
    my ($rp_speed, $rp_class, $rp_oneway, $rp_toll, @acc_flags) = split q{,}, $info->{routeparams} || q{};

    $params{road_class} = $info->{road_class} // $rp_class || 0;
    $params{oneway}     = $info->{oneway} // $rp_oneway     if $flags->{oneway};  # !!!
    $params{toll}       = $info->{toll} // $rp_toll;

    # calculate speed
    state $speed_tags = [
        [ avgspeed             => 1.0 ],
        [ 'maxspeed:practical' => 0.9 ],
        [ maxspeed             => 0.9 ],
    ];

    # gme or garmin table: [8,20,40,56,72,93,108,128]
    my $predicted_speed = $info->{speed} // [5,20,40,60,80,90,110,130]->[$rp_speed || 0];

    for my $tag_info ( @$speed_tags ) {
        my ($key, $coef) = @$tag_info;
        my $val = $tags->{$key};
        next if !$val;

        my $speed = extract_number( $val );
        next if !$speed;

        $speed *= 1.61   if  $val =~ /mph$/ixms;
        $predicted_speed = $speed * $coef;
        last;
    }

    $params{speed} = $predicted_speed;

    # calculate access restrictions
    my $points = $osm->get_lonlat(\@smart_nodes);
    $params{access_flags} = join q{,}, @{ $calc_access->get_road_flags( $tags, \@acc_flags, @$points ) };

    # resolve address
    if ( $info->{name} ) {
        $params{address} = _get_address( { type => 'way', id => $orig_id }, level => 'street',
            points => \@smart_nodes, tags => $tags, street => $info->{name},
        );
    }

=disabled
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
=cut

    # load road
    $road{$info->{id}} = \%params;
#    $rgraph->add_road( \%params );

    my $chain = $info->{chain};

    # external nodes
    if ( $bound ) {
        my $is_first_inside = is_inside_bounds($chain->[0]);
        $xnode{$chain->[0]} = 1  if $is_first_inside < 1;
        $xnode{$chain->[1]} = 1  if $is_first_inside == 0;
        
        my $is_last_inside = is_inside_bounds($chain->[-1]);
        $xnode{$chain->[-1]} = 1  if $is_last_inside < 1;
        $xnode{$chain->[-2]} = 1  if $is_last_inside == 0;
    }

    # process associated turn restrictions
    for my $relid ( @{$nodetr{$chain->[0]}} ) {
        next unless exists $trest{$relid};
        if ( $trest{$relid}->{fr_way} eq $orig_id ) {
            $trest{$relid}->{fr_way} = $info->{id};
            $trest{$relid}->{fr_dir} = -1;
            $trest{$relid}->{fr_pos} = 0;
        }
        if ( $trest{$relid}->{to_way} eq $orig_id ) {
            $trest{$relid}->{to_way} = $info->{id};
            $trest{$relid}->{to_dir} = 1;
            $trest{$relid}->{to_pos} = 0;
        }
    }

    for my $relid ( @{$nodetr{$chain->[-1]}} ) {
        next unless exists $trest{$relid};
        if ( $trest{$relid}->{fr_way} eq $orig_id ) {
            $trest{$relid}->{fr_way} = $info->{id};
            $trest{$relid}->{fr_dir} = 1;
            $trest{$relid}->{fr_pos} = $#$chain;
        }
        if ( $trest{$relid}->{to_way} eq $orig_id ) {
            $trest{$relid}->{to_way} = $info->{id};
            $trest{$relid}->{to_dir} = -1;
            $trest{$relid}->{to_pos} = $#$chain;
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

    my @contours = ( @{$param->{areas}}, @{$param->{holes} || []} );

    if ( $bound ) {
        my $is_clipping = !$opt{no_clip} && $flags->{clip_areas};
        my $need_to_clip;

        # check if inside
        @contours =
            grep {
                my $poly = $_;
                my $is_inside = $bound->{tree}->contains_polygon_rough($poly, inaccurate=>1);
                $is_inside = $bound->{tree}->contains_points($poly)
                    if !defined $is_inside && $is_clipping && $flags->{less_gpc};
                $need_to_clip = 1  if !defined $is_inside && $is_clipping;
                !defined $is_inside || $is_inside;
            }
            @contours;

        # clip if necessary
        if ( $need_to_clip ) {
            my $gpc = Clipper->new();
            $gpc->add_polygon($_, 0)  for @{$param->{areas}};
            $gpc->add_polygon($_, 1)  for @{$param->{holes} || []};

            @contours = $gpc->clip_to($bound->{gpc}, 'INTERSECT')->get_polygons();
        }
    }

    # sort contours by its area
    my %area;
    @contours =
        sort {
            my $aa = $area{$a} //= Math::Polygon::Calc::polygon_area(@$a);
            my $bb = $area{$b} //= Math::Polygon::Calc::polygon_area(@$b);
            $bb <=> $aa;
        }
        grep { @$_ >= 3 }
        map { push @$_, $_->[0] unless $_->[0] ~~ $_->[-1]; $_ } # closing contours
        @contours;

    return  if !@contours;

    ## Navitel polygon addressing
    if ( $flags->{navitel} && $param->{tags}->{'addr:housenumber'} ) {
        $param->{address} = _get_address($obj, point => $contours[0]->[0]);
    }


    # rearrange contours
    my @polygons;
    while ( @contours ) {
        my @polygon = ( shift @contours );
        @contours =
            grep {
                my $is_inner = Math::Polygon::Tree::polygon_contains_point( $_->[0], $polygon[0] );
                push @polygon, $_  if $is_inner;
                !$is_inner;
            }
            @contours;
        push @polygons, \@polygon;
    }

    for my $polygon ( @polygons ) {
        $param->{contours} = $polygon;
        $writer->output( polygon => { data => $param } );
    }

    return;
}




####    Config processing

sub cond_is_inside_city {
    my ($obj) = @_;

    state $cache = {};

    if ( $obj->{coords} ) {
        return !!$cityside_area->find_area($obj->{coords});
    }
    elsif ( $obj->{type} && $obj->{id} ) {
        my $key = "$obj->{type}_$obj->{id}";
        return $cache->{$key} //= !!_is_object_inside_city($obj);
    }
    
    croak "Invalid object:\n".Dump $obj;
    return;
}


sub _is_object_inside_city {
    my ($obj) = @_;

    return $cityside_area->find_area( $osm->get_lonlat($obj->{id}) )  if $obj->{type} eq 'Node';
    
    if ( $obj->{type} eq 'Way' ) {
        my $chain = $obj->{chain} || $obj->{outer}->[0];
        my ($p1, $p2) =
            map { $osm->get_lonlat($chain->[$_]) }
            ( floor($#$chain/3), ceil($#$chain*2/3) );
        return $cityside_area->find_area($p1) && $cityside_area->find_area($p2);
    }

    return;
}


sub cond_is_named {
    my ($obj) = @_;
    return !!( name_from_list( label => $obj->{tag} ) );
}



sub _get_line_parts_inside_bounds {
    my ($chain) = @_;
    return $chain  if !$bound;

    my @is_inside = map { is_inside_bounds($_) } @$chain;

    my @begin = grep { $is_inside[$_] && ( $_ == 0        || !$is_inside[$_-1] ) } ( 0 .. $#$chain );
    my @end   = grep { $is_inside[$_] && ( $_ == $#$chain || !$is_inside[$_+1] ) } ( 0 .. $#$chain );

    return
        grep { @$_ > 1 }
        map {[ @$chain[
            ( $begin[$_] == 0      || $is_inside[$begin[$_]] < 0 ? $begin[$_] : $begin[$_]-1 )
            ..
            ( $end[$_] == $#$chain || $is_inside[$end[$_]] < 0   ? $end[$_]   : $end[$_]+1 )
        ] ]}
        (0 .. $#end);
}


sub action_load_coastline {
    my ($obj, $action) = @_;
    return if !$flags->{shorelines};
    return if !$obj->{chain};

    my @parts = _get_line_parts_inside_bounds( $obj->{chain} );
    for my $part ( @parts ) {
        $coast->add_coastline( $osm->get_lonlat($part) );
    }
    return;
}


# calculate config field value

sub _get_field_by_template_string {
    my ($template, $obj, %opt) = @_;

    my $failed;
    $template =~ s[%(\w+)][
        my $r = name_from_list($1, $obj->{tag});
        $failed++ if !defined $r;
        $r // q{}
    ]ge;

    return undef  if $failed && $opt{empty_failed};
    return undef  if !defined $template || !length $template;
    return $template;
}


sub _get_field_by_tag {
    my ($selector, $obj, %opt) = @_;

    my $tag = $selector->{tag};
    croak "No tag defined in 'tag' selector"  if !$tag;
    my $value = $obj->{tag}->{$tag};

    my $selected = ( defined $value ? $selector->{$value} : $selector->{_empty} ) // $selector->{_default};

    return undef  if !defined $selected;
    return _get_field_content($selected, $obj, %opt);
}


sub _get_field_by_threshold {
    my ($selector, $obj, %opt) = @_;

    croak "No value in 'threshold' selector"  if !$selector->{value};
    my $value = extract_number(_get_field_content($selector->{value}, $obj, %opt)) || 0;

    my @tholds =
        sort { $a <=> $b }
        grep {!( $_ ~~ [qw/ selector value /] )}
        keys %$selector;
    croak "No threshold values defined in 'threshold' selector" if !@tholds;

    my $thold = last_value { $value >= $_ } @tholds;
    return undef if !defined $thold;
    
    return $selector->{$thold};
}


sub _get_obj_area_size {
    my ($obj) = @_;

    return 0  if !$obj->{outer};

    my $area_size =
        sum
        map {
            Math::Polygon::Calc::polygon_area(@$_)
            * cos( polygon_centroid($_)->[1] / 180 * 3.14159 )
            * (40000/360)**2
        }
        map { $osm->get_lonlat($_) }
        @{ $obj->{outer} };

    return $area_size;
}


sub _get_field_by_condition {
    my ($selector, $obj, %opt) = @_;

    croak "No condition in 'if' selector"  if !$selector->{condition};
    if ( $ft_config->check_condition($selector->{condition}, $obj) ) {
        croak "No 'then' key in 'if' selector"  if !exists $selector->{then};
        my $then = $selector->{then};
        return $then if !defined $then;
        return _get_field_content($then, $obj, %opt);
    }
    elsif ( defined( my $else = $selector->{else} ) ) {
        return _get_field_content($else, $obj, %opt);
    }

    return undef;
}


sub _get_field_by_lang {
    my ($selector, $obj, %opt) = @_;

    my $selected =
        $selector->{ $values->{target_lang} || 'en' }
        || $selector->{en}
        || first { $_ ne 'selector' && $selector->{$_} } keys %$selector;
    croak "Bad lang selector"  if !$selected;

    return _get_field_content($selected, $obj, %opt);
}


BEGIN {
    my %selector = (
        lang => \&_get_field_by_lang,
        if   => \&_get_field_by_condition,
        tag  => \&_get_field_by_tag,
        thresholds => \&_get_field_by_threshold,
    );

sub _get_field_content {
    my ($field, $obj, %opt) = @_;

    return $field if !defined $field || !length $field;

    for ( ref $field ) {
        # string: resolve templates
        when (q{}) {
            return _get_field_by_template_string($field, $obj, %opt);
        }
        # sub: execute 
        when ('CODE') {
            return $field->($obj);
        }
        # array: select first succeed result
        when ('ARRAY') {
            for my $subfield ( @$field ) {
                my $text = _get_field_content($subfield, $obj, empty_failed => 1);
                return $text  if defined $text && length $text;
            }
            return undef;
        }
        # special selector, based on 'selector' key
        when ('HASH') {
            my $selector_key = $field->{selector};
            croak 'Undefined selector key'  if !$selector_key;
            my $selector_sub = $selector{$selector_key};
            croak "Unknown selector '$selector_key'"  if !$selector_sub;

            return $selector_sub->($field, $obj, %opt);
        }

        # unknown
        say STDERR Dump \@_;
        confess "Bad field type: $_";
    }
}
}


sub _get_result_object_params {
    my ($obj, $action) = @_;

    $action->{name} = '%label'  if !exists $action->{name};

    my %info = %$action;

    state $fields = [ qw/
        name type level_l level_h
        routeparams road_ref
        oneway toll reverted
    / ];
    for my $key ( @$fields ) {
        next if !defined $info{$key};
        $info{$key} = _get_field_content($info{$key}, $obj);
    }

    # skip objects with undefined type
    return if !defined $info{type} || !length $info{type};

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

    # road refs
    if ( $flags->{road_shields} && $info{road_ref} ) {
        my @refs =
            uniq sort
            map { my $ref = $_; $ref =~ s/[\s\-]//gx; split /[,;]/, $ref }
            grep { $_ } (
                $obj->{tag}->{ref},
                @{ $road_ref{ $obj->{id} } || [] }
            );
        $info{refs} = \@refs  if @refs;
    }

    return \%info;
}


sub action_write_line {
    my ($obj, $action) = @_;

    my @parts =
        map { _get_line_parts_inside_bounds($_) }
        ( $obj->{chain} || ( @{$obj->{outer}}, @{ $obj->{inner} || [] } ) );
    return if !@parts;

    my $info = _get_result_object_params($obj, $action);
    return if !$info;

    for my $part ( @parts ) {
        $info->{chain} = $part;
        output_line( $info );
    }
    return;
}


sub action_load_road {
    return action_write_line(@_)  if !$flags->{routing};

    my ($obj, $action) = @_;
    return if !$obj->{chain};

    my $id = $obj->{id};
    my @parts = map { _get_line_parts_inside_bounds($_) } $obj->{chain};
    return if !@parts;

    my $info = _get_result_object_params($obj, $action);
    return if !$info;

    for my $part_no ( 0 .. $#parts ) {
        $info->{chain} = $parts[$part_no];
        $info->{id}    = "$id:$part_no";
        AddRoad( $info );
    }
    return;
}


sub action_process_interpolation {
    my ($obj, $action) = @_;
    return if !$flags->{addr_interpolation};
    return if !$obj->{chain};

    my $id = $obj->{id};

    my @parts = map { _get_line_parts_inside_bounds($_) } $obj->{chain};
    return if !@parts;

    for my $part ( @parts ) {
        my @chain = grep { my $t = $osm->get_tags(node => $_); $t && exists $t->{'addr:housenumber'} } @$part;
        next if @chain < 2;

        my $new_action = { %$action, action => 'write_poi' };
        for my $i ( 0 .. $#chain-1 ) {
            my ( $node1, $node2 ) = @chain[ $i, $i+1 ];
            my ( $house1, $house2 ) =
                map { my $x = $osm->get_tags(node => $_)->{'addr:housenumber'}; $x =~ s/^(\d+).*/$1/x; $x }
                ( $node1, $node2 );
            next if $house1 == $house2;

            my %tag = ( %{$osm->get_tags(node => $node2)}, %{$osm->get_tags(node => $node1)}, %{$obj->{tag}} );
            my $step = ( $tag{'addr:interpolation'} eq 'all' ? 1 : 2 );
            $step *= -1  if $house1 > $house2;

            my ($lon1, $lat1) = @{ $osm->get_lonlat($node1) };
            my ($lon2, $lat2) = @{ $osm->get_lonlat($node2) };
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
                    coords  => [ $clon, $clat ],
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
    return if !$obj->{outer};

    my $info = _get_result_object_params($obj, $action);
    return if !$info;

    $info->{areas} = [ map { $osm->get_lonlat($_) } @{$obj->{outer}} ];
    $info->{holes} = [ map { $osm->get_lonlat($_) } @{$obj->{inner} || []} ];
    $info->{entrance} = [
        map { [ $osm->get_lonlat($_), $entrance{$_} ] }
        grep { exists $entrance{$_} }
        map { @$_ } map { @{ $_ || [] } } ( $obj->{outer}, $obj->{inner} )
    ];

    output_area( $info, $obj );
    return;
}


sub action_address_poi {
    my ($obj, $action) = @_;

    return if !$obj->{outer};
    return if !exists $poi_rtree->{root};

    my $outer_chain = $obj->{outer}->[0];
    my $outer = $osm->get_lonlat($outer_chain);
    my $bbox = polygon_bbox($outer);

    my @poilist;
    $poi_rtree->query_completely_within_rect( @$bbox, \@poilist );

    for my $id ( @poilist ) {
        next unless exists $poi{$id};
        next unless polygon_contains_point($osm->get_lonlat($id), $outer);

        my $house_address = $addresser->get_address_tags($obj->{tag});

        for my $poiobj ( @{ $poi{$id} } ) {
            $poiobj->{tags} = Utils::hash_merge( $house_address, $poiobj->{tags} );
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
    return if !$info;

    my $coords;
    {
        if ( $obj->{coords} ) {
            $coords = $obj->{coords};
            last;
        }

        if ( $obj->{type} eq 'Node' ) {
            $coords = $osm->get_lonlat($obj->{id});
            last;
        }

        if ( $obj->{type} eq 'Way' ) {
            # for areas: place poi at main entrance if exists
            if ( $obj->{outer} ) {
                my $entrance_node = first { exists $main_entrance{$_} } map {@$_} @{$obj->{outer}};
                if ( $entrance_node ) {
                    $coords = $osm->get_lonlat($entrance_node);
                    last;
                }
            }

            $coords = polygon_centroid($osm->get_lonlat( $obj->{chain} || $obj->{outer}->[0] ));
            last;
        }
    }

    return if !$coords;
    return if !is_inside_bounds( $coords );

    $info->{coords} = $coords;

    if ( $flags->{addr_from_poly}
        && $obj->{type} eq 'Node' # ???
        && $info->{contacts}
        && (!defined $info->{inherit_address} || $info->{inherit_address})
    ) {
        # save poi for addressing
        my $id = $obj->{id};
        my @bbox = ( (@$coords) x 2 );
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
    return if !$obj->{outer};

    my @outers = map { $osm->get_lonlat($_) } @{$obj->{outer}};
    my @inners = map { $osm->get_lonlat($_) } @{$obj->{inner}};
    $calc_access->add_area( $obj->{tag}, \@outers, \@inners ); 
    
    return;
}


sub action_load_cityside_area {
    my ($obj, $action) = @_;
    return if !$obj->{outer};

    my @outers = map { $osm->get_lonlat($_) } @{$obj->{outer}};
    my @inners = map { $osm->get_lonlat($_) } @{$obj->{inner}};
    $cityside_area->add_area( 1, \@outers, \@inners ); 
    
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
    my ($number) = $str =~ /^ ( [-+]? \d+ (?: \. \d*)? ) /x;
    return $number;
}



=head2 _get_address( $obj, %opt )

Options:
  * city (don't search again)
  * point
  * tag (if no 'tag' field in $obj)
  * street (main street name)

=cut

sub _get_address {

    my ($obj, %opt) = @_;

    my $tags = $opt{tag} || $opt{tags} || $obj->{tag} || $obj->{tags} || {};
    my @points =
        map { ref $_ ? $_ : $osm->get_lonlat($_) }
        grep { $_ }
        ( $opt{point}, @{ $opt{points} || [] } );

    my $address_tags = $addresser->get_address_tags( $tags,
        level => $opt{level},
        ( @points ? (points => \@points) : () ),
    );

    my $address = $addresser->get_lang_address($address_tags, $lang_select);

    return $address;
}





