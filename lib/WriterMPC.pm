package WriterMPC;

# ABSTRACT: garmin shapefile writer

# $Id$


use 5.010;
use strict;
use warnings;
use autodie;

use Carp;

use Encode;
use List::MoreUtils qw/ uniq /;
use Math::Polygon;

use Geo::Shapefile::Writer;
use TextFilter;
use GarminTools;

use YAML;


our @COMMON_ATTRS = (
    [ NAME => 'C', 120 ],
    [ GRMN_TYPE => 'C', 32 ],
);

our %ATTRS = (
    points => [
        @COMMON_ATTRS,
        [ STRT_ADDR => 'C', 64 ],
        [ CITY => 'C', 64 ],
        [ STATE => 'C', 64 ],
        [ COUNTRY => 'C', 64 ],
        [ PCODE => 'C', 64 ],
        [ PHONE => 'C', 64 ],
    ],
    areas => [
        @COMMON_ATTRS,
        [ HGT_DP_FMT => 'C', 3 ],
        [ HGT_DP => 'N', 5 ],
    ],
    lines => \@COMMON_ATTRS,
    roads => [
        @COMMON_ATTRS,
        [ ROUTE_LVL   => 'N', 1 ],
        [ SPD_LIMIT   => 'N', 3 ],
        [ SPD_FORMAT  => 'N', 1 ],
        [ IS_LGL_SPD  => 'N', 1 ],
        [ ROUTE_SPD   => 'N', 3 ],
        [ ONE_WAY     => 'N', 1 ],
        [ TOLL_ROAD   => 'N', 1 ],
        [ LINK_ID     => 'N' ],
        [ ACC_MASK    => 'C', 10 ],
        [ CNTRL_ACC   => 'N', 1 ],
        [ IS_TUNNEL   => 'N', 1 ],

        [ L_CITY      => 'C', 64 ],
        [ R_CITY      => 'C', 64 ],
        [ L_STATE     => 'C', 64 ],
        [ R_STATE     => 'C', 64 ],
        [ L_COUNTRY   => 'C', 64 ],
        [ R_COUNTRY   => 'C', 64 ],
        [ L_COUNTRY   => 'C', 8 ],
        [ R_COUNTRY   => 'C', 8 ],
    ],
);


our %MP2SHP = _init_code_table();


=method new( param => $value )

Create writer instance
Options:
    codepage

=cut

sub new {
    my ($class, %opt) = @_;
    
    my $self = bless {}, $class;

    ##  Encoding
    $self->_register_codepage( $opt{codepage} );

    return $self;
}




=method output( $template, $data )

=cut

{
my %writer = (
    section => undef,
    info    => undef,

    point       => \&_write_point,
    polygon     => \&_write_polygon,
    polyline    => \&_write_polyline,
    road =>     => \&_write_road_polyline,
    
    # !!! to remove
    turn_restriction => undef,
    destination_sign => undef,
);

sub output {
    my ( $self, $base, $data ) = @_;

    my $writer = $writer{$base};
    return if !$writer;

    $self->{_count}->{$base} ++;
    $writer->($self, $data);
    return;
}
}



sub _get_shp {
    my ($self, $name, $type) = @_;

    my $shp = $self->{shp}->{$name} ||= do {
        my $prefix = q{};
        $prefix .= $self->{output_base}  if $self->{output_base};
        $prefix .= q{.}  if $prefix && $prefix !~ m#[\\/]$#x;

        Geo::Shapefile::Writer->new("$prefix$name", $type, @{ $ATTRS{$name} } );
    };

    return $shp;
}




{
my %addr_field = (
    STRT_ADDR   => [ q{, } => qw/ street house / ],
    CITY        => 'city',
    STATE       => 'region',
    COUNTRY     => 'country',
    PCODE       => 'postcode',
);

sub _write_point {
    my ($self, $vars) = @_;
    my $data = $vars->{data} || {};

    my $shp = $self->_get_shp( 'points', 'POINT' );
    my $type = $MP2SHP{1}->{lc $data->{type}} // $data->{type};
    carp "Unknown point type: $type"  if !$type || $type =~ /^0/;

    my %record = (
        NAME => $data->{name},
        GRMN_TYPE => $type,
        %{ $data->{extra_fields} },
    );

    if ( my $addr = $data->{address} ) {
        my $garmin_address = GarminTools::get_garmin_address($addr);
        while ( my ($field, $info) = each %addr_field ) {
            if ( ref $info ) {
                my ($sep, @fields) = @$info;
                $record{$field} = join $sep, grep {$_} map {$garmin_address->{$_}} @fields;
            }
            else {
                $record{$field} = $garmin_address->{$info};
            }
        }
    }

    $shp->add_shape(
        $data->{coords},
        { map {( $_ => encode( $self->{codepage}, $record{$_} ) )} keys %record },
    );
    return;
}
}

{
my %addr_field = (
    L_CITY      => 'city',
    R_CITY      => 'city',
    L_STATE     => 'region',
    R_STATE     => 'region',
    L_COUNTRY   => 'country',
    R_COUNTRY   => 'country',
    L_PCODE     => 'postcode',
    R_PCODE     => 'postcode',
);

sub _write_road_polyline {
    my ($self, $vars) = @_;
    my $data = $vars->{data} || {};

    return if !@{$data->{chain}};

    my $shp = $self->_get_shp( roads => 'POLYLINE' );
    my $type = $MP2SHP{3}->{lc $data->{type}} // $data->{type};
    carp "Unknown routable type: $type"  if $type =~ /^0/;

    # !!! convert from  mp-compatible access mask
    # emergency,delivery,car,bus,taxi,foot,bike,truck
    my @acc_flags = map {$_ // 0} (split /,/x, $data->{access_flags})[2,3,4,9,5,6,7,9,1,0];

    state $mp_ref = { '~[0x04]' => '{M', '~[0x05]' => '{P', '~[0x06]' => '{O' };
    my $ref_prefix = $data->{road_ref} && $data->{refs}
        ? ( $mp_ref->{$data->{road_ref}} // $data->{road_ref} ) . join q{-}, sort uniq @{$data->{refs}}
        : undef;

    my %record = (
        NAME => join( q{ }, grep { defined && length } ( $ref_prefix, $data->{name} ) ),
        GRMN_TYPE   => $type,
        ROUTE_LVL   => ($data->{road_class} // -1) + 1,
        ROUTE_SPD   => $data->{speed},
        SPD_FORMAT  => 1, # km/h
        ONE_WAY     => $data->{oneway},
        TOLL_ROAD   => $data->{toll},
        LINK_ID     => $data->{road_id},
        ACC_MASK    => join( q{}, @acc_flags ),
        %{ $data->{extra_fields} || {} },
    );

    # old-style speed limit
    $record{SPD_LIMIT} = delete $record{ROUTE_SPD}  if !( $record{IS_LGL_SPD} ~~ [qw/ 1 Y T /] );

    if ( my $addr = $data->{address} ) {
        my $garmin_address = GarminTools::get_garmin_address($addr);
        while ( my ($field, $info) = each %addr_field ) {
            $record{$field} = $garmin_address->{$info};
        }
    }

#    use YAML; say Dump $vars, \%record; exit;

    $shp->add_shape(
        [ $data->{chain} ],
        { map {( $_ => encode( $self->{codepage}, $record{$_} ) )} keys %record },
    );
    return;
}
}


sub _write_polygon {
    my ($self, $vars) = @_;
    my $data = $vars->{data} || {};

    return if !@{$data->{contours}};

    my $shp = $self->_get_shp( 'areas', 'POLYGON' );
    my $type = $MP2SHP{5}->{lc $data->{type}} // $data->{type};
    carp "Unknown area type: $type"  if $type =~ /^0/;

    my %record = (
        NAME => $data->{name},
        GRMN_TYPE => $type,
        %{ $data->{extra_fields} || {} },
    );

    # assume first contour is outer and all others are inners
    # fix direction
    my $is_outer = 1;
    my @rings = map {
            my $need_cw = $is_outer;
            $is_outer = 0;
            my $is_cw = Math::Polygon->new(@$_)->isClockwise();
            $is_cw == $need_cw ? $_ : [ reverse @$_ ];
        } @{$data->{contours}};

    $shp->add_shape(
        \@rings,
        { map {( $_ => encode( $self->{codepage}, $record{$_} ) )} keys %record },
    );
    return;
}


sub _write_polyline {
    my ($self, $vars) = @_;
    my $data = $vars->{data} || {};

    return if !@{$data->{chain}};

    my $shp = $self->_get_shp( lines => 'POLYLINE' );
    my $type = $MP2SHP{3}->{lc $data->{type}} // $data->{type};
    carp "Unknown line type: $type"  if $type =~ /^0/;

    my %record = (
        NAME => $data->{name},
        GRMN_TYPE => $type,
        %{ $data->{extra_fields} || {} },
    );

    $shp->add_shape(
        [ $data->{chain} ],
        { map {( $_ => encode( $self->{codepage}, $record{$_} ) )} keys %record },
    );
    return;
}



=method finalize()

=cut

sub finalize {
    my ($self) = @_;

    for my $shp ( values %{ $self->{shp} } ) {
        $shp->finalize();
    }

    return;
}


{
my %enc_to_cp = (
    ( map {($_ => 65001)} qw/ utf8 utf-8 / ),
    ( map {/(cp(\d+))/xms}  grep {/^cp\d{3,}$/xms} Encode->encodings(':all') ),
);
my %cp_to_enc = reverse %enc_to_cp;

sub _register_codepage {
    my $self = shift;
    my $cp = lc( shift || 'utf8' );

    if ( $enc_to_cp{$cp} ) {
        $self->{codepage} = $cp;
    }
    elsif ( $cp_to_enc{$cp} ) {
        $self->{codepage} = $cp_to_enc{$cp};
    }
    else {
        croak "Unknown code page: $cp";
    }
    return;
}
}




=method get_getopt()

=cut

sub get_getopt {
    my ($self) = @_;
    return (
        'o|output=s'            => \$self->{output_base},
        'codepage=s'            => sub { $self->_register_codepage( $_[1] ) },
    );
}


=method get_usage()

=cut

sub get_usage {
    my ($self) = @_;
    return (
        [ 'o|output' => 'output file' ],
        [ 'codepage <num>' => 'output character encoding', $self->{codepage} ],
    );
}


sub _init_code_table {
    my %table;
    for my $line (<DATA>) {
        my ($mp_code, $type, $grmn_code) = split /\s+/x, $line;
        $table{$type}->{lc $mp_code} = $grmn_code;
    }
    return %table;
}

1;



# mp -> garmin type translation
# taken from vasyusya's _mp2shp distribution 
__DATA__


0x0100  1   LARGE_CITY
0x0200  1   MEDIUM_CITY
0x0300  1   CITY_2M
0x0400  1   CITY_1M
0x0500  1   CITY_500K
0x0600  1   CITY_200K
0x0700  1   CITY_100K
0x0800  1   CITY_50K
0x0900  1   CITY_20K
0x0a00  1   CITY_10K
0x0b00  1   CITY_5K
0x0c00  1   CITY_LT5K
0x0d00  1   CITY_UNKNOWN
0x1400  1   MAJOR_COUNTRY
0x1500  1   MINOR_COUNTRY
0x1e00  1   STATE
0x1f00  1   COUNTY

0x2800  1   ISLAND

0x2a00  1   RESTAURANT
0x2a01  1   RESTAURANT_AMERICAN
0x2a02  1   RESTAURANT_ASIAN
0x2a03  1   RESTAURANT_BARBECUE
0x2a04  1   RESTAURANT_CHINESE
0x2a05  1   RESTAURANT_DELI
0x2a06  1   RESTAURANT_INTRNTNL
0x2a07  1   RESTAURANT_FAST_FOOD
0x2a08  1   RESTAURANT_ITALIAN
0x2a09  1   RESTAURANT_MEXICAN
0x2a0a  1   RESTAURANT_PIZZA
0x2a0b  1   RESTAURANT_SEAFOOD
0x2a0c  1   RESTAURANT_STEAK
0x2a0d  1   RESTAURANT_DONUTS
0x2a0e  1   RESTAURANT_CAFES
0x2a0f  1   RESTAURANT_FRENCH
0x2a10  1   RESTAURANT_GERMAN
0x2a11  1   RESTAURANT_BRITISH

0x2b01  1   HOTEL
0x2b02  1   BED_AND_BREAKFAST
0x2b03  1   CAMPGROUND
0x2b04  1   RESORT

0x2c01  1   THEME_PARK
0x2c02  1   MUSEUM
0x2c03  1   LIBRARY
0x2c04  1   LANDMARK
0x2c05  1   SCHOOL
0x2c06  1   PARK
0x2c07  1   ZOO
0x2c08  1   ARENA
0x2c09  1   HALL
0x2c0a  1   WINERY
0x2c0b  1   PLACE_OF_WORSHIP
0x2C0D  1 POW_MOSQUE
0x2C0E  1 POW_CHURCH
0x2C0F  1 POW_TEMPLE
0x2C10  1 POW_SYNAGOGUE

0x2d01  1   LIVE_THEATER
0x2d02  1   BAR
0x2d03  1   MOVIE_THEATER
0x2d04  1   CASINO
0x2d05  1   GOLF_COURSE
0x2d06  1   SKI_CENTER
0x2d07  1   BOWLING
0x2d08  1   ICE_SKATING
0x2d09  1   SWIMMING_POOL
0x2d0a  1   FITNESS_CENTER
0x2d0b  1   SPORT_AIRPORT

0x2e01  1   DEPARTMENT_STORE
0x2e02  1   GROCERY_STORE
0x2e03  1   GENERAL_STORE
0x2e04  1   SHOPPING_CENTER
0x2e05  1   PHARMACY
0x2e06  1   CONVENIENCE_STORE
0x2e07  1   CLOTHING_RETAIL
0x2e08  1   HOME_AND_GARDEN_STORE
0x2e09  1   HOME_FURNISHINGS_STORE
0x2e0a  1   SPECIALTY_RETAIL
0x2e0b  1   SOFTWARE_RETAIL

0x2f01  1   GAS_STATION
0x2f02  1   AUTO_RENTAL
0x2f03  1   AUTO_REPAIR
0x2f04  1   AIRPORT
0x2f05  1   POST_OFFICE
0x2f06  1   BANK
0x2f07  1   DEALER
0x2f08  1   GND_TRANSPORT
0x2f09  1   MARINA
0x2f0a  1   WRECKER_SERVICE
0x2f0b  1   PARKING
0x2f0c  1   REST_AREA_TOURIST_INFO
0x2f0d  1   AUTO_CLUB
0x2f0e  1   CAR_WASH
0x2f0f  1   GARMIN_DEALERS
0x2f10  1   SERVICES_PERSONAL
0x2f11  1   SERVICES_BUSINESS
0x2f12  1   COMMUNICATION_SERVICES
0x2f13  1   REPAIR_SERVICE
0x2f14  1   SOCIAL_SERVICES
0x2f15  1   UTILITY
0x2f16  1   TRUCK_STOP
0x2f17  1   TRANSIT_SERVICES

0x3001  1   POLICE_STATION
0x3002  1   HOSPITAL
0x3003  1   CITY_HALL
0x3004  1   COURTHOUSE
0x3005  1   COMMUNITY_CENTER
0x3006  1   BORDER_CROSSING
0x3007  1   GOV_OFFICE
0x3008  1   FIRE_DEPT



0x4000  1   GOLF_COURSE
0x4000  1   GOLF_COURSE
0x4100  1   FISHING_SPOT
0x4100  1   FISHING_SPOT
0x4300  1   MARINA
0x4300  1   MARINA
0x4500  1   RESTAURANT
0x4500  1   RESTAURANT
0x4600  1   BAR
0x4600  1   BAR
0x4700  1   BOAT_RAMP
0x4700  1   BOAT_RAMP
0x4800  1   CAMPGROUND
0x4800  1   CAMPGROUND
0x4900  1   PARK
0x4900  1   PARK
0x4a00  1   PICNIC_AREA
0x4a00  1   PICNIC_AREA
0x4b00  1   FIRST_AID
0x4b00  1   FIRST_AID
0x4c00  1   INFORMATION
0x4c00  1   INFORMATION
0x4d00  1   PARKING
0x4d00  1   PARKING
0x4e00  1   RESTROOMS
0x4e00  1   RESTROOMS
0x4f00  1   SHOWERS
0x5000  1   DRINKING_WATER
0x5000  1   DRINKING_WATER
0x5100  1   PHONE
0x5100  1   PHONE
0x5200  1   SCENIC_AREA
0x5200  1   SCENIC_AREA
0x5300  1   SKI_CENTER
0x5300  1   SKI_CENTER
0x5400  1   SWIMMING_AREA
0x5400  1   SWIMMING_AREA
0x5500  1   DAM
0x5500  1   DAM
0x5900  1   AIRPORT
0x5900  1   AIRPORT
0x5901  1   AIRPORT
0x5901  1   AIRPORT
0x5c00  1   DIVING_AREA
0x5c00  1   DIVING_AREA
0x6400  1   BUILDING
0x6400  1   BUILDING
0x6401  1   BRIDGE
0x6401  1   BRIDGE
0x6403  1   CEMETERY
0x6403  1   CEMETERY
0x6404  1   CHURCH
0x6404  1   CHURCH
0x6405  1   HOUSE
0x6408  1   HOSPITAL
0x6408  1   HOSPITAL
0x640b  1   PILLAR
0x640c  1   MINE
0x640d  1   OILFIELD
0x640d  1   OILFIELD
0x640e  1   PARK
0x640e  1   PARK
0x640f  1   POST_OFFICE
0x640f  1   POST_OFFICE
0x6411  1   TOWER
0x6411  1   TOWER
0x6412  1   TRAIL
0x6412  1   TRAIL
0x6413  1   TUNNEL
0x6413  1   TUNNEL
0x6414  1   WELL
0x6414  1   WELL
0x6508  1   WATERFALL
0x6508  1   WATERFALL
0x6509  1   GEYSER
0x6509  1   GEYSER
0x650c  1   ISLAND
0x650c  1   ISLAND
0x6511  1   SPRING
0x6511  1   SPRING
0x6512  1   STREAM
0x6512  1   STREAM
0x6600  1   CUSTOMIZABLE_POINT_6
0x6600  1   CUSTOMIZABLE_POINT_6
0x6604  1   BEACH
0x6605  1   BENCH
0x6605  1   BENCH
0x6606  1   CAPE
0x6606  1   CAPE
0x660f  1   PILLAR
0x660f  1   PILLAR
0x6610  1   PLAIN
0x6610  1   PLAIN
0x6611  1   RANGE
0x6611  1   RANGE
0x6612  1   RESERVE
0x6612  1   RESERVE
0x6613  1   RIDGE
0x6613  1   RIDGE
0x6614  1   ROCK
0x6614  1   ROCK
0x6616  1   SUMMIT
0x6616  1   SUMMIT


0x00 3   RESIDENTIAL
0x01 3   MAJOR_HWY
0x01 5   LARGE_CITY
0x02 3   PRINCIPAL_HWY
0x02 5   SMALL_CITY
0x03 3   OTHER_HWY
0x03 5   TOWN
0x04 3   ARTERIAL
0x04 5   MILITARY_BASE
0x05 3   COLLECTOR
0x05 5   PARKING_LOT
0x06 3   RESIDENTIAL
0x06 5   PARKING_GARAGE
0x07 3   ALLEY
0x07 5   AIRPORT
0x08 3   LOW_SPEED_RAMP
0x08 5   SHOPPING_AREA
0x09 3   HIGH_SPEED_RAMP
0x09 5   MARINA
0x0a 3   UNPAVED_ROAD
0x0a 5   COLLEGE
0x0b 3   MAJOR_CONNECTOR
0x0b 5   HOSPITAL
0x0c 3   ROUNDABOUT
0x0c 5   INDUSTRIAL_COMPLEX
0x0d 5   RESERVATION
0x0e 5   AIRPORT_RUNWAYS
0x13    5   GENERIC_MANMADE
0x14    3   RAILROAD
0x14    5   NATIONAL_PARK
0x15    3   SHORELINE
0x16    3   TRAIL
0x17    5   URBAN_PARK
0x18    3   STREAM
0x18    5   GOLF_COURSE
0x19    5   SPORTS_COMPLEX
0x1a    3   FERRY
0x1a    5   CEMETARY
0x1e    3   INTRN_PLTCL_BDRY
0x1e    5   STATE_PARK
0x1f    3   RIVER
0x1f    5   STATE_PARK
0x20    3   MINOR_CONTOUR
0x20    5   STATE_PARK
0x21    3   INT_CONTOUR
0x22    3   MAJOR_CONTOUR
0x23    3   MINOR_BATHY_CONTOUR
0x26    3   INTERMITTENT_STREAM
0x28    3   PIPELINE
0x29    5   LAKE
0x32    5   SEA
0x3b    5   SMALL_LAKE
0x3c    5   LARGE_LAKE
0x3d    5   LARGE_LAKE
0x3e    5   LAKE
0x3f    3   CUSTOMIZABLE_LINE_1
0x3f    5   LAKE
0x40    5   SMALL_LAKE
0x41    3   CUSTOMIZABLE_LINE_1
0x41    5   SMALL_LAKE
0x42    3   UNPAVED_ROAD
0x43    3   CUSTOMIZABLE_ROUTE_LINE_1
0x44    3   RIVER
0x45    3   MNR_PLTCL_BDRY
0x45    5   LAKE
0x46    3   MISC_LINE
0x46    5   LARGE_RIVER
0x47    3   CUSTOMIZABLE_LINE_2
0x47    5   LARGE_RIVER
0x48    3   UNPAVED_ROAD
0x48    5   SMALL_RIVER
0x49    5   SMALL_RIVER
0x4b    5   DATA_BOUNDS
0x4e    5   ORCHARD
0x4f    5   SCRUB
0x50    5   WOODS
0x51    5   WETLAND
0x53    5   FLAT
0x68    5   GENERIC_MANMADE
0x69    5   GENERIC_MANMADE
0x6a    5   MISC_AREA
0x6b    5   MISC_AREA
0x6c    5   GENERIC_MANMADE
0x6d    5   GENERIC_MANMADE
0x6e    5   GENERIC_MANMADE
0x6f    5   GENERIC_MANMADE
0x78    5   MISC_AREA
0x7a    5   MISC_AREA
0x7b    5   MISC_AREA
0x81    5   WETLAND
0x83    5   WOODS
0x85    5   WOODS
0x88    5   LAND
0x89    5   FLAT
0x8a    5   LAND
0x8c    5   INTERMITTENT_LAKE
0x8d    5   MARINE_MISC_AREA
0x95    5   ORCHARD
0x98    5   LAND



0x00    3   RESIDENTIAL
0x01    3   MAJOR_HWY
0x01    5   LARGE_CITY
0x02    3   PRINCIPAL_HWY
0x02    5   SMALL_CITY
0x03    3   OTHER_HWY
0x03    5   TOWN
0x04    3   ARTERIAL
0x04    5   MILITARY_BASE
0x05    3   COLLECTOR
0x05    5   PARKING_LOT
0x06    3   RESIDENTIAL
0x06    5   PARKING_GARAGE
0x07    3   ALLEY
0x07    5   AIRPORT
0x08    3   LOW_SPEED_RAMP
0x08    5   SHOPPING_AREA
0x09    3   HIGH_SPEED_RAMP
0x09    5   MARINA
0x0a    3   UNPAVED_ROAD
0x0a    5   COLLEGE
0x0b    3   MAJOR_CONNECTOR
0x0b    5   HOSPITAL
0x0c    3   ROUNDABOUT
0x0c    5   INDUSTRIAL_COMPLEX
0x0d    5   RESERVATION
0x0e    5   AIRPORT_RUNWAYS
0x13    5   GENERIC_MANMADE
0x14    3   RAILROAD
0x14    5   NATIONAL_PARK
0x15    3   SHORELINE
0x16    3   TRAIL
0x1c    3   MJR_PLTCL_BDRY
0x17    5   URBAN_PARK
0x18    3   STREAM
0x18    5   GOLF_COURSE
0x19    5   SPORTS_COMPLEX
0x1a    3   FERRY
0x1d    3   MNR_PLTCL_BDRY
0x1a    5   CEMETARY
0x1e    3   INTRN_PLTCL_BDRY
0x1e    5   STATE_PARK
0x1f    3   RIVER
0x1f    5   STATE_PARK
0x20    3   MINOR_CONTOUR
0x20    5   STATE_PARK
0x21    3   INT_CONTOUR
0x22    3   MAJOR_CONTOUR
0x23    3   MINOR_BATHY_CONTOUR
0x26    3   INTERMITTENT_STREAM
0x27    3   AIRPORT_RUNWAY
0x28    3   PIPELINE
0x29    5   LAKE
0x32    5   SEA
0x3b    5   SMALL_LAKE
0x3c    5   SMALL_LAKE
0x3d    5   LARGE_LAKE
0x3e    5   LAKE
0x3f    3   CUSTOMIZABLE_LINE_1
0x3f    5   LAKE
0x40    5   SMALL_LAKE
0x41    3   CUSTOMIZABLE_LINE_1
0x41    5   SMALL_LAKE
0x42    3   UNPAVED_ROAD
0x43    3   CUSTOMIZABLE_ROUTE_LINE_1
0x44    3   RIVER
0x44    5   LARGE_LAKE
0x45    3   MNR_PLTCL_BDRY
0x45    5   LAKE
0x46    3   MISC_LINE
0x46    5   LARGE_RIVER
0x47    3   CUSTOMIZABLE_LINE_2
0x47    5   LARGE_RIVER
0x48    3   UNPAVED_ROAD
0x48    5   SMALL_RIVER
0x49    5   SMALL_RIVER
0x4b    5   DATA_BOUNDS
0x4e    5   ORCHARD
0x4f    5   SCRUB
0x50    5   WOODS
0x4c    5   WETLAND
0x53    5   FLAT
0x68    5   GENERIC_MANMADE
0x69    5   GENERIC_MANMADE
0x6a    5   MISC_AREA
0x6b    5   MISC_AREA
0x6c    5   GENERIC_MANMADE
0x6d    5   GENERIC_MANMADE
0x6e    5   GENERIC_MANMADE
0x6f    5   GENERIC_MANMADE
0x78    5   MISC_AREA
0x7a    5   MISC_AREA
0x7b    5   MISC_AREA
0x81    5   WETLAND
0x83    5   WOODS
0x85    5   WOODS
0x88    5   LAND
0x89    5   FLAT
0x8a    5   LAND
0x8c    5   INTERMITTENT_LAKE
0x8d    5   MARINE_MISC_AREA
0x95    5   ORCHARD
0x98    5   LAND

