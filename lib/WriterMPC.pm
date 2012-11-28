package WriterMPC;

# ABSTRACT: garmin shapefile writer

# $Id$


use 5.010;
use strict;
use warnings;
use autodie;

use Carp;

use Encode;

use Geo::Shapefile::Writer;
use TextFilter;

use YAML;



our @POINT_ATTRS = (
    [ NAME => 'C', 250 ],
    [ GRMN_TYPE => 'C', 32 ],
);


our %POI_TYPE = _init_poi_codes();


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

    ##  Initialize filters
    my $filter_chain = $self->{filter_chain} = TextFilter->new();

    return $self;
}




=method output( $template, $data )

=cut

sub output {
    my ( $self, $template, $data ) = @_;

    my %writer = (
        section => undef,
        info => undef,

        point => sub { $self->_write_point(@_) },
        # !!!
        polygon => undef,
        polyline => undef,
        turn_restriction => undef,
        destination_sign => undef,
    );

    if ( !exists $writer{$template} ) {
        say Dump $template, $data;
        exit;
    }

    my $writer = $writer{$template};
    return if !$writer;
    
    $writer->($data);
    return;
}


sub _write_point {
    my ($self, $data) = @_;

#    say Dump $data; exit;

    my $shp = $self->{shp}->{points};
    if ( !$shp ) {
        my $prefix = $self->{output_base} ? "$self->{output_base}." : q{};
        $shp = $self->{shp}->{points} = Geo::Shapefile::Writer->new("${prefix}points", 'POINT', @POINT_ATTRS );
    }

    my $type = $POI_TYPE{ $data->{opts}->{Type} };
    carp "Unknown type $data->{opts}->{Type}"  if !$type;
    $type ||= $data->{opts}->{Type};

    $shp->add_shape(
        [ reverse @{ $data->{opts}->{coords} } ],
        encode( $self->{codepage}, $data->{opts}->{Label} ),
        encode( $self->{codepage}, $type ),
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
#        'mapid=s'               => sub { $self->{header_opts}->{ID} = $_[1] },
#        'mapname=s'             => sub { $self->{header_opts}->{Name} = $_[1] },
        'codepage=s'            => sub { $self->_register_codepage( $_[1] ) },
        'filter=s'              => sub { $self->{filter_chain}->add_filter( $_[1] ) },
        'upcase!'               => sub { $self->{filter_chain}->add_filter( 'upcase' ) },
        'translit!'             => sub { $self->{filter_chain}->add_filter( 'translit' ) },
        'perlio-filter|textfilter=s' => sub { $self->{filter_chain}->add_perlio_filter( $_[1] ) },
        'ttable=s'              => sub { $self->{filter_chain}->add_table_filter( $_[1] ) },
    );
}


=method get_usage()

=cut

sub get_usage {
    my ($self) = @_;
    return (
        [ 'o|output' => 'output file', 'stdout' ],
        [ 'codepage <num>' => 'output character encoding', $self->{header_opts}->{CodePage} // 'utf8' ],
        [ 'filter <name>' => 'add predefined filter' ],
        [ 'upcase' => 'same as --filter upcase' ],
        [ 'translit' => 'same as --filter translit' ],
        [ 'perlio-filter' => 'use perlio via-layer as filter' ],
        [ 'ttable' => 'character conversion table' ],
    );
}



sub _init_poi_codes {
    my %code = (
        CITY_10M        =>  '0x0100',
        CITY_5M         =>  '0x0200',
        LARGE_CITY      =>  '0x0200',
        CITY_2M         =>  '0x0300',
        CITY_1M         =>  '0x0400',
        CITY_500K       =>  '0x0500',
        CITY_200K       =>  '0x0600',
        CITY_100K       =>  '0x0700',
        CITY_50K        =>  '0x0800',
        MEDIUM_CITY     =>  '0x0800',
        CITY_20K        =>  '0x0900',
        CITY_10K        =>  '0x0A00',
        CITY_5K         =>  '0x0B00',
        CITY_LT5K       =>  '0x0C00',
        SMALL_CITY      =>  '0x0C00',
        CITY_UNKNOWN    =>  '0x0D00',

        RESTAURANT           =>  '0x2A00',
        RESTAURANT_AMERICAN  =>  '0x2A01',
        RESTAURANT_ASIAN     =>  '0x2A02',
        RESTAURANT_BARBECUE  =>  '0x2A03',
        RESTAURANT_CHINESE   =>  '0x2A04',
        RESTAURANT_DELI      =>  '0x2A05',
        RESTAURANT_INTRNTNL  =>  '0x2A06',
        RESTAURANT_FAST_FOOD =>  '0x2A07',
        RESTAURANT_ITALIAN   =>  '0x2A08',
        RESTAURANT_MEXICAN   =>  '0x2A09',
        RESTAURANT_PIZZA     =>  '0x2A0A',
        RESTAURANT_SEAFOOD   =>  '0x2A0B',
        RESTAURANT_STEAK     =>  '0x2A0C',
        RESTAURANT_DONUTS    =>  '0x2A0D',
        RESTAURANT_CAFES     =>  '0x2A0E',
        RESTAURANT_FRENCH    =>  '0x2A0F',
        RESTAURANT_GERMAN    =>  '0x2A10',
        RESTAURANT_BRITISH   =>  '0x2A11',

        HOTEL               =>  '0x2B01',
        BED_AND_BREAKFAST   =>  '0x2B02',
        CAMPGROUND          =>  '0x2B03',
        RESORT              =>  '0x2B04',

        THEME_PARK      =>  '0x2C01',
        MUSEUM          =>  '0x2C02',
        LANDMARK        =>  '0x2C04',
        PARK            =>  '0x2C06',
        GARDEN          =>  '0x2C06',
        ZOO             =>  '0x2C07',
        ARENA           =>  '0x2C08',
        HALL            =>  '0x2C09',
        WINERY          =>  '0x2C0A',

        GOLF_COURSE     =>  '0x2D05',
        SKI_CENTER      =>  '0x2D06',
        BOWLING         =>  '0x2D07',
        SKATING         =>  '0x2D08',
        SWIMMING_POOL   =>  '0x2D09',
        FITNESS_CENTER  =>  '0x2D0A',
        SPORT_AIRPORT   =>  '0x2D0B',

        LIVE_THEATER    =>  '0x2D01',
        BAR             =>  '0x2D02',
        MOVIE_THEATER   =>  '0x2D03',
        CASINO          =>  '0x2D04',

        DEPARTMENT_STORE        =>  '0x2E01',
        GROCERY_STORE           =>  '0x2E02',
        GENERAL_STORE           =>  '0x2E03',
        SHOPPING_CENTER         =>  '0x2E04',
        PHARMACY                =>  '0x2E05',
        CONVENIENCE_STORE       =>  '0x2E06',
        CLOTHING_RETAIL         =>  '0x2E07',
        HOME_AND_GARDEN_STORE   =>  '0x2E08',
        HOME_FURNISHINGS_STORE  =>  '0x2E09',
        SPECIALTY_RETAIL        =>  '0x2E0A',
        SOFTWARE_RETAIL         =>  '0x2E0B',

        GAS_STATION             =>  '0x2F01',
        AUTO_RENTAL             =>  '0x2F02',
        AUTO_REPAIR             =>  '0x2F03',
        DEALER                  =>  '0x2F07',
        WRECKER_SERVICE         =>  '0x2F0A',
        PARKING                 =>  '0x2F0B',
        REST_AREA_TOURIST_INFO  =>  '0x2F0C',
        AUTO_CLUB               =>  '0x2F0D',
        CAR_WASH                =>  '0x2F0E',
        TRUCK_STOP              =>  '0x2F16',

        AIRPORT                 =>  '0x2F04',
        GND_TRANSPORT           =>  '0x2F08',
        TRANSIT_SERVICES        =>  '0x2F17',

        LIBRARY             =>  '0x2C03',
        SCHOOL              =>  '0x2C05',
        PLACE_OF_WORSHIP    =>  '0x2C0B',
        POW_CHURCH          =>  '0x2C0E',
        POW_MOSQUE          =>  '0x2C0D',
        POW_TEMPLE          =>  '0x2C0F',
        POW_SYNAGOGUE       =>  '0x2C10',
        POLICE_STATION      =>  '0x3001',
        CITY_HALL           =>  '0x3003',
        COURTHOUSE          =>  '0x3004',
        COMMUNITY_CENTER    =>  '0x3005',
        BORDER_CROSSING     =>  '0x3006',
        GOV_OFFICE          =>  '0x3007',
        FIRE_DEPT           =>  '0x3008',
        POST_OFFICE         =>  '0x2F05',
        BANK                =>  '0x2F06',
        UTILITY             =>  '0x2F15',

        BANK    =>  '0x2F06',

        MARINA                  =>  '0x2F09',
        GARMIN_DEALERS          =>  '0x2F0F',
        SERVICES_PERSONAL       =>  '0x2F10',
        SERVICES_BUSINESS       =>  '0x2F11',
        COMMUNICATION_SERVICES  =>  '0x2F12',
        REPAIR_SERVICE          =>  '0x2F13',
        SOCIAL_SERVICES         =>  '0x2F14',
        
        HOSPITAL    =>  '0x3002',

        BRIDGE          =>  '0x6401',
        BUILDING        =>  '0x6402',
        CEMETERY        =>  '0x6403',
        CHURCH          =>  '0x6404',
        CIVIL_BUILDING  =>  '0x6405',
        CROSSING        =>  '0x6406',
        DAM             =>  '0x6407',
        LEVEE           =>  '0x6409',
        LOCALE          =>  '0x640A',
        MILITARY        =>  '0x640B',
        MINE            =>  '0x640C',
        OILFIELD        =>  '0x640D',
        TOWER           =>  '0x6411',
        TRAIL           =>  '0x6412',
        TUNNEL          =>  '0x6413',
        WELL            =>  '0x6414',
        HISTORICAL_TOWN =>  '0x6415',
        SUBDIVISION     =>  '0x6416',

        ARROYO      =>  '0x6501',
        SAND_BAR    =>  '0x6502',
        BAY         =>  '0x6503',
        CANAL       =>  '0x6505',
        CHANNEL     =>  '0x6506',
        COVE        =>  '0x6507',
        WATERFALL   =>  '0x6508',
        GEYSER      =>  '0x6509',
        GLACIER     =>  '0x650A',
        HARBOR      =>  '0x650B',
        LAKE        =>  '0x650D',
        RAPIDS      =>  '0x650E',
        RESERVOIR   =>  '0x650F',
        SEA         =>  '0x6510',
        SPRING      =>  '0x6511',
        STREAM      =>  '0x6512',
        SWAMP       =>  '0x6513',

        ARCH        =>  '0x6601',
        BASIN       =>  '0x6603',
        BEACH       =>  '0x6604',
        BENCH       =>  '0x6605',
        CAPE        =>  '0x6606',
        CLIFF       =>  '0x6607',
        CRATER      =>  '0x6608',
        FLAT        =>  '0x6609',
        FOREST      =>  '0x660A',
        GAP         =>  '0x660B',
        GUT         =>  '0x660C',
        ISTHMUS     =>  '0x660D',
        LAVA_FLOW   =>  '0x660E',
        PILLAR      =>  '0x660F',
        PLAIN       =>  '0x6610',
        RANGE       =>  '0x6611',
        RESERVE     =>  '0x6612',
        RIDGE       =>  '0x6613',
        ROCK        =>  '0x6614',
        SLOPE       =>  '0x6615',
        SUMMIT      =>  '0x6616',
        VALLEY      =>  '0x6617',
        WOODS       =>  '0x6618',

        MAJOR_COUNTRY   =>  '0x1400',
        MINOR_COUNTRY   =>  '0x1500',
        STATE           =>  '0x1E00',
        PROVINCE        =>  '0x1E00',
        COUNTY          =>  '0x1F00',
    );

    return map {( $_ => $_, $code{$_} => $_ )} keys %code;
}



1;

