package GarminTools;

# ABSTRACT: garmin-specific routines

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;


=function get_garmin_address

    Convert extended address to garmin- and mp-compatible form:
    country - region - city - street - housenumber

=cut

sub get_garmin_address {
    my ($address, %opt) = @_;

    my %mp_address;

    if ( $address->{house} ) {
        $mp_address{house} = $address->{house};
    }

    if ( $address->{house} || $address->{street} ) {
        my @fields = grep {$_} map { $address->{$_} } qw/ street quarter suburb /;
        push @fields, $address->{city}  if !@fields && $address->{city} && $address->{city};

        if ( @fields && ( my $street = join q{ }, shift(@fields), map {"($_)"} @fields ) ) {
            $mp_address{street} = $street;
        }
    }

    if ( $address->{city} ) {
        $mp_address{city} = $address->{city};
    }

    if ( $address->{region} ) {
        my $region = join q{ }, grep {$_}
            map { $address->{$_} && $address->{$_} } qw/ region district subdistrict /;
        $mp_address{region} = $region;
    }

    if ( $address->{country} ) {
#        $mp_address{country} = rename_country( $address->{country} );
        $mp_address{country} = $address->{country};
    }

    if ( $address->{postcode} ) {
        $mp_address{postcode} = $address->{postcode};
    }

    return \%mp_address;
}




1;

