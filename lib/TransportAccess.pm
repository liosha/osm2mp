package TransportAccess;

# ABSTRACT: calculate access flags

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::MoreUtils qw/ none /;

use AreaTree;



=method new

    my $calc_access = TransportAccess->new( %opts );

Options:
    * transport_types
    * access_tags (obsolete: transport)
    * yesno
    * barrier

Constructor

=cut

sub new {
    my ($class, %opt) = @_;
    my $self = {
        areas => AreaTree->new(),
        yesno => $opt{yesno} || { yes => 1, no => 0, 1=>1, 0=>0 },
    };

    # flag positions
    my $types = $opt{transport_types} || [];
    for my $position ( 0 .. $#$types ) {
        next if !$types->[$position];
        for my $transport_id ( @{ $types->[$position] } ) {
            $self->{position}->{$transport_id} = $position;
        }
    }

    # tags
    my $access_tags = $opt{access_tags} || $opt{transport}  or croak 'No access tags info';
    for my $tag_info ( @$access_tags ) {
        my $tag = $tag_info->{key} || $tag_info->{tag}
            or croak "No tag name";
        my $mode = $tag_info->{mode} ~~ -1 ? 0 : 1;
        my $flags = _parse_flag_string( $tag_info->{val} );

        push @{ $self->{tags} }, [ $tag, $mode, $flags ];
        $self->{num_flags} = scalar @$flags  if !$self->{num_flags} || @$flags > $self->{num_flags};
    }

    # barriers
    my $barriers = $opt{barrier_tags} || $opt{barrier} || {};
    $self->{barrier} = {  map {( $_ => _parse_flag_string( $barriers->{$_} ) )}  keys %$barriers  };

    return bless $self, $class;
}



sub add_area {
    my ($self, $tags, $outers, $inners) = @_;
    return if !@$outers;

    my $acc = $self->get_tag_flags( $tags );
    return if none {$_} @$acc;

    $self->{areas}->add_area( $acc, $outers, $inners );
    return;
}



sub get_tag_flags {
    my ($self, $tags, $default_flags) = @_;
    my @flags = $default_flags ? @$default_flags : ( (0) x $self->{num_flags} );

    for my $tag_info ( @{ $self->{tags} } ) {
        my ($tag, $mode, $mask) = @$tag_info;
        my $tag_val = $tags->{$tag};
        next if !defined $tag_val;
    
        my $flag = $self->{yesno}->{$tag_val};
        next if !defined $flag;
        my $flag_val = $mode ? 1-$flag : $flag;

        # ??? boolean?
        for my $position ( 0 .. $self->{num_flags}-1 ) {
            next if !$mask->[$position];
            $flags[$position] = $flag_val;
        }
    }
        
    return \@flags;
}



sub get_road_flags {
    my ($self, $tags, $default_flags, @points) = @_;

    my $acc = $self->get_tag_flags( $tags, $default_flags );
    if ( @points && ( my $area_acc = $self->{areas}->find_area( @points ) ) ) {
        @$acc = map { $acc->[$_] || $area_acc->[$_] } (0 .. $self->{num_flags}-1);
    }

    return $acc;
}



sub get_barrier_flags {
    my ($self, $tags) = @_;

    my $barrier_tag = $tags->{barrier} || q{};
    my $default_acc = $self->{barrier}->{$barrier_tag} || [ (1) x $self->{num_flags} ];

    return $self->get_tag_flags( $tags, $default_acc );
}




sub _parse_flag_string {
    my ($str) = @_;
    
    my @flags = split /[;,\s]*/xms, $str;
    return \@flags;
}


1;

