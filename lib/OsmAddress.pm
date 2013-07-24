package OsmAddress;

# ABSTRACT: OSM addressing

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::Util qw/ first /;
use List::MoreUtils qw/ none first_index last_index /;

use AreaTree;
use Utils;


our @NAME_TAGS = qw/ name place_name /;
our $MIN_DEFAULT_LEVEL = 'city';


=head2 new

Constructor

    my $addresser = OsmAddress->new( %opts );

Options:
  * addr_levels
  * addr_prefixes
  * name_tags
  * rename_*

=cut

sub new {
    my ($class, %opt) = @_;

    my $self = {
        addr_prefixes    => $opt{addr_prefixes} || [ 'addr' ],
        addr_levels      => $opt{addr_levels} || [],
        default_address  => {},
    };
    bless $self, $class;

    # regexp for all addr tag names
    my $name_tag_str = Utils::make_re_from_list( $opt{name_tags} || \@NAME_TAGS );
    $self->{name_tag_re} = qr/ ^ (?: $name_tag_str ) \b /xms;

    for my $level_info ( @{ $self->{addr_levels} } ) {
        my $level = $level_info->{level};
        $self->{level_info}->{$level} = $level_info;

        my @level_tags =
            map { my $tag = $_;  map {"$_:$tag"} @{ $self->{addr_prefixes} } }
            @{ $level_info->{tags} };
        $self->{addr_tags}->{$level} = \@level_tags;

        my $level_tag_str = Utils::make_re_from_list(\@level_tags);
        $self->{addr_tag_re}->{$level} = qr/ ^ (?: $level_tag_str ) \b /xms;

        for my $tag ( @level_tags ) {
            $self->{tag_level}->{$tag} = $level;
        }
    }

    my $addr_tags = $self->{addr_tags};
    my @addr_levels = ( q{}, map { $_->{level} } reverse @{ $self->{addr_levels} } );
    while ( @addr_levels ) {
        my $level = shift @addr_levels;
        $self->{addr_parents}->{$level} = [ @addr_levels ];

        my @parent_areas = grep { $self->{level_info}->{$_}->{area_condition} } @addr_levels;
        $self->{parent_areas}->{$level} = \@parent_areas  if @parent_areas;

        if ( $level ) {
            my @level_tags = @{ $addr_tags->{$level} };
            my $level_tag_str = Utils::make_re_from_list(\@level_tags);
            $self->{addr_tag_re}->{$level} = qr/ ^ (?: $level_tag_str ) \b /xms;
        }
        
        my @tags = map { @{ $addr_tags->{$_} } } (($level ? $level : ()), @addr_levels);
        my $tag_str = Utils::make_re_from_list(\@tags);
        $self->{full_addr_tag_re}->{$level} = qr/ ^ (?: $tag_str ) \b /xms;

        next if !$level;
        my $table = $opt{"rename_$level"};
        next if !$table;
        $self->add_rename_table( $level => $table );
    }

    return $self;
}


sub add_rename_table {
    my ($self, $level, $table) = @_;

    $self->{rename}->{$level} = $table;
    return;
}


sub add_rename_table_yml {
    my ($self, $file) = @_;

    require YAML;
    my ($key, $table) = YAML::LoadFile( $file );
    my ($level) = $key =~ /^ rename_(\w+) $ /xms;
    croak "Invalid rename table key: $key"  if !$level || !$self->{addr_tags}->{$level};

    $self->add_rename_table( $level => $table );
    return;
}


sub get_area_ftconfig {
    my ($self, $osm) = @_;

    my @rules;
    for my $level_info ( @{ $self->{addr_levels} } ) {
        my ($level, $condition) = @$level_info{ qw/ level area_condition / };
        next if !$condition;
        push @rules, {
            condition => [ $condition ],
            action => [ sub {
                    my ($obj, $action) = @_;
                    return if !$obj->{outer};

                    my $address_tags = $self->get_address_tags($obj->{tag}, level => $level);
                    return if none { $_ =~ $self->{addr_tag_re}->{$level} } keys %$address_tags;

                    my @outers = map { $osm->get_lonlat($_) } @{$obj->{outer}};
                    my @inners = map { $osm->get_lonlat($_) } @{$obj->{inner}};
                    $self->load_area( $level, $address_tags, \@outers, \@inners );
                } ],
        };
    }

    return \@rules;
}


sub load_area {
    my ($self, $level, $info, $outers, $inners) = @_;

    my $tree = $self->{areas}->{$level} ||= AreaTree->new();
    $tree->add_area( $info, $outers, $inners );

    return;
}


sub find_area {
    my ($self, $level, @points) = @_;

    my $tree = $self->{areas}->{$level}
        or return;

    return $tree->find_area(@points);
}



=head2 $addresser->get_address_tags( \%tags, %opt )

    Filters out all non-address tags

    Options:
        level - assume object has this level:
            * use it's name as addr:<level>
            * filter out all lower address levels
        points

=cut


sub get_address_tags {
    my ($self, $tags, %opt) = @_;

    my $level = $opt{level} // q{};
    my $tag_re = $self->{full_addr_tag_re}->{$level};
    croak "Unknown level: $level"  if !$tag_re;

    my %result;

    # get 'name' tags if object itself
    if ( $level ) {
        my $level_tag = $self->{addr_tags}->{$level}->[0];
        while ( my ($k, $v) = each %$tags ) {
            next if $k !~ $self->{name_tag_re};

            $k =~ s/$self->{name_tag_re}/$level_tag/xms;
            $result{$k} = $v;
        }
    }

    # get addr:* tags
    while ( my ($k, $v) = each %$tags ) {
        next if $k !~ $tag_re;
        $result{$k} = $v;
    }

    # get parent areas if points defined
    if ( $opt{points} && ( my $area_levels = $self->{parent_areas}->{$level} ) ) {
        for my $area_level ( @$area_levels ) {
            my $area_tags = $self->find_area( $area_level => @{$opt{points}} );
            next if !$area_tags;
            Utils::hash_merge \%result, $area_tags;
        }
    }

    return \%result;
}



sub get_lang_address {
    my ($self, $tags, $lang_select, %opt) = @_;

    my %address;
    while ( my ($level, $keys) = each %{ $self->{addr_tags} } ) {

        my $value;
        if ( my $table = $self->{rename}->{$level} ) {
            my $def_value = first {defined} map {$tags->{$_}} @$keys;
            $def_value //= first {defined} map {$self->{default_address}->{$_}} @$keys;

            if ( $def_value ) {
                my $vtags = $table->{uc $def_value};
                $value = $lang_select->get_value(q{}, $vtags)  if $vtags;
            }
        }

        $value //= first {defined} map { $lang_select->get_value($_, $tags) } @$keys;
        if ( !$value && %{ $self->{default_address} } ) {
            $value = first {defined} map { $lang_select->get_value($_, $self->{default_address}) } @$keys;
        }
        next if !$value;

        $address{$level} = $value;
    }

    return \%address;
}



sub get_getopt {
    my ($self) = @_;

    my @opt_list = (
        'rename-table=s' => sub { $self->add_rename_table_yml($_[1]) },
    );

    my $last_idx = last_index { $_->{level} eq $MIN_DEFAULT_LEVEL } @{ $self->{addr_levels} };
    for my $i ( 0 .. $last_idx ) {
        my $level = $self->{addr_levels}->[$i]->{level};
        my $level_tag = $self->{addr_tags}->{$level}->[0];
        push @opt_list, ( "default-$level|default$level=s" => sub {
                my ($lang, $name) = $_[1] =~ / (?: (\w{1,3}) : )? (.*) /xms;
                my $vtag = $lang ? "$level_tag:$lang" : $level_tag;
                $self->{default_address}->{$vtag} = $_[1];
            } );
    }

    return @opt_list;
}
   

=method get_usage()

=cut

sub get_usage {
    my ($self) = @_;
    
    my @opt_list = (
        [ 'rename-table' => 'table for renaming, yaml-file' ],
    );

    my $last_idx = last_index { $_->{level} eq $MIN_DEFAULT_LEVEL } @{ $self->{addr_levels} };
    for my $i ( 0 .. $last_idx ) {
        my $level = $self->{addr_levels}->[$i]->{level};
        push @opt_list, [ "default-$level" => "default $level" ];
    }
    
    return @opt_list;
}


1;

