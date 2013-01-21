package FeatureConfig;

# ABSTRACT: feature type selector

# $Id$


use 5.010;
use strict;
use warnings;

use Carp;
use List::MoreUtils qw/ any all notall first_index /;


our $RULE_CONDITIONS_KEY = 'condition';
our $RULE_ACTIONS_KEY    = 'action';



=method new

    my $ft_selector = FeatureConfig->new( %options );

Create instance.
Options:
    actions => { $action_id => \&execute_action_id, ... }
    conditions => { $condition_id => \&is_condition_id, ... }
    rules => { $section => \@rules, ... }

=cut

sub new {
    my ($class, %opt) = @_;

    my $self = bless { actions => $opt{actions}, conditions => $opt{conditions} || {} }, $class;

    while ( my ($section, $rules) = each %{ $opt{rules} || {} } ) {
        $self->add_rules( $section => $rules );
    }

    return $self;
}


=method add_rules

    $ft_selector->add_rules( $section => \@rules );

Add rules to config section. Replace rules with same id

=cut

sub add_rules {
    my ($self, $section, $new_rules) = @_;
    
    my $rules = $self->{rules}->{$section} //= [];
    for my $rule ( @$new_rules ) {

        for my $condition ( @{ $rule->{$RULE_CONDITIONS_KEY} } ) {
            $condition = $self->_precompile_condition($condition);
        }

        if ( $rule->{id} && (my $index = first_index { $_->{id} ~~ $rule->{id} } @$rules) >= 0 ) {
            $rules->[$index] = $rule;
        }
        else {
            push @$rules, $rule;
        }
    }
    return;
}



sub _precompile_condition {
    my ($self, $condition) = @_;

    # direct code
    return $condition if ref $condition eq 'CODE';

    # tag matching
    if ( my ($key, $neg, $val) = $condition =~ m/ (\S+) \s* (!?) = \s* (.+) /xms ) {
        return sub { !!$neg xor exists shift()->{tag}->{$key} }  if $val eq q{*};
        my $re = qr/^(?:$val)$/xms;
        return sub { !!$neg xor any { $_ =~ $re } split(/;/x, shift()->{tag}->{$key} // q{}) };
    }

    # recursive
    if ( ref $condition eq 'HASH' ) {
        if ( exists $condition->{or} ) {
            my @list = map { $self->_precompile_condition($_) } @{ $condition->{or} };
            return sub { my $object = shift; any { $_->($object) } @list };
        }
        if ( exists $condition->{and} ) {
            my @list = map { $self->_precompile_condition($_) } @{ $condition->{and} };
            return sub { my $object = shift; all { $_->($object) } @list };
        }
        # complex
        if ( my $key = $condition->{key} ) {
            my $sub = $self->{conditions}->{$key};
            croak "Unknown condition: $key"  if !$sub;
            return sub { $sub->($condition, @_) };
        }
    }

    # id codes
    if ( my ($neg, $cond_id) = $condition =~ / (\~?) \s* (\w+) /xms ) {
        return sub { !!$neg xor &{$self->{conditions}->{$cond_id}} }  if exists $self->{conditions}->{$cond_id};
        return sub { !!$neg xor shift()->{type} ~~ 'Node' }     if $cond_id ~~ 'only_node';
        return sub { !!$neg xor !(shift()->{type} ~~ 'Node') }  if $cond_id ~~ [ 'only_way', 'no_node' ];    
    }

    croak "Unknown condition: $condition";
}


=method process

    $ft_config->process( $section => $object );

Check rules for object and execute actions if matches

=cut

sub process {
    my ($self, $section, $object) = @_;

    my $rules = $self->{rules}->{$section};
    for my $rule ( @$rules ) {
        # !!! use check_condition
        next if notall { $_->($object) } @{ $rule->{$RULE_CONDITIONS_KEY} };
        for my $action ( @{ $rule->{$RULE_ACTIONS_KEY} } ) {
            given ( ref $action ) {
                when ('CODE') {
                    $action->($object);
                }
                when ('HASH') {
                    my $action_code = $action->{action};
                    croak "Unknown action: $action_code" if !exists $self->{actions}->{$action_code};
                    $self->{actions}->{$action_code}->($object, $action);
                }
                croak "Invalid action type: $_";
            }
        }
    }

    return;
}



=method check_condition

    my $is_succeed = $ft_config->check_condition( $condition, $object );

Checks condition.
Precompiles and replaces if not CODE.

=cut

sub check_condition {
    my ( $self, $condition, $object ) = @_;

    if ( ref $condition ne 'CODE' ) {
        $condition = $_[1] = $self->_precompile_condition($condition);
    }

    return $condition->($object);
}

1;

