package Utils;

# ABSTRACT: non-specific service functions

# $Id$


use 5.010;
use strict;
use warnings;
use utf8;

use Carp;


=head2 make_re_from_list

Make regexp (optimized if possible) from plain list.

Options:

    * capture
    * quote
    * get_rule_num

=cut

sub make_re_from_list {
    my ($list, %opt) = @_;

    my @keywords =
        map {
            my $text = $opt{quote} ? quotemeta $list->[$_] : $list->[$_];
            $text .= "(?{$_})"  if $opt{get_rule_num};
            $text
        }
        sort { length $list->[$b] <=> length $list->[$a] }
        ( 0 .. $#$list );

    my $re_text;
    eval {
        require Regexp::Assemble;
        my $flags = $opt{i} ? q{i} : q{};
        $re_text = Regexp::Assemble->new( flags => $flags )->add( @keywords )->re();
    }
    or eval {
        $re_text =
            ( $opt{i} ? '(?i)' : q{} ) .
            join q{|}, @keywords;
        die if $opt{get_rule_num};  # skip optimization attempt
        require Regexp::Optimizer;
        $re_text = Regexp::Optimizer->new()->optimize($re_text);
    };

    use re 'eval';
    my $re = $opt{capture} ? qr/($re_text)/ : qr/$re_text/;

    return $re;
}


=head2 hash_merge
=cut

sub hash_merge {
    my $target = shift;
    for my $hash_to_add ( @_ ) {
        for my $key ( keys %$hash_to_add ) {
            $target->{$key} = $hash_to_add->{$key};
        }
    }

    return $target;
}

1;

