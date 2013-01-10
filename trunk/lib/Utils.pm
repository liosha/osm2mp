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

=cut

sub make_re_from_list {
    my ($list, %opt) = @_;

    my @keywords = map { quotemeta $_ } sort { length $b <=> length $a } @$list;

    my $re_text;
    eval {
        require Regexp::Assemble;
        $re_text = Regexp::Assemble->new()->add( @keywords )->re();
    }
    or eval {
        $re_text = join q{|}, @keywords;
        require Regexp::Optimizer;
        $re_text = Regexp::Optimizer->new()->optimize($re_text);
    };
 
    my $re = $opt{capture} ? qr/($re_text)/ : qr/$re_text/;

    return $re;
}


1;

