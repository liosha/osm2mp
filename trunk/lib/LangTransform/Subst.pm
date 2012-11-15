package LangTransform::Subst;

# $Id$

# ABSTRACT: simple substitute transliterator, just proof-of-concept

use 5.010;
use strict;
use warnings;
use utf8;

our $PRIORITY = 1;

our %DATA = (
    uk_ru => {
        from  => 'uk',
        to    => 'ru',
        table => {
            'ґ' => 'г',
            'е' => 'э',
            'є' => 'е',
            'и' => 'ы',
            'і' => 'и',
            'i' => 'и',
            'ї' => 'йи',
            'щ' => 'шч',
        },
        same_upcase => 1,
    },
);


sub init {
    my (undef, %callback) = @_;

    for my $tr ( get_transformers() ) {
        $callback{register_transformer}->($tr);
    }

    return;
}



sub get_transformers {

    my @result;
    while ( my ($id, $data) = each %DATA ) {
        my %table = (
            %{ $data->{table} },
            ( $data->{same_upcase}
                ? ( map {( uc($_) => ucfirst($data->{table}->{$_}) )} keys %{ $data->{table} } )
                : ()
            )
        );
        my $re_text = join q{|}, map {quotemeta $_} sort { length $b <=> length $a } keys %table;
        my $re = qr/($re_text)/xms; # TODO: optimize

        push @result, {
            id => "subst_$id",
            from => $data->{from},
            to => $data->{to},
            priority => $PRIORITY,
            transformer => sub { my ($text) = @_; $text =~ s/$re/$table{$1}/gexms; return $text },
        };
    }

    return @result;
}


1;

