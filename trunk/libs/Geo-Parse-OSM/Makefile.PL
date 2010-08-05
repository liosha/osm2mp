use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Geo::Parse::OSM',
    AUTHOR              => q{liosha <liosha@cpan.org>},
    VERSION_FROM        => 'lib/Geo/Parse/OSM.pm',
    ABSTRACT_FROM       => 'lib/Geo/Parse/OSM.pm',
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    PREREQ_PM => {
        'Encode' => 0,
        'Test::More' => 0,
        'HTML::Parser' => 0,
        'IO::Uncompress::AnyUncompress' => 0,
        'List::Util' => 0,
        'List::MoreUtils' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Geo-Parse-OSM-*' },
);
