use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Math::Polygon::Tree',
    AUTHOR              => q{liosha <liosha@cpan.org>},
    VERSION_FROM        => 'lib/Math/Polygon/Tree.pm',
    ABSTRACT_FROM       => 'lib/Math/Polygon/Tree.pm',
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
        'List::Util' => 0,
        'List::MoreUtils' => 0,
        'Math::Geometry::Planar::GPC::Polygon' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Math-Polygon-Tree-*' },
);
