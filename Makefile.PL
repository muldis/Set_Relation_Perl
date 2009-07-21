use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME' => 'Set::Relation',
    'VERSION' => '0.11.3',
    'ABSTRACT' => 'Relation data type for Perl',
    'AUTHOR' => 'Darren Duncan <perl@DarrenDuncan.net>',
    'LICENSE' => 'lgpl',
    'PREREQ_PM' => {
        # These are bundled with Perls 5.10.x but not Perls 5.8.x.
        'version' => 0.74,
        # These are not bundled with any version of Perl.
        'namespace::autoclean' => 0.08,
        'List::MoreUtils' => 0.22,
        'Moose' => 0.87,
        'Moose::Role' => 0.87,
        'Class::MOP' => 0.89, # req latest indep of Moose
        # These are only used by the tests, are not bundled with any Perl.
        'Test::More' => 0.88, # but upd later to v that perl 5.10.1 bundles
        'Carp::Always' => 0.09,
        'Test::Deep' => 0,
    },
);

1; # Magic true value required at end of a reusable file's code.
