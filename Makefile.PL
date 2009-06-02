use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME' => 'Set::Relation',
    'VERSION' => '0.11.0',
    'ABSTRACT' => 'Relation data type for Perl',
    'AUTHOR' => 'Darren Duncan <perl@DarrenDuncan.net>',
    'LICENSE' => 'lgpl',
    'PREREQ_PM' => {
        # These are bundled with Perls 5.10.x but not Perls 5.8.x.
        'version' => 0.74,
        # These are not bundled with any version of Perl.
        'namespace::clean' => 0.11,
        'List::MoreUtils' => 0.22,
        'Moose' => 0.79,
        'Moose::Role' => 0.79,
        'Class::MOP' => 0.84, # req latest indep of Moose
        # These are only used by the tests, are not bundled with any Perl.
        'Test::Deep' => 0,
    },
);

1; # Magic true value required at end of a reusable file's code.
