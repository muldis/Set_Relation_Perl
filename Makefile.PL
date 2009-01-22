use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME' => 'Set::Relation',
    'VERSION' => '0.0.1',
    'ABSTRACT' => 'Relation data type for Perl',
    'AUTHOR' => 'Darren Duncan <perl@DarrenDuncan.net>',
    'LICENSE' => 'lgpl',
    'PREREQ_PM' => {
        # These are bundled with Perls 5.10.x but not Perls 5.8.x.
        'version' => 0.74,
        # These are not bundled with any version of Perl.
        'Moose' => 0.64,
        'Class::MOP' => 0.75, # req latest indep of Moose
    },
);

1; # Magic true value required at end of a reusable file's code.
