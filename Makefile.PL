use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME' => 'Set::Relation',
    'VERSION' => '0.12.3',
    'ABSTRACT' => 'Relation data type for Perl',
    'AUTHOR' => 'Darren Duncan <perl@DarrenDuncan.net>',
    'LICENSE' => 'lgpl',
    'PREREQ_PM' => {
        # ALWAYS REQUIRES:

        # These are not bundled with any version of Perl.
        'namespace::autoclean' => 0.09,
        'List::MoreUtils' => 0.22, # but req 0.25_02+ under Perl 5.10.1+
        'Moose' => 0.90,
        'Moose::Role' => 0.90,
        'Class::MOP' => 0.93, # req latest indep of Moose

        # BUILD REQUIRES:

        # These are bundled with Perls 5.10.1+ but not earlier Perls.
        'Test::More' => 0.92,
        # These are not bundled with any version of Perl.
        'Carp::Always' => 0.09,
        'Test::Deep' => 0.106,
    },
);

1; # Magic true value required at end of a reusable file's code.
