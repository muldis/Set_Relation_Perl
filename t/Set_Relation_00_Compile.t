use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use version 0.74;

use Test::More;

plan( 'tests' => 6 );

use_ok( 'Set::Relation' );
is( $Set::Relation::VERSION, qv('0.11.1'),
    'Set::Relation is the correct version' );

use_ok( 'Set::Relation::V1' );
is( $Set::Relation::V1::VERSION, qv('0.11.1'),
    'Set::Relation::V1 is the correct version' );

use_ok( 'Set::Relation::V2' );
is( $Set::Relation::V2::VERSION, qv('0.11.1'),
    'Set::Relation::V2 is the correct version' );

1; # Magic true value required at end of a reusable file's code.
