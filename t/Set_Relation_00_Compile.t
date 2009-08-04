use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';
use Carp::Always 0.09;

use version 0.74;

use Test::More 0.88;

use_ok( 'Set::Relation' );
is( $Set::Relation::VERSION, qv('0.12.0'),
    'Set::Relation is the correct version' );

use_ok( 'Set::Relation::V1' );
is( $Set::Relation::V1::VERSION, qv('0.12.0'),
    'Set::Relation::V1 is the correct version' );

use_ok( 'Set::Relation::V2' );
is( $Set::Relation::V2::VERSION, qv('0.12.0'),
    'Set::Relation::V2 is the correct version' );

done_testing();

1; # Magic true value required at end of a reusable file's code.
