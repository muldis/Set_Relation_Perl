use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

###########################################################################
###########################################################################

{ package Set::Relation; # role
    our $VERSION = '0.012007';
    $VERSION = eval $VERSION;
    # Note: This given version applies to all of this file's packages.

    use namespace::autoclean 0.09;

    use Moose::Role 0.98;

    requires 'export_for_new';
    requires 'which';
    requires 'members';
    requires 'heading';
    requires 'body';
    requires 'slice';
    requires 'attr';
    requires 'keys';

    requires 'degree';
    requires 'is_nullary';
    requires 'has_attrs';
    requires 'attr_names';
    requires 'cardinality';
    requires 'count';
    requires 'is_empty';
    requires 'has_member';
    requires 'has_key';
    requires 'empty';
    requires 'insertion';
    requires 'deletion';
    requires 'rename';
    requires 'projection';
    requires 'cmpl_proj';
    requires 'wrap';
    requires 'cmpl_wrap';
    requires 'unwrap';
    requires 'group';
    requires 'cmpl_group';
    requires 'ungroup';
    requires 'tclose';
    requires 'restriction';
    requires 'restr_and_cmpl';
    requires 'cmpl_restr';
    requires 'classification';
    requires 'extension';
    requires 'static_exten';
    requires 'map';
    requires 'summary';
    requires 'cardinality_per_group';
    requires 'count_per_group';

    requires 'is_identical';
    requires 'is_subset';
    requires 'is_superset';
    requires 'is_proper_subset';
    requires 'is_proper_superset';
    requires 'is_disjoint';
    requires 'union';
    requires 'exclusion';
    requires 'symmetric_diff';
    requires 'intersection';
    requires 'diff';
    requires 'semidiff';
    requires 'antijoin';
    requires 'semijoin_and_diff';
    requires 'semijoin';
    requires 'join';
    requires 'product';
    requires 'quotient';
    requires 'composition';
    requires 'join_with_group';

    requires 'rank';
    requires 'rank_by_attr_names';
    requires 'limit';
    requires 'limit_by_attr_names';

    requires 'substitution';
    requires 'static_subst';
    requires 'subst_in_restr';
    requires 'static_subst_in_restr';
    requires 'subst_in_semijoin';
    requires 'static_subst_in_semijoin';

    requires 'outer_join_with_group';
    requires 'outer_join_with_undefs';
    requires 'outer_join_with_static_exten';
    requires 'outer_join_with_exten';

} # role Set::Relation

###########################################################################
###########################################################################

{ package Set::Relation::Mutable; # role

    use namespace::autoclean 0.09;

    use Moose::Role 0.98;

    with 'Set::Relation';

    requires 'clone';
    requires 'has_frozen_identity';
    requires 'freeze_identity';

    requires 'evacuate';
    requires 'insert';
    requires 'delete';

} # role Set::Relation::Mutable

###########################################################################
###########################################################################

1; # Magic true value required at end of a reusable file's code.
__END__

=pod

=encoding utf8

=head1 NAME

Set::Relation -
Relation data type for Perl

=head1 VERSION

This document describes Set::Relation version 0.12.7 for Perl 5.

=head1 SYNOPSIS

    use Set::Relation::V2;

    sub relation { return Set::Relation::V2->new( @_ ); }

    my $r1 = relation( [ [ 'x', 'y' ], [
        [ 4, 7 ],
        [ 3, 2 ],
    ] ] );

    my $r2 = relation( [
        { 'y' => 5, 'z' => 6 },
        { 'y' => 2, 'z' => 1 },
        { 'y' => 2, 'z' => 4 },
    ] );

    my $r3 = $r1->join( $r2 );

    my $r3_as_nfmt_perl = $r3->members();
    my $r3_as_ofmt_perl = $r3->members( 1 );

    # Then $r3_as_nfmt_perl contains:
    # [
    #     { 'x' => 3, 'y' => 2, 'z' => 1 },
    #     { 'x' => 3, 'y' => 2, 'z' => 4 },
    # ]
    # And $r3_as_ofmt_perl contains:
    # [ [ 'x', 'y', 'z' ], [
    #     [ 3, 2, 1 ],
    #     [ 3, 2, 4 ],
    # ] ]

I<This documentation is pending.>

=head1 DESCRIPTION

Set::Relation provides a simple Perl-native facility for an application to
organize and process information using the relational model of data,
without having to employ a separate DBMS, and without having to employ a
whole separate sub-language (such as L<Muldis Rosetta|Muldis::Rosetta>
does).  Rather, it is integrated a lot more into the Perl way of doing
things, and you use it much like a Perl array or hash, or like some other
third-party Set:: modules available for Perl.  This module defines a Perl 5
object role that represents a L<Muldis D|Muldis::D> relation value,
whose methods implement all the Muldis D relational operators.  Or more
specifically, it defines several such roles (immutable and mutable
variants) and there are several bundled implementing Perl 5 classes.

B<WARNING:  This module is still experimental and may change in
incompatible ways between releases.  While the module is considered feature
complete, and is fully documented, and a review of its code makes it look
correct, most features have in fact not yet been tested in running code and
so might actually be broken.  This module might in fact work for you now,
but it is officially alpha quality.  Please use it with caution.>

B<If you want to help out with this module's development, generally the
most helpful thing you can do to start out is to flesh out the test suite.
I suggest looking at the test suites of other Set:: modules as well as
various database related modules as inspiration or a source for copying.>

Loosely speaking, a Set::Relation object is a wrapper over a set of hash
refs, where all the hash refs in the set have the same number of elements
and the same set of hash keys; so in this context, a Perl hash ref and a
Set::Relation object correspond to the relational model concepts of a tuple
and a relation, respectively.  A relation is analogous to a SQL row-set
but that the columns and rows are not ordered, and there are no duplicate
column names or duplicate rows.  Set::Relation provides all the normal
operators of other Set:: classes, such as 'is_subset', 'union',
'diff' etc, but it also provides operators like 'join', 'quotient',
'projection', 'group', 'summary', 'semijoin', 'restriction',
'semidiff', 'tclose', 'outer_join' etc.

Note, you can model a whole relational database by having a hash ref whose
keys are akin to SQL table names and whose values are Set::Relation
objects.

The name Set::Relation was chosen because it seems the most descriptive.  A
'relation' is a value, same as an integer or an array is a value, and a
relation can do everything a generic set can do plus more.  The Set::
namespace is used to reduce confusion amongst other concepts of the word
'relation', as some people think it means 'compare'; Set:: illustrates that
this class' objects are functionally set-like collection values.

I<This documentation is pending.>

=head2 Appropriate Uses For Set::Relation

Set::Relation I<is> intended to be used in production environments
I<(except for the fact it is mostly untested at the moment)>.  It has
been developed according to a rigorously thought out API and behaviour
specification, and it should be easy to learn, to install and use, and to
extend.  It is expected to be maintained and supported by the original
author over the long term, either standalone or by the author providing an
effective migration path to a suitable replacement.

At the same time, the primary design goal of Set::Relation is to be simple.

Set::Relation focuses on providing all the operators of the relational
model of data to Perl developers in as concise a manner as is possible
while focusing on correctness of behaviour above all else, and also
focusing on ease of understanding and maintainability, since generally a
developer's time is the most valuable resource for us to conserve.

Despite initial design efforts to help Set::Relation's execution (CPU, RAM,
etc) performance, this module is still assumed to be very un-optimized for
its conceptually low level task of data crunching.  It generally applies
the same internal representation and algorithms regardless of the actual
structure or meaning of the data, and regardless of the amount of data.

This module is still assumed to be considerably, perhaps an order or three
of magnitude, slower than a hand-rolled task-specific solution.  If your
primary concern is execution performance, you will most likely not want to
use Set::Relation but rather hand-code what it does specifically for your
task with your specific data, or alternately employ some other dependency
to do the work for you (or even, if necessary, write the task in C).

Set::Relation is best used in situations where you either want to just get
some correct solution up and working quickly (conserving developer time),
such as because it is a prototype or proof of concept, or where your data
set is relatively small (so Set::Relation's overhead cost is less
noticeable), or where your task is one that is less time sensitive like a
batch process where a longer execution time isn't harmful.

Some specific suggested uses for Set::Relation are:

=over

=item Flat File Processing

Use it to simplify some kinds of batch processing data from flat files such
as CSV text files; a Set::Relation object can be used to store the content
of one source file, and then the relational operators can be used to easily
join or filter the file contents against each other, and eventually reports
or other results be produced.

=item SQL Generation

Use it when gathering and pre-processing data that needs to end up in a SQL
database for longer term use.  If you generate your INSERT SQL from
Set::Relation objects then those objects can help you do it all in a batch
up front, and Set::Relation can help you test for duplicates or various
kinds of dirty data that might violate database constraints.  So it is less
likely that you would need to connect to your SQL database interactively to
test your data against it before insertion, and it is more likely you can
just talk to it once when your single batch of SQL INSERTs is ready to go.

=item Database APIs

Various DBMS wrappers, ORMs, persistence tools, etc can use Set::Relation
objects internally or as part of their API to represent database row sets.
Wrappers that like to do some database-like work internally, such as
associating parent and child row sets, or testing key constraints, or
various other tasks can use Set::Relation to do some of their work for
them, making development and maintenance of said tools easier.  Note that
in general this would fall under the "small data set" use category since a
large number of applications, particularly web apps, just access or display
from one to a hundred rows at a time.

=item Testing

Since it represents row-sets and provides all the relational operators,
with a focus on correctness, Set::Relation should be useful in helping to
test all sorts of other code intended to work with databases, particularly
code that is a wrapper for a database, as a basis for comparison to whether
the other code is having correct behaviour or not.  For example, it could
help test that code which generates and runs SQL is producing the correct
results with various inputs and scenarios.

=item Teaching

Set::Relation should be helpful in teaching the relational model to people,
helping them to know what is really going on conceptually with different
operations, without being distracted by a lot of ancillary matters, and
without being distracted by limitations of various DBMSs that may not
expose the whole relational model or may do it incorrectly.  It provides
something students can experiment with right now.

=item General List or Set Operations

Set::Relation is complementary to the things you do with Perl's built-in
Array and Hash types, including 'map' and 'grep' operations.  It is useful
when you want to do miscellaneous combining or filtering of lists of data
against other lists, particularly multi-dimensional ones, or helping in
summarizing lists of data for reports.  Maybe helping with some tasks that
are easier in Perl 6 than in Perl 5, when you're using Perl 5.

=back

Of course, like any generic tool, Set::Relation should be widely applicable
in many different situations.

Now, another situation where you may not want to use Set::Relation is when
its sibling project L<Muldis Rosetta|Muldis::Rosetta> would serve you
better.  In contrast to Set::Relation, which is standalone and intended to
integrate closely with Perl, Muldis Rosetta implements a whole programming
language distinct from Perl, L<Muldis D|Muldis::D>, and presents a superior
environment at large for working with the relational model of data; you use
it sort of like how you use L<DBI> to talk to a SQL DBMS, as a separate
thing walled off from Perl.  The benefits of using Muldis Rosetta over
Set::Relation are multiple, including much better performance and
scalability, and that it can directly persist data as you'd expect a DBMS
to do, as well as provide easy access to many other relational model
features like stronger typing and arbitrary database constraints, and
nested transactions, as well as access to full powered DBMS engines like
PostgreSQL and Oracle and SQLite (though you don't have to use those and
Muldis Rosetta can be used purely implemented in Perl).

I<That all said, Set::Relation is actually implemented and works today,
while Muldis Rosetta is still under construction and you can't use it yet.>

That brings out another important reason why Set::Relation exists now; it
also serves as a proof of concept for a main part of Muldis D and Muldis
Rosetta, or for a so-called "truly relational DBMSs" in general.  It
demonstrates ideal features and behaviour for relational operators, in a
functioning form that users can experiment with right now.  Set::Relation
is also meant to serve as inspiration for similar projects, and better
illustrate features that would be nice for modern programming languages to
have built-in, same as they have collection types like ordered and
associative arrays and one-dimensional sets and bags built-in.  It I<is>
reasonable for standard equipment to not just be plain set operators but
the other relational model operators too, such as relational join.

I<Note:  Assistance is appreciated in making Set::Relation perform better,
such that some of its present-day caveats can disappear, keeping in mind
its general design goals.  But generally first we will need to have a
complete test suite to confirm correct behaviour, and correct any
outstanding API issues, and preferably also a benchmark suite.  That way,
when the likely extensive changes are made for performance, we can actually
measure their effect and know we didn't break the behaviour.  Also if the
behaviour/API is nailed first, then new benchmarks we make along the way
will be backwards compatible with previous baseline versions.  But keep in
mind that only some kinds of improvements are suitable; some others may be
inordinately complex and may better be relegated to Muldis Rosetta or other
overlapping solutions instead; the distinction can be discussed
case-by-case as solutions are proposed.>

=head2 Matters of Value Identity

The relational model of data fundamentally involves values as being
immutable and distinct values having distinct identities, and the type of a
value is part of its identity.

The relational model has 3 kinds of types and values, called scalar
types/values, tuple types/values, and relation types/values; the 3 are all
mutually disjoint and no value in one kind can be identical to a value in
another.  A Set::Relation-doing class treats the objects of all
Set::Relation-doing classes as relation values, it treats ordinary
un-blessed Perl hash-refs as tuple values, and it treats all other Perl
values (and Perl undef) as scalar values.

This documentation section outlines how Set::Relation considers every kind
of Perl value in regards to its value identity, which has a large role to
play in the semantics of Set::Relation's routines.  Since a tuple is a set
of zero or more distinctly named attribute values and a relation is a set
of zero or more distinct-valued tuples having the same set of attribute
names, matters of whether 2 Perl values are considered duplicates or not
depends on this concept of identity.

A tuple/relation attribute name is exactly the same as a Perl Hash key; it
is a defined non-reference case/everything-sensitive character string of
zero or more characters.

Two tuples/Perl-Hashes are considered identical iff they are of the same
degree (same number of elements), have the same keys, and have
corresponding values that individually are considered identical.

Two relation/Set::Relation objects are considered identical iff they are of
the same degree (same number of attributes), have the same attribute names,
are of the same cardinality (same number of tuples), and every tuple in one
corresponds to a distinct tuple in the other, individually considered
identical.  Other object attributes of a Set::Relation object such as
keys/key-constraints or member identity caches have no bearing on the value
identity of a Set::Relation.

Two Perl undefined values are considered identical to each other, and
distinct from all defined values.

Two defined non-reference Perl values are considered identical iff their
string representations are identical; either is distinct from Perl undef
and from all reference/blessed Perl values.

Two non-blessed reference non-Hash Perl values are considered identical to
each other iff they stringify to the same string representations, meaning
essentially if they are the very same reference / have the same memory
address; either is distinct from any blessed reference or non-reference or
Hash-ref Perl value.

Two blessed reference / Perl object non-Set::Relation Perl values are
considered identical to each other iff they both are blessed into the same
Perl class I<and> they both stringify to the same string representations,
meaning generally that either they are the very same reference or their
class overloads stringification (in which case we treat their class as a
value type); either is distinct from any non-blessed Perl value or
Set::Relation object.

Loosely speaking, a Set::Relation-doing class treats the objects of all
Set::Relation-doing classes, and Perl Hash-refs and non-reference or
undefined Perl values, as being of value types, that compare on their
actual values, while treating all other Perl reference or blessed values as
being reference types, that compare on their memory addresses.  Value types
can't be mutated while the containers that the reference values point to
can be mutated.  If you want some other class' object treated as a value
type, make it overload stringification I<(or an alternate/additional
convention can be devised like Set::Relation's own 'which' convention)>.
If you want a Hash-ref/tuple or Set::Relation object to be treated as a
reference type, then pass it around using another layer of reference
indirection, such as by adding a scalar-ref up front.

I<Note: Were this Perl 6, we would basically just use the standard C<WHICH>
method or C<===> comparison operator to determine identity; but Perl 5
doesn't have that so we do the aforementioned instead.>

=head2 Matters of Correctness

Set::Relation explicitly supports implementations that want to give users
the option of trading away specified guarantees of correctness for their
user-visible behaviour in exchange for better performance (by letting the
implementation be lazier), when an approximation is good enough for the
users' purposes.  But Set::Relation requires that implementations fully
guarantee correctness (insomuch as they reasonably have the power to do so)
for their user-visible behaviour by default, and only trade this for
laziness when users explicitly request the trade.  Of course,
implementations can always be internally lazy as they see fit where it
doesn't impact their externally observable semantics, so the trading
feature under discussion here is only about behaviour that users can see.

For example, some Set::Relation routines have the optional boolean
parameter C<$allow_dup_tuples> which, when explicitly given a true
argument, tells the implementation it may save itself the work of comparing
tuples with each other to test their uniqueness, and so multiple instances
of the same tuple value may be treated in user-visible ways as being
distinct when they logically aren't.  For example, if users construct a
Set::Relation-doing object using a list of tuples that contains duplicates,
then by default that object's C<cardinality> method will only result in a
count of distinct tuples, and that object's C<body> method will only result
in a list containing distinct tuples, because a relation is logically a set
of tuples not a multiset; but if a true C<$allow_dup_tuples> argument is
given to either method, then the result may include duplicates as if the
class were implementing a multiset rather than a set.  Similarly, internal
duplicates can arise out of none in many other situations than object
construction, such as from a relational union or projection.  Note that
because Perl closures in general are not pure and so might have different
results or side-effects from repeated invocations with the same arguments,
the C<$allow_dup_tuples> parameter is also present on methods like
C<restriction> and C<extension>.

It is important to realize that even when users explicitly waive a need for
correctness, the implementation may choose to ignore them and give the
fully correct answers anyway.  Trade-off options should not be construed as
having a guarantee to not do the normal behaviour, and in particular a true
C<$allow_dup_tuples> argument will I<not> guarantee consistent logical
multiset behaviour.

It is an design decision feature of Set::Relation that flags like
C<$allow_dup_tuples> are only available directly on the methods where they
would take effect just on specific invocations, and are not available as
object attributes.  This is to prevent action at a distance, where one spot
in code requesting less accuracy doesn't also cause other spots in code
using the same objects to also get less accuracy.  And so you may have to
use said flag explicitly multiple times to get continuous such laziness
through the lifecycle of an object or chain of derived objects.

=head2 Matters of Mutability

This module defines both the Set::Relation role and the
Set::Relation::Mutable role, the latter of which composes the former;
objects of a class composing just Set::Relation are guaranteed to never
mutate; objects also/instead composing the latter may some times mutate.

Now, while a relation value is conceptually immutable, the
Set::Relation::Mutable role allows you to mutate a Set::Relation::Mutable
object under some circumstances as a convenience to users, in a similar
manner to how you can mutate a Perl Hash or Array by inserting or deleting
its elements.  By default, a newly created Set::Relation::Mutable object is
mutable, that is its identity is said to not be frozen; but when you
perform certain operations with one, it will become immutable, gaining a
frozen identity, and this change can not be reversed, though you can clone
said object to get an un-frozen duplicate.

You can make a Set::Relation::Mutable object immutable explicitly by
invoking its C<freeze_identity> method.  This change may also happen
implicitly, with those happenings being implementation-dependent.  For
example, invoking an object's C<which> method may freeze the object.  Or
for another example, if another Set::Relation::Mutable object is
constructed that is given the first object as a tuple attribute value; this
could be done rather than cloning the input object under the assumption
that most of the time you wouldn't want to mutate the input object
afterwards, for efficiency.  These details are subject to change.

=head2 Matters of Performance

Note:  See also the L</Appropriate Uses For Set::Relation> section above.

The matters of performance can differ significantly depending on the
implementation class, so see each of their corresponding documentation
sections for details: L<Set::Relation::V1>, L<Set::Relation::V2>.

=head1 INTERFACE

The interface of Set::Relation is entirely object-oriented; you use it by
creating objects from its member classes (or more specifically, of
implementing classes that compose its member roles) and then invoking
methods on those objects.  All of their attributes are private, so you must
use accessor methods.

The usual way that Set::Relation indicates a failure is to throw an
exception; most often this is due to invalid input.  If an invoked routine
simply returns, you can assume that it has succeeded, even if the return
value is undefined.

=head1 THE Set::Relation ROLE

A C<Set::Relation> object represents a single relation value.  For any
class composing this role, its objects guarantee that their value will
never mutate following object construction, unless that class also composes
the C<Set::Relation::Mutable> role.

=head1 Constructor Submethods

This is currently the only routine declared by Set::Relation that you
invoke off of the class name; currently you invoke all other routines off
of a Set::Relation object.

=head2 new

C<multi submethod new of Set::Relation (Array|Set::Relation|Str :$members,
Array|Str :$keys?)>

C<multi submethod new of Set::Relation (Array|Set::Relation|Str $members)>

C<multi submethod new of Set::Relation ()>

This constructor submethod creates and returns a new C<Set::Relation>
object, representing a single relation value, that is initialized primarily
using the multi-dimensional Perl data structure given in its C<$members>
argument, which defines the member attributes and tuples of the new
relation.  The allowed formats of a C<$members> argument mostly are those
defined by the Muldis D language dialect
L<HDMD_Perl5_STD|Muldis::D::Dialect::HDMD_Perl5_STD> for a C<Relation>
value literal node payload, except that attribute values are just ordinary
Perl values and not HDMD_Perl5_STD value literal nodes.  Examples are:

    sub relation { return $sr_class_name->new( @_ ); }

    # Zero attrs + zero tuples.
    my $r1 = relation( [] );

    # 3 attrs + zero tuples.
    my $r2 = relation( [ 'x', 'y', 'z' ] );

    # Zero attrs + 1 tuple
    my $r3 = relation( [ {} ] );

    # Named attributes format: 3 attrs + 1 tuple.
    my $r4 = relation( [
        {
            'login_name' => 'hartmark',
            'login_pass' => 'letmein',
            'is_special' => 1,
        },
    ] );

    # Ordered attributes format: 2 attrs + 1 tuple.
    my $r5 = relation( [ [ 'name', 'age' ], [
        [ 'Michelle', 17 ],
    ] ] );

However, Set::Relation also supports a few additional, trivial formats for
C<$members>, as illustrated here:

    # The default value of a Set::Relation has zero attrs + zero tuples.
    my $r6 = relation();

    # One way to clone a relation object.
    my $r7 = relation( $r5 );

    # Abbreviated way to specify 1 attr + zero tuples.
    my $r8 = relation( 'value' );

If the optional argument C<$keys> is defined, then it defines one or more
explicit candidate keys (or unique key constraints) for the relation value,
all of which C<$members> must satisfy or else C<new> will fail; if C<$keys>
is not defined, then by default the new relation has a single implicit
candidate key ranging over all of its attributes.  The canonical format of
a C<$keys> argument is a Perl Array of 0..N Perl Array of 0..N Perl Str,
where each inner Array defines a single key and each Str defines one of the
relation's attributes that the key ranges over.  Examples are:

    # No explicit keys (just an implicit key over all attributes).
    my $rI = relation( members => [ 'x', 'y', 'z' ], keys => [] );

    # Explicit nullary key (over 0 attrs; rel may have just 0..1 tuples).
    my $rN = relation( members => [ 'x', 'y', 'z' ], keys => [ [] ] );

    # Explicit unary key (over 1 attribute).
    my $rU = relation( members => [ 'x', 'y', 'z' ], keys => [ [ 'z' ] ] );

    # Explicit binary key (over 2 attributes).
    my $rB = relation(
        members => [ 'x', 'y', 'z' ], keys => [ [ 'x', 'z' ] ] );

    # Explicit ternary key (over 3 attributes).
    my $rT = relation(
        members => [ 'x', 'y', 'z' ], keys => [ [ 'x', 'y', 'z' ] ] );

    # Explicit unary key plus explicit binary key.
    my $rUB = relation(
        members => [ 'x', 'y', 'z' ], keys => [ [ 'x' ], [ 'y', 'z' ] ] );

    # Pair of explicit binary keys.
    my $rBB = relation( members => [ 'x', 'y', 'z' ],
        keys => [ [ 'x', 'y' ], [ 'y', 'z' ] ] );

Alternately, C<$keys> may be either a Perl Array of 1..N Perl Str, that
defines a single key over N attributes, or C<$keys> may be a Perl Str, that
defines a single key over a single attribute.  Examples are:

    # Explicit unary key (over 1 attribute).
    my $rU2 = relation( members => [ 'x', 'y', 'z' ], keys => 'x' );
    my $rU3 = relation( members => [ 'x', 'y', 'z' ], keys => [ 'y' ] );

    # Explicit binary key (over 2 attributes).
    my $rB2 = relation(
        members => [ 'x', 'y', 'z' ], keys => [ 'x', 'y' ] );

Note that if any explicit key is a superkey of another, that is, the set of
attributes that the former ranges over is a proper superset of those that
the latter ranges over, then the superkey will be silently eliminated since
it is completely redundant; similarly, if any explicit key is the nullary
key, then it is the only key.

Note that any explicit key given to C<new> is only a constraint as far as
the initial value of the new Set::Relation object is concerned.  Any
subsequent mutation of the object by C<insert>, if the object is mutable,
is allowed to invalidate any prior-existing keys.  So 'keys' is simply a
reflection of explicitly tested for candidate keys that are still known to
be valid.  I<Note: it is possible in the future that subsequent feature
additions can permit these keys to also act as constraints against future
mutations.>

=head1 Accessor Methods

These Set::Relation object methods are mainly about extracting object
attributes, essentially the reverse process of an object constructor.

=head2 export_for_new

C<method export_for_new of Hash ($self: Bool|Array $want_ord_attrs?,
Bool $allow_dup_tuples?)>

This method results in a Perl Hash value whose Hash keys and values you can
give as argument names and values to C<new> such that the latter would
result in a clone of this method's invocant.  In other words,
C<export_for_new> is the inverse function to C<new>.  If
this method's C<$want_ord_attrs> argument is missing or false, then the
exported attributes and tuples are in named attributes format; if that
argument is true, they are in ordered attributes format.  If
C<$want_ord_attrs> is a Perl Array then it is assumed to contain a list of
attribute names matching those of the invocant, and so it is specifying
what order the attributes should be in the result; otherwise if
C<$want_ord_attrs> is the Perl string value C<1>, then the result will have
its attributes ordered alphabetically by attribute name (see the C<heading>
method docs for why that is the case).  If this method's
C<$allow_dup_tuples> argument is true, then the result may have
duplicate tuples; otherwise (the default), all result tuples are unique.

=head2 which

C<method which of Str ($self:)>

This method results in a character string representation of the invocant's
value identity.  The identity value result of this method is essentially a
serialization of all the invocant's attribute names and tuple values, all
of which are encoded and sorted in such a way that any 2 Set::Relation
values having the same attributes and tuples are guaranteed to have the
same value identity, and any 2 with different attributes or tuples are
guaranteed to have different ones.  This method is analogous to the special
C<WHICH> method of Perl 6 and lets you treat Set::Relation as a "value
type".  That all being said, currently the details of the serialization
format are implementation dependent, and it is assumed that an application
will exclusively use a single Set::Relation implementation in any
situations where code external to a Set::Relation-doing class is invoking
C<which> (code inside said classes will do conversions as needed).

=head2 members

C<method members of Array ($self: Bool|Array $want_ord_attrs?,
Bool $allow_dup_tuples?)>

This method results in a Perl Array value as per the 'members' element of
the Hash that C<export_for_new> would result in with the same invocant and
with the same arguments.

=head2 heading

C<method heading of Array ($self:)>

This method results in a Perl Array value whose elements are the attribute
names of the invocant.  The attribute names are sorted alphabetically so
that if this method's result is used together with the result of invoking
C<body> on the same invocant with a non-Array but true valued
C<$want_ord_attrs> then the default order of the ordered attributes
resulting from C<body> matches the default order resulting from C<heading>;
in contrast, if C<body> was invoked to return attributes in named format,
it doesn't matter what order C<heading> returns their names in.  This
method is currently an alias for the C<attr_names> functional method.

=head2 body

C<method body of Array ($self: Bool|Array $want_ord_attrs?,
Bool $allow_dup_tuples?)>

This method results in a Perl Array value whose elements are the tuples of
the invocant.  Each tuple is either a Perl Hash or a Perl Array depending
on the value of the C<$want_ord_attrs>, like with the C<members> method;
similarly C<$allow_dup_tuples> affects the result as with C<members>.

=head2 slice

C<method slice of Array ($self: Array|Str $attr_names,
Bool $want_ord_attrs?, Bool $allow_dup_tuples?)>

This method is like C<body> except that the result has just a subset of the
attributes of the invocant, those named by C<$attr_names>.  Unlike using
C<projection> followed by C<body> to do this, any duplicate subtuples are
retained in the result of C<slice>.  Each result subtuple is either a Perl
Hash or a Perl Array depending on the value of the C<$want_ord_attrs>, like
with C<body>, except that C<$want_ord_attrs> may only be a Bool here; when
that argument is true, the exported attributes are in the same order as
specified in C<$attr_names>.  If this method's C<$allow_dup_tuples>
argument is false (the default), then the result is guaranteed to have the
same number of elements as the cardinality of C<$self>; otherwise, the
result may have more elements.

=head2 attr

C<method attr of Array ($self: Str $name, Bool $allow_dup_tuples?)>

This method is like C<slice> except that the result has exactly one of the
invocant's attributes, the one named by C<$name>, and each result element
is that attribute value directly, not a single-element tuple.  This method
is expected to see a lot of use in relation summarizing operations, for
extracting the input values for reduction or aggregate operators.

=head2 keys

C<method keys of Array ($self:)>

This method results in a Perl Array value as per the 'keys' element of the
Hash that C<export_for_new> would result in with the same invocant; the
result format is the canonical one for the C<$keys> parameter of C<new>.

=head1 Single Input Relation Functional Methods

These Set::Relation object methods are pure functional, each one whose
execution results in a value and each one not mutating anything or having
any side-effects; the sole exception to the latter is that some may mutate
certain internals of some existing Set::Relation objects in ways that don't
affect their value identity, such as by creating indexes to speed up
current and future operations with said objects.

These methods each have a single Set::Relation object as input, which is
the invocant.  Some of them also result in a Set::Relation object while
others do not.

=head2 degree

C<method degree of UInt ($topic:)>

This functional method results in the degree of its invocant (that is, the
count of attributes it has).

=head2 is_nullary

C<method is_nullary of Bool ($topic:)>

This functional method results in true iff its invocant has a degree of
zero (that is, it has zero attributes), and false otherwise.

=head2 has_attrs

C<method has_attrs of Bool ($topic: Array|Str $attr_names)>

This functional method results in true iff, for every one of the attribute
names specified by its argument, its invocant has an attribute with that
name; otherwise it results in false.  As a trivial case, this method's
result is true if its argument is empty.  Note that C<has_attrs> has a
side-effect in that subsequent calls to C<keys> will include the key tested
for with C<has_attrs> if the latter had returned true, assuming an
intermediate call to C<insert> hadn't invalidated that.

=head2 attr_names

C<method attr_names of Array ($topic:)>

This functional method results in the set of the names of the attributes of
its invocant.  This method is currently an alias for the C<heading>
accessor method.

=head2 cardinality

C<method cardinality of UInt ($topic: Bool $allow_dup_tuples?)>

This functional method results in the cardinality of its invocant (that is,
the count of tuples its body has).  If this method's C<$allow_dup_tuples>
argument is false (the default), then the result is guaranteed to only
count the number of distinct tuples of C<$topic>; otherwise, the result may
be higher, unless the invocant is empty, in which case the result is still
exactly zero.  Note that this operation is also known as I<count> or C<R#>.

=head2 count

C<method count of UInt ($topic: Bool $allow_dup_tuples?)>

This functional method is an alias for C<cardinality>.

=head2 is_empty

C<method is_empty of Bool ($topic:)>

This functional method results in true iff its invocant has a cardinality
of zero (that is, it has zero tuples), and false otherwise.

=head2 has_member

C<method has_member of Bool ($r: Array|Hash $t)>

This functional method results in true iff all of the tuples of its C<$t>
argument match tuples of its invocant (that is, iff conceptually C<$t> is a
member of C<$r>), and false otherwise.  Note that this operation is also
known as C<∋> or C<hmem>.

=head2 has_key

C<method has_key of Bool ($topic: Array|Str $attr_names)>

This functional method results in true iff its invocant has a (unique) key
over the subset of its attributes whose names are specified by its
argument; otherwise it results in false.  This function will fail if its
invocant does not have all of the attributes named by its argument.  As a
trivial case, this function's result is true if its invocant is empty.

=head2 empty

C<method empty of Set::Relation ($topic:)>

This functional method results in the empty relation of the same heading of
its invocant, that is having the same degree and attribute names; it has
zero tuples.

=head2 insertion

C<method insertion of Set::Relation ($r: Array|Hash $t)>

This functional method results in a relation that is the relational union
of C<$r> and a relation whose tuples are C<$t>; that is, conceptually the
result is C<$t> inserted into C<$r>.  As a trivial case, if all of C<$t>
already exist in C<$r>, then the result is just C<$r>.

=head2 deletion

C<method deletion of Set::Relation ($r: Array|Hash $t)>

This functional method results in a relation that is the relational
difference from C<$r> of a relation whose tuples are C<$t>; that is,
conceptually the result is C<$t> deleted from C<$r>.  As a trivial case, if
all of C<$t> already doesn't exist in C<$r>, then the result is just C<$r>.

=head2 rename

C<method rename of Set::Relation ($topic: Hash $map)>

This functional method results in a relation value that is the same as its
C<$topic> invocant but that some of its attributes have different names.
Each element of the Hash argument C<$map> specifies how to rename one
C<$topic> attribute, with the element's Hash key and Hash value
representing the new and old names of a C<$topic> attribute, respectively.
As a trivial case, this method's result is C<$topic> if C<$map> has no
elements.  This method supports renaming attributes to each others' names.
This method will fail if C<$map> specifies any old names that C<$topic>
doesn't have, or any new names that are the same as C<$topic> attributes
that aren't being renamed.  Note that this operation is also known as C<<
@{<-} >>.

=head2 projection

C<method projection of Set::Relation ($topic: Array|Str $attr_names)>

This functional method results in the relational projection of its
C<$topic> invocant that has just the subset of attributes of C<$topic>
which are named in its C<$attr_names> argument.  As a trivial case, this
method's result is C<$topic> if C<$attr_names> lists all attributes of
C<$topic>; or, it is a nullary relation if C<$attr_names> is empty.  This
method will fail if C<$attr_names> specifies any attribute names that
C<$topic> doesn't have.  Note that this operation is also known as C<@{}>.

=head2 cmpl_proj

C<method cmpl_proj of Set::Relation ($topic: Array|Str $attr_names)>

This functional method is the same as C<projection> but that it results in
the complementary subset of attributes of its invocant when given the same
argument.  Note that this operation is also known as C<@{!}>.

=head2 wrap

C<method wrap of Set::Relation ($topic: Str $outer, Array|Str $inner)>

This functional method results in a relation value that is the same as its
C<$topic> invocant but that, for each of its member tuples, some of its
attributes have been wrapped up into a new tuple-typed attribute, which
exists in place of the original attributes.  The C<$inner> argument
specifies which C<$topic> attributes are to be removed and wrapped up, and
the C<$outer> argument specifies the name of their replacement attribute.
The result relation has the same cardinality as C<$topic>.  As a trivial
case, if C<$inner> is empty, then the result has all the same attributes as
before plus a new tuple-typed attribute of degree zero; or, if C<$inner>
lists all attributes of C<$topic>, then for each of its member tuples
C<TT>, the result tuple C<TR> has a single attribute whose value is the
same as C<TT>.  This method supports the new attribute having the same name
as an old one being wrapped into it.  This method will fail if C<$inner>
specifies any attribute names that C<$topic> doesn't have, or if C<$outer>
is the same as a C<$topic> attribute that isn't being wrapped.  Note that
this operation is also known as C<< @{%<-} >>.

=head2 cmpl_wrap

C<method cmpl_wrap of Set::Relation ($topic: Str $outer,
Array|Str $cmpl_inner)>

This functional method is the same as C<wrap> but that it wraps the
complementary subset of attributes of C<$topic> to those specified by
C<$cmpl_inner>.  Note that this operation is also known as C<< @{%<-!} >>.

=head2 unwrap

C<method unwrap of Set::Relation ($topic: Array|Str $inner, Str $outer)>

This functional method is the inverse of C<wrap>, such that it will unwrap
a tuple-type attribute into its member attributes.  This method will fail
if C<$outer> specifies any attribute name that C<$topic> doesn't have, or
if C<$topic{$outer}> does not have a same-heading tuple value for every
tuple of C<$topic> (because then there would be no consistent set of
attribute names to extend C<$topic> with), or if an attribute of
C<$topic{$outer}> has the same name as another C<$topic> attribute.  Now,
C<unwrap> requires the extra C<$inner> argument to prevent ambiguity in the
general case where C<$topic> might have zero tuples, because in that
situation the names of the attributes to add to C<$topic> in place of
C<$topic{$outer}> can not be determined from C<$topic{$outer}>.  This
method will fail if C<$topic> has at least 1 tuple and C<$inner> does not
match the names of the attributes of C<$topic{$outer}> for every tuple of
C<$topic>.  Note that this operation is also known as C<< @{<-%} >>.

=head2 group

C<method group of Set::Relation ($topic: Str $outer, Array|Str $inner)>

This functional method is similar to C<wrap> but that the C<$topic>
attribute-wrapping transformations result in new relation-typed attributes
rather than new tuple-typed attributes, and moreover multiple C<$topic>
tuples may be combined into fewer tuples whose new relation-typed
attributes have multiple tuples.  This method takes a relation of N tuples
and divides the tuples into M groups where all the tuples in a group have
the same values in the attributes which aren't being grouped (and distinct
values in the attributes that are being grouped); it then results in a new
relation of M tuples where the new relation-valued attribute of the result
has the tuples of the M groups.  A grouped relation contains all of the
information in the original relation, but it has less redundancy due to
redundant non-grouped attributes now just being represented in one tuple
per the multiple tuples whose grouped attributes had them in common.  A
relation having relation-valued attributes like this is a common way to
group so-called child tuples under their parents.  As a trivial case, if
C<$inner> is empty, then the result has all the same tuples and attributes
as before plus a new relation-typed attribute of degree zero whose value
per tuple is of cardinality one; or, if C<$inner> lists all attributes of
C<$topic>, then the result has a single tuple of a single attribute whose
value is the same as C<$topic> (except that the result has zero tuples when
C<$topic> does).  This method supports the new attribute having the same
name as an old one being grouped into it.  This method will fail if
C<$inner> specifies any attribute names that C<$topic> doesn't have, or if
C<$outer> is the same as C<$topic> attributes that aren't being grouped.
Note that this operation is also known as I<nest> or C<< @{@<-} >>.

=head2 cmpl_group

C<method cmpl_group of Set::Relation ($topic: Str $outer,
Array|Str $group_per)>

This functional method is the same as C<group> but that it groups the
complementary subset of attributes of C<$topic> to those specified by
C<$group_per>.  Note that this operation is also known as C<< @{@<-!} >>.

=head2 ungroup

C<method ungroup of Set::Relation ($topic: Array|Str $inner, Str $outer)>

This functional method is the inverse of C<group> as C<unwrap> is to
C<wrap>; it will ungroup a relation-type attribute into its member
attributes and tuples.  A relation can be first grouped and then that
result ungrouped to produce the original relation, with no data loss.
However, the ungroup of a relation on a relation-valued attribute will lose
the information in any outer relation tuples whose inner relation value has
zero tuples; a group on this result won't bring them back.  This method
will fail if C<$outer> specifies any attribute name that C<$topic> doesn't
have, or if C<$topic{$outer}> does not have a same-heading relation value
for every tuple of C<$topic> (because then there would be no consistent set
of attribute names to extend C<$topic> with), or if an attribute of
C<$topic{$outer}> has the same name as another C<$topic> attribute.  Note
that this operation is also known as I<unnest> or C<< @{<-@} >>.

=head2 tclose

C<method tclose of Set::Relation ($topic:)>

This functional method results in the transitive closure of its invocant.
The invocant must be a binary relation whose attributes are both of the
same type (for whatever concept of "type" you want to have), and the result
is a relation having the same heading and a body which is a superset of the
invocant's tuples.  Assuming that the invocant represents all of the node
pairs in a directed graph that have an arc between them, and so each
invocant tuple represents an arc, C<tclose> will determine all
of the node pairs in that graph which have a path between them (a recursive
operation), so each tuple of the result represents a path.  The result is a
superset since all arcs are also complete paths.  The C<tclose>
method is intended to support recursive queries, such as in connection
with the "part explosion problem" (the problem of finding all components,
at all levels, of some specified part).

=head2 restriction

C<method restriction of Set::Relation ($topic: Callable $func,
Bool $allow_dup_tuples?)>

This functional method results in the relational restriction of its
C<$topic> invocant as determined by applying the Bool-resulting
zero-parameter Perl subroutine reference given in its C<$func> argument.
The result relation has the same heading as C<$topic>, and its body
contains the subset of C<$topic> tuples where, for each tuple, the
subroutine given in C<$func> results in true when the tuple is its C<$_>
topic.  As a trivial case, if C<$func> is defined to
unconditionally result in true, then this method results simply in
C<$topic>; or, for an unconditional false, this method results in the empty
relation with the same heading.  Note that this operation is also
known as I<where>.  See also the C<semijoin> method, which is
a simpler-syntax alternative for C<restriction> in its typical usage where
restrictions are composed simply of anded or ored tests for attribute value
equality.  If this method's C<$allow_dup_tuples> argument is false (the
default), then C<$func> is guaranteed to be invoked just once per distinct
tuple of C<$topic>; otherwise it might be multiple invoked.

=head2 restr_and_cmpl

C<method restr_and_cmpl of Array ($topic: Callable $func,
Bool $allow_dup_tuples?)>

This functional method performs a 2-way partitioning of all the tuples of
C<$topic> and results in a 2-element Perl Array whose element values are
each Set::Relation objects that have the same heading as C<$topic> and
complementary subsets of its tuples; the first and second elements are
what C<restriction> and C<cmpl_restr>, respectively, would result in
when having the same invocant and arguments.

=head2 cmpl_restr

C<method cmpl_restr of Set::Relation ($topic: Callable $func,
Bool $allow_dup_tuples?)>

This functional method is the same as C<restriction> but that it results in
the complementary subset of tuples of C<$topic> when given the same
invocant and arguments.  See also the C<semidiff> method.

=head2 classification

C<method classification of Set::Relation ($topic: Callable $func,
Str $class_attr_name, Str $group_attr_name, Bool $allow_dup_tuples?)>

This functional method conceptually is to C<restriction> what C<group> is
to C<semijoin>.  It classifies the tuples of C<$topic> into N groups using
the zero-parameter Perl subroutine reference given in C<$func>, such that
any distinct tuples are in a common group if the subroutine given in
C<$func> results in the same value when given either of those tuples as its
C<$_> topic.  This method conceptually is a short-hand for first extending
C<$topic> with a new attribute whose name is given in C<$class_attr_name>,
whose value per tuple is determined from C<$topic> using C<$func>, and then
grouping that result relation on all of its original attributes, with the
post-group RVA having the name given in C<$group_attr_name>; the result of
C<classification> is a binary relation whose 2 attributes have the names
given in C<$class_attr_name> and C<$group_attr_name>.  This method is
intended for use when you want to partition a relation's tuples into an
arbitrary number of groups using arbitrary criteria, in contrast with
C<restriction> where you are dividing into exactly 2 groups (and returning
one) using arbitrary criteria.  If this method's C<$allow_dup_tuples>
argument is false (the default), then C<$func> is guaranteed to be invoked
just once per distinct tuple of C<$topic>; otherwise it might be multiple
invoked.

=head2 extension

C<method extension of Set::Relation ($topic: Array|Str $attr_names,
Callable $func, Bool $allow_dup_tuples?)>

This functional method results in the relational extension of its C<topic>
invocant as determined by applying the tuple/Hash-resulting zero-parameter
Perl subroutine reference given in its C<$func> argument.  The result
relation has a heading that is a superset of that of C<$topic>, and its
body contains the same number of tuples, with all attribute values of
C<$topic> retained, and possibly extra present, determined as follows; for
each C<$topic> tuple, the subroutine given in C<$func> results in a second
tuple when the first tuple is its C<$_> topic; the first and second tuples
must have no attribute names in common, and the result tuple is derived by
joining (cross-product) the tuples together.  As a trivial case, if
C<$func> is defined to unconditionally result in the degree-zero tuple,
then this method results simply in C<$topic>.  Now, C<extension> requires
the extra C<$attr_names> argument to prevent ambiguity in the general case
where C<$topic> might have zero tuples, because in that situation, C<$func>
would never be invoked, and the names of the attributes to add to C<$topic>
are not known (we don't generally assume that C<extension> can
reverse-engineer C<$func> to see what attributes it would have resulted
in).  This method will fail if C<$topic> has at least 1 tuple and the
result of C<$func> does not have matching attribute names to those named by
C<$attr_names>.  If this method's C<$allow_dup_tuples> argument is false
(the default), then C<$func> is guaranteed to be invoked just once per
distinct tuple of C<$topic>; otherwise it might be multiple invoked.

=head2 static_exten

C<method static_exten of Set::Relation ($topic: Hash $attrs)>

This functional method is a simpler-syntax alternative to both C<extension>
and C<product> in the typical scenario of extending a relation, given in
the C<$topic> invocant, such that every tuple has mutually identical values
for each of the new attributes; the new attribute names and common values
are given in the C<$attrs> argument.

=head2 map

C<method map of Set::Relation ($topic: Array|Str $result_attr_names,
Callable $func, Bool $allow_dup_tuples?)>

This functional method provides a convenient one-place generalization of
per-tuple transformations that otherwise might require the chaining of up
to a half-dozen other operators like projection, extension, and rename.
This method results in a relation each of whose tuples is the result of
applying, to each of the tuples of its C<$topic> invocant, the
tuple/Hash-resulting zero-parameter Perl subroutine reference given in its
C<$func> argument.  There is no
restriction on what attributes the result tuple of C<$func> may have
(except that all tuples from C<$func> must have compatible headings); this
tuple from C<$func> would completely replace the original tuple from
C<$topic>.  The result relation has a cardinality that is the same as that
of C<$topic>, unless the result of C<$func> was redundant tuples, in which
case the result has appropriately fewer tuples.  As a trivial case, if
C<$func> is defined to unconditionally result in the same tuple as its own
C<$topic> argument, then this method results simply in C<$topic>; or, if
C<$func> is defined to have a static result, then this method's result
will have just 0..1 tuples.  Now, C<map> requires the extra
C<$result_attr_names> argument to prevent ambiguity in the general case
where C<$topic> might have zero tuples, because in that situation, C<$func>
would never be invoked, and the names of the attributes of the result are
not known (we don't generally assume that C<map> can reverse-engineer
C<$func> to see what attributes it would have resulted in).  This method
will fail if C<$topic> has at least 1 tuple and the result of C<$func> does
not have matching attribute names to those named by C<$result_attr_names>.
If this method's C<$allow_dup_tuples> argument is false (the default), then
C<$func> is guaranteed to be invoked just once per distinct tuple of
C<$topic>; otherwise it might be multiple invoked.

=head2 summary

C<method summary of Set::Relation ($topic: Array|Str $group_per,
Array|Str $summ_attr_names, Callable $summ_func, Bool $allow_dup_tuples?)>

This functional method provides a convenient context for using aggregate
functions to derive a per-group summary relation, which is its result, from
another relation, which is its C<$topic> invocant.  This method first
performs a C<cmpl_group> on C<$topic> using C<$group_per> to specify which
attributes get grouped into a new relation-valued attribute and which
don't; those that don't instead get wrapped into a tuple-valued attribute.
Then, per binary tuple in the main relation, this method applies the
tuple/Hash-resulting zero-parameter Perl subroutine reference given in its
C<$summ_func> argument; for each post-group main relation tuple, the
subroutine given in C<$summ_func> results in a second tuple when the first
tuple is its C<$_> topic; the C<$_> tuple has the 2 attribute names
C<summarize> and C<per>, which are valued with the relation-valued
attribute and tuple-valued attribute, respectively.  As per a subroutine
that C<extension> applies, the subroutine given in C<$summ_func>
effectively takes a whole post-grouping input tuple and results in a
partial tuple that would be joined by C<summary> with the C<per> tuple to
get the result tuple; the
applied subroutine would directly invoke any N-adic / aggregate operators,
and extract their inputs from (or calculate) C<summarize> as it sees fit.
Note that C<summary> is not intended to be used to summarize an entire
C<$topic> relation at once (except by chance of it resolving to 1 group);
you should instead invoke your summarize-all C<$summ_func> directly, or
inline it, rather than by way of C<summary>, especially if you want a
single-tuple result on an empty C<$topic> (which C<summary>) won't do.
Now, C<summary> requires the extra C<$summ_attr_names> argument to
prevent ambiguity in the general case where C<$topic> might have zero
tuples, because in that situation, C<$summ_func> would never be invoked,
and the names of the attributes to add to C<per> are not known (we don't
generally assume that C<summary> can reverse-engineer C<$summ_func> to see
what attributes it would have resulted in).  This method will fail if
C<$topic> has at least 1 tuple and the result of C<$summ_func> does not
have matching attribute names to those named by C<$summ_attr_names>.  If
this method's C<$allow_dup_tuples> argument is false (the default), then
C<$summ_func> is guaranteed to be invoked just once per distinct post-group
tuple; otherwise it might be multiple invoked.

=head2 cardinality_per_group

C<method cardinality_per_group of Set::Relation ($topic:
Str $count_attr_name, Array|Str $group_per, Bool $allow_dup_tuples?)>

This functional method is a convenient shorthand for the common use of
C<summary> that is just counting the tuples of each group.  This function
is like C<cmpl_group> but that the single added attribute, rather than an
RVA of the grouped C<$topic> attributes, has the cardinality that said RVA
would have had.  The result's heading consists of the attributes named in
C<$group_per> plus the attribute named in C<$count_attr_name> (a positive
integer).  Note that this operation is also known as C<< @{#@<-!} >>.

=head2 count_per_group

C<method count_per_group of Set::Relation ($topic:
Str $count_attr_name, Array|Str $group_per, Bool $allow_dup_tuples?)>

This functional method is an alias for C<cardinality_per_group>.

=head1 Multiple Input Relation Functional Methods

These Set::Relation object methods are pure functional, each one whose
execution results in a value and each one not mutating anything or having
any side-effects; the sole exception to the latter is that some may mutate
certain internals of some existing Set::Relation objects in ways that don't
affect their value identity, such as by creating indexes to speed up
current and future operations with said objects.

These methods each have at least 2 Set::Relation objects as input, one of
which is the invocant and the other of which is an additional argument.
Some of them also result in a Set::Relation object while others do not.

=head2 is_identical

C<method is_identical of Bool ($topic: Set::Relation $other)>

This symmetric functional method results in true iff its
invocant and argument are exactly the same value (that is, Set::Relation
considers them to have the same value identity), and false otherwise.  Note
that this operation is also known as I<is equal> or C<=>.

=head2 is_subset

C<method is_subset of Bool ($topic: Set::Relation $other)>

This functional method results in true iff the set of tuples comprising
C<$topic> is a subset of the set of tuples comprising C<$other> (both
must have the same heading regardless), and false otherwise.  Note that
this operation is also known as C<⊆> or C<sub>.

=head2 is_superset

C<method is_superset of Bool ($topic: Set::Relation $other)>

This functional method is an alias for C<is_subset> except that it
transposes the C<$topic> invocant and C<$other> argument.  This functional
method results in true iff the set of tuples comprising C<$topic> is a
superset of the set of tuples comprising C<$other> (both must have the same
heading regardless), and false otherwise.  Note that this operation is also
known as C<⊇> or C<super>.

=head2 is_proper_subset

C<method is_proper_subset of Bool ($topic: Set::Relation $other)>

This functional method is exactly the same as C<is_subset> except that it
results in false if its invocant and argument are identical. Note that this
operation is also known as C<⊂> or C<psub>.

=head2 is_proper_superset

C<method is_proper_superset of Bool ($topic: Set::Relation $other)>

This functional method is an alias for C<is_proper_subset> except that it
transposes the C<$topic> invocant and C<$other> argument.  This functional
method is exactly the same as C<is_superset> except that it results in
false if its invocant and argument are identical.  Note that this operation
is also known as C<⊃> or C<psuper>.

=head2 is_disjoint

C<method is_disjoint of Bool ($topic: Set::Relation $other)>

This symmetric functional method results in true iff the set of tuples
comprising each of its same-heading invocant and argument are mutually
disjoint, that is, iff the intersection of the invocant and argument is
empty; it results in false otherwise.

=head2 union

C<method union of Set::Relation ($topic: Array|Set::Relation $others)>

This functional method results in the relational union/inclusive-or of the
collective N element values of its same-heading invocant and argument,
hereafter referred to as C<$inputs>; it is a reduction operator that
recursively takes each pair of input values and relationally unions (which
is commutative, associative, and idempotent) them together until just one
is left, which is the result.  The result relation has the same heading as
all of its input relations, and its body contains every tuple that is in
any of the input relations.  The identity value of relational union is the
same-heading empty relation value (having zero tuples).  Note that this
operation is also known as C<∪>.

=head2 exclusion

C<method exclusion of Set::Relation ($topic: Array|Set::Relation $others)>

This functional method results in the relational exclusion/exclusive-or of
the collective N element values of its same-heading invocant and argument,
hereafter referred to as C<$inputs>; it is a reduction operator that
recursively takes each pair of input values and relationally excludes
(which is both commutative and associative) them together until just one is
left, which is the result.  The result relation has the same heading as all
of its input relations, and its body contains every tuple that is in just
an odd number of the input relations.  The identity value of relational
exclusion is the same as for C<union>.  Note that this operation is also
known as I<symmetric difference> or C<∆>.

=head2 symmetric_diff

C<method symmetric_diff of Set::Relation ($topic:
Array|Set::Relation $others)>

This functional method is an alias for C<exclusion>.

=head2 intersection

C<method intersection of Set::Relation ($topic:
Array|Set::Relation $others)>

This functional method results in the relational intersection/and of the
collective N element values of its same-heading invocant and argument,
hereafter referred to as C<$inputs>; it is a reduction operator that
recursively takes each pair of input values and relationally intersects
(which is commutative, associative, and idempotent) them together until
just one is left, which is the result.  The result relation has the same
heading as all of its input relations, and its body contains only the
tuples that are in every one of the input relations.  The identity value of
relational intersection is the same-heading universal relation value
(having all the tuples that could possible exist together in a common
relation value with that heading; this is impossibly large to represent in
the general case, except perhaps lazily).  Note that this C<intersection>
method is conceptually a special case of C<join>, applicable when the
headings of the inputs are the same, and C<join> will produce the same
result as this when given the same inputs, but with the exception that
relational intersection has a different identity value for zero inputs than
relational join has.  Note that this operation is also known as C<∩>.

=head2 diff

C<method diff of Set::Relation ($source: Set::Relation $filter)>

This functional method results in the relational difference when its
C<$filter> argument is subtracted from its same-heading C<$source>
invocant.  The result relation has the same heading as the input relations,
and its body contains only the tuples that are in C<$source> and are not in
C<$filter>.  Note that this I<diff> operator is conceptually a
special case of I<semidiff>, applicable when the headings of the
inputs are the same.  Note that this operation is also known as I<minus> or
I<except> or C<∖>.

=head2 semidiff

C<method semidiff of Set::Relation ($source: Set::Relation $filter)>

This functional method is the same as C<semijoin> but that it results in
the complementary subset of tuples of C<$source> when given the same
arguments.  Note that this operation is also known as I<antijoin> or
I<anti-semijoin> or I<semiminus> or C<!matching> or C<not-matching> or
C<⊿>.

=head2 antijoin

C<method antijoin of Set::Relation ($source: Set::Relation $filter)>

This functional method is an alias for C<semidiff>.

=head2 semijoin_and_diff

C<method semijoin_and_diff of Array ($source: Set::Relation $filter)>

This functional method performs a 2-way partitioning of all the tuples of
C<$source> and results in a 2-element Perl Array whose element values are
each Set::Relation objects that have the same heading as C<$source> and
complementary subsets of its tuples; the first and second elements are
what C<semijoin> and C<semidiff>, respectively, would result in when
having the same invocant and argument.

=head2 semijoin

C<method semijoin of Set::Relation ($source: Set::Relation $filter)>

This functional method results in the relational semijoin of its invocant
and argument.  The result relation has the same heading as C<$source>, and
its body contains the subset of C<$source> tuples that match those of
C<$filter> as per C<join>.  Note that relational semijoin is conceptually a
short-hand for first doing an ordinary relational join between C<$source>
and C<$filter>, and then performing a relational projection on all of the
attributes that just C<$source> has.  Note that this operation is also
known as C<matching> or C<⋉>.

=head2 join

C<method join of Set::Relation ($topic: Array|Set::Relation $others)>

This functional method results in the relational join (natural inner join)
of the collective N element values of its invocant and argument, hereafter
referred to as C<$inputs>; it is a reduction operator that recursively
takes each pair of input values and relationally joins (which is
commutative,
associative, and idempotent) them together until just one is left, which is
the result.  The result relation has a heading that is a union of all of
the headings of its input relations, and its body is the result of first
pairwise-matching every tuple of each input relation with every tuple of
each other input relation, then where each member of a tuple pair has
attribute names in common, eliminating pairs where the values of those
attributes differ and unioning the remaining said tuple pairs, then
eliminating any result tuples that duplicate others.  The identity value of
relational join is the nullary (zero attribute) relation value having a
single tuple.  As a trivial case, if any input relation has zero tuples,
then the method's result will too; or, if any input is the nullary
relation with one tuple, that input can be ignored (see identity value);
or, if any 2 inputs have no attribute names in common, then the join of
just those 2 is a cartesian product; or, if any 2 inputs have all attribute
names in common, then the join of just those 2 is an intersection; or, if
for 2 inputs, one's set of attribute names is a proper subset of another's,
then the join of just those two is a semijoin with the former filtering the
latter.  Note that this operation is also known as I<natural inner join> or
C<⋈>.

=head2 product

C<method product of Set::Relation ($topic: Array|Set::Relation $others)>

This functional method results in the relational cartesian/cross product of
the collective N element values of its invocant and argument, hereafter
referred to as C<$inputs>; it is conceptually a special case of C<join>
where all input relations have mutually distinct attribute names; unlike
C<join>, C<product> will fail if any inputs have attribute names in common.
Note that this operation is also known as I<cartesian join> or
C<cross-join> or C<×>.

=head2 quotient

C<method quotient of Set::Relation ($dividend: Set::Relation $divisor)>

This functional method results in the quotient when its C<$dividend>
invocant is divided by its C<$divisor> argument using relational division.
Speaking informally, say the relations C<$dividend> and C<$divisor> are
called C<A> and C<B>, and their attribute sets are respectively named
C<{X,Y}> and C<{Y}>, then the result relation has a heading composed of
attributes C<{X}> (so the result and C<$divisor> headings are both
complementary subsets of the C<$dividend> heading); the result has all
tuples C<{X}> such that a tuple C<{X,Y}> appears in C<A> for all tuples
C<{Y}> appearing in C<B>; that is, C<A ÷ B> is shorthand for C<A{X} ∖
((A{X} × B) ∖ A){X}>.  Note that this operation is also known as
I<divideby> or C<÷>.

=head2 composition

C<method composition of Set::Relation ($topic: Set::Relation $other)>

This symmetric functional method results in the relational composition of
its invocant and argument.  It is conceptually a short-hand for first doing
an ordinary relational join between the input relations, and then
performing a relational projection on all of the attributes that only one
of the arguments has; that is, the result has all of and just the
attributes that were not involved in matching the tuples of the inputs.

=head2 join_with_group

C<method join_with_group of Set::Relation ($primary:
Set::Relation $secondary, Str $group_attr)>

This functional method is a short-hand for first taking a (natural inner)
C<join> of its C<$primary> invocant and C<$secondary> argument, and then
taking a C<group> on all of the attributes that only the C<$secondary>
argument had, such that the attribute resulting from the group has the name
C<$group_attr>.  The result has 1 tuple for every tuple of C<$primary>
where at least 1 matching tuple exists in C<$secondary>.  This method
will fail if C<$group_attr> is the same name as any source attribute that
wasn't grouped.  This method is a convenient tool for gathering both parent
and child records from a database using a single query while avoiding
duplication of the parent record values.

=head1 Relational Ranking and Quota Functional Methods

These Set::Relation object methods are pure functional.  They are specific
to supporting ranking and quotas.

=head2 rank

C<method rank of Set::Relation ($topic: Str $name, Callable $ord_func)>

This functional method results in the relational extension of its C<$topic>
invocant by a single nonnegative-integer-typed attribute whose name is
provided by the C<$name> argument, where the value of the new attribute for
each tuple is the rank of that tuple as determined by the Order-resulting
zero-parameter Perl subroutine reference given in its C<$ord_func>
argument.  The subroutine compares tuples, similarly to Perl's built-in
C<sort> operator, with each invocation of it having a C<$_> topic whose
value is a binary tuple/Hash with attributes named C<a> and C<b>, each of
the latter having a C<$topic> tuple as its value.  The new attribute of
C<rank>'s result has a value of zero for its ranked-first tuple, and each
further consecutive ranked tuple has the next larger integer value.  Note
that C<rank> provides the functionality of SQL's "RANK" feature but that
the result of C<rank> is always a total ordering and so there is no "dense"
/ "not dense" distinction (however a partial ordering can be implemented
over it).

=head2 rank_by_attr_names

C<method rank_by_attr_names of Set::Relation ($topic: Str $name,
Array|Str $order_by)>

This functional method provides a convenient short-hand of C<rank> for the
common case of ranking tuples of a relation on a sequential list of its
named attributes; it simply takes a C<$order_by> array argument rather than
a Perl closure argument, and it ranks each pair of tuples by comparing
corresponding attribute values in the order that they are named in
C<$order_by>, stopping once a comparison doesn't result in I<same>.  Iff
C<$order_by> is a Str then it is equivalent to an C<$order_by> that is a
single-element Array whose element is that Str; the rest of this
documentation assumes that C<$order_by> is simply an Array.  Each
element of C<$order_by> is either a Str or a 1-3 element Array; in the
first case, that is simply the name of the attribute; in the second case,
the Array has these 1-3 elements in order: attribute name, "is reverse
order" direction indicator, and comparison operator.  This method will fail
if C<$order_by> contains any non-Str|Array elements, or if it names an
attribute that C<$topic> doesn't have.  The "is reverse order" direction
indicator is a boolean value; if it is false/undefined/missing then
ordering on that attribute will be as per usual for the comparator; if it
is true then the result is the reverse to what is usual.  This method will
fail if the comparison operator is defined and is anything other than
C<cmp> (string compare semantics) or C<< <=> >> (numeric compare
semantics); if it is undefined then C<cmp> (string) is the default.

=head2 limit

C<method limit of Set::Relation ($topic: Callable $ord_func,
UInt $min_rank, UInt $max_rank)>

This functional method results in the relational restriction of its
C<$topic> argument as determined by first ranking its tuples as per C<rank>
method (using C<$ord_func>) and then keeping just those tuples whose rank
is within the inclusive range specified by the C<$min_rank> and
C<$max_rank> arguments (C<rank>'s extra attribute is not kept).  The
C<limit> method implements a certain kind of quota query where all the
result tuples are consecutive in their ranks.  This method will fail if
C<$max_rank> is before C<$min_rank>.  It is valid for C<$min_rank> or
C<$max_rank> to be greater than the maximum rank of the source tuples; in
the first case, the result has zero tuples; in the second case, the result
has all remaining tuples starting at C<min_rank>.  If C<$topic> has any
tuples and C<$min_rank> matches the rank of a source tuple, then the result
will always have at least 1 tuple.  Note that C<limit> provides the
functionality of SQL's "LIMIT/OFFSET" feature in combination with "ORDER
BY" but that the result tuples of C<limit> do not remain ordered.

=head2 limit_by_attr_names

C<method limit_by_attr_names of Set::Relation ($topic: Array|Str $order_by,
UInt $min_rank, UInt $max_rank)>

This functional method is to C<limit> what C<rank_by_attr_names> is to
C<rank>.

=head1 Relational Substitution Functional Methods

These Set::Relation object methods are pure functional.  They are specific
to supporting substitutions.

=head2 substitution

C<method substitution of Set::Relation ($topic: Array|Str $attr_names,
Callable $func, Bool $allow_dup_tuples?)>

This functional method is similar to C<extension> except that it
substitutes values of existing relation attributes rather than adding new
attributes.  The result relation has the same heading as C<$topic>.  The
result tuple of the Perl subroutine reference given in C<$func> must have a
heading that is a subset of the heading of C<$topic>; corresponding values
resulting from the subroutine given in C<$func> will replace the values of
the tuples of C<$topic>.  The result relation has a cardinality that is the
same as that of C<$topic>, unless the result of any substitutions was
redundant tuples, in which case the result has appropriately fewer tuples.
As a trivial case, if C<$func> is defined to unconditionally result in
either the degree-zero tuple or in the same tuple as its own C<$topic>
argument, then this method results simply in C<$topic>; or, if C<$func> is
defined to have a static result and it replaces all attributes, then this
method's result will have just 0..1 tuples.  Now, strictly speaking,
C<substitution> could conceivably be implemented such that each result from
C<$func> is allowed to specify replacement values for different subsets of
C<$topic> attributes; however, to improve the method's predictability and
ease of implementation over disparate foundations, C<substitution> requires
the extra C<$attr_names> argument so that users can specify a consistent
subset that C<$func> will update (possibly to itself).  This method will
fail if C<$topic> has at least 1 tuple and the result of C<$func> does not
have matching attribute names to those named by C<$attr_names>.  If this
method's C<$allow_dup_tuples> argument is false (the default), then
C<$func> is guaranteed to be invoked just once per distinct tuple of
C<$topic>; otherwise it might be multiple invoked.

=head2 static_subst

C<method static_subst of Set::Relation ($topic: Hash $attrs)>

This functional method is a simpler-syntax alternative to C<substitution>
in the typical scenario where every tuple of a relation, given in the
C<$topic> invocant, is updated with identical values for the same
attributes; the new attribute values are given in the C<$attrs> argument.

=head2 subst_in_restr

C<method subst_in_restr of Set::Relation ($topic: Callable $restr_func,
Array|Str $subst_attr_names, Callable $subst_func,
Bool $allow_dup_tuples?)>

This functional method is like C<substitution> except that it only
transforms a subset of the tuples of C<$topic> rather than all of them.  It
is a short-hand for first separating the tuples of C<$topic> into 2 groups
where those passed by a relational restriction (defined by C<$restr_func>)
are then transformed (defined by C<$subst_attr_names> and C<$subst_func>),
then the result of the substitution is unioned with the un-transformed
group.  See also the C<subst_in_semijoin> method, which is a simpler-syntax
alternative for C<subst_in_restr> in its typical usage where restrictions
are composed simply of anded or ored tests for attribute value equality.

=head2 static_subst_in_restr

C<method static_subst_in_restr of Set::Relation ($topic:
Callable $restr_func, Hash $subst, Bool $allow_dup_tuples?)>

This functional method is to C<subst_in_restr> what C<static_subst>
is to C<substitution>.  See also the C<static_subst_in_semijoin> method.

=head2 subst_in_semijoin

C<method subst_in_semijoin of Set::Relation ($topic: Set::Relation $restr,
Array|Str $subst_attr_names, Callable $subst_func,
Bool $allow_dup_tuples?)>

This functional method is like C<subst_in_restr> except that the subset of
the tuples of C<$topic> to be transformed is determined by those matched by
a semijoin with C<$restr> rather than those that pass a generic relational
restriction.

=head2 static_subst_in_semijoin

C<method static_subst_in_semijoin of Set::Relation ($topic:
Set::Relation $restr, Hash $subst)>

This functional method is to C<subst_in_semijoin> what
C<static_subst> is to C<substitution>.

=head1 Relational Outer-Join Functional Methods

These Set::Relation object methods are pure functional.  They are specific
to supporting outer-joins.

=head2 outer_join_with_group

C<method outer_join_with_group of Set::Relation ($primary:
Set::Relation $secondary, Str $group_attr)>

This functional method is the same as C<join_with_group> except that it
results in a half-outer natural join rather than an inner natural join;
every tuple of C<$primary> has exactly 1 corresponding tuple in the result,
but where there were no matching C<$secondary> tuples, the result attribute
named by C<$group_attr> contains zero tuples rather than 1+.

=head2 outer_join_with_undefs

C<method outer_join_with_undefs of Set::Relation ($primary:
Set::Relation $secondary)>

This functional method results in a plain half-outer natural join of its
C<$primary> invocant and C<$secondary> argument where, for all result
tuples coming from a C<$primary> tuple that didn't match a C<$secondary>
tuple, the result attributes coming from just C<$secondary> are filled with
the Perl undef.

=head2 outer_join_with_static_exten

C<method outer_join_with_static_exten of Set::Relation ($primary:
Set::Relation $secondary, Hash $filler)>

This functional method is the same as C<outer_join_with_undefs> but that
C<$secondary>-sourced result attributes are not filled with the Perl undef;
rather, for result tuples from non-matches, the missing values are provided
explicitly from the C<$filler> argument, which is a tuple/Hash whose
heading matches the projection of C<$secondary>'s attributes that aren't in
common with C<$primary>, and whose body is the literal values to use for
those missing attribute values.

=head2 outer_join_with_exten

C<method outer_join_with_exten of Set::Relation ($primary:
Set::Relation $secondary, Callable $exten_func, Bool $allow_dup_tuples?)>

This functional method is the same as C<outer_join_with_static_exten> but
that the result tuples from non-matches are the result of performing a
relational extension on the un-matched C<$primary> tuples such that each
said result tuple is determined by applying the Perl subroutine given in
C<$exten_func> to each said C<$primary> tuple.  If this method's
C<$allow_dup_tuples> argument is false (the default), then C<$exten_func>
is guaranteed to be invoked just once per un-matched tuple of
C<$primary>; otherwise it might be multiple invoked.

=head1 THE Set::Relation::Mutable ROLE

A C<Set::Relation::Mutable> object is a C<Set::Relation> object that is
also allowed to have its value mutate at some times.

=head1 Constructor Submethods

=head2 new

C<multi submethod new of Set::Relation::Mutable (Array|Set::Relation|Str
:$members, Array|Str :$keys?, Bool :$has_frozen_identity?)>

The Set::Relation::Mutable role extends the C<new> constructor submethod of
the Set::Relation role to add the optional named-only parameter
C<$has_frozen_identity>; if its argument is true, then the new
Set::Relation::Mutable object is made value-immutable once initialized (its
identity is frozen); otherwise, if that argument is false or not provided,
then the new object is initially mutable (its identity is not frozen).

=head1 Accessor Methods

=head2 clone

C<method clone of Set::Relation::Mutable ($self:)>

This method results in a new Set::Relation::Mutable object that has an
exact clone of its invocant's attributes and tuples.  The new
Set::Relation::Mutable is initially a mutable object; its value identity is
not frozen, regardless of whether the invocant is frozen or not.

=head2 has_frozen_identity

C<method has_frozen_identity of Bool ($self:)>

This method results in true if the invocant is currently value-immutable,
and it results in false otherwise.

=head2 freeze_identity

C<method freeze_identity ($self:)>

This method causes the invocant to become value-immutable when invoked; it
freezes the invocant's value identity.  This change is not reversible (an
immutable Set::Relation::Mutable object can't be made mutable again),
however invoking C<clone> on said object will give you a mutable duplicate.

=head1 Mutator Methods

Invocations of these Set::Relation::Mutable object methods will cause their
invocants to mutate.  But they do not mutate any of their non-invocant
arguments.  These methods also result in their invocants post-mutation, for
the convenience of users that like to chain method calls.

=head2 evacuate

C<method evacuate of Set::Relation::Mutable ($topic:)>

This mutator method deletes all of the tuples in its invocant relation.
For a non-mutating equivalent, see the C<empty> functional method.

=head2 insert

C<method insert of Set::Relation::Mutable ($r: Array|Hash $t)>

This mutator method inserts its tuples argument into its invocant relation.
For a non-mutating equivalent, see the C<insertion> functional method.
Note that C<insert> is allowed to invalidate any currently existing
candidate keys of the invocant, in which case, the C<keys> method would no
longer list them; those keys are not constraints on future mutations.

=head2 delete

C<method delete of Set::Relation::Mutable ($r: Array|Hash $t)>

This mutator method deletes its tuples argument from its invocant relation.
For a non-mutating equivalent, see the C<deletion> functional method.

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 5.x.y that is at least 5.8.1, and
recommends one that is at least 5.10.1.

It also requires these Perl 5 packages that are on CPAN:
L<namespace::autoclean-ver(0.09..*)|namespace::autoclean>,
L<Moose::Role-ver(0.98..*)|Moose::Role>.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

The separate all-documentation distribution L<Muldis::D> is the formal
definition of the Muldis D language, a portion of which Set::Relation is
mainly based on.  The Muldis D language in turn has as a primary influence
the work of Christopher J. Date and Hugh Darwen whose home website is
L<http://www.thethirdmanifesto.com/>.

These Perl 5 packages that are in the current distribution are classes that
implement the Set::Relation roles: L<Set::Relation::V1>,
L<Set::Relation::V2>.

These other Perl 6 packages: L<Muldis::Rosetta>, L<Set>.

These other Perl 5 packages: L<Muldis::Rosetta>, L<Set::Object>,
L<Set::Scalar>.

=head1 BUGS AND LIMITATIONS

The Set::Relation library for Perl 5 does not make explicit attempts in
code to enforce privacy of the library's internals, besides not documenting
them as part of the public API.  (The Set::Relation library for Perl 6 is
different.)  That said, you should still respect that privacy and just use
the public API that Set::Relation provides.  If you bypass the public API
anyway, as Perl 5 allows, you do so at your own peril.

I<This documentation is pending.>

=head1 AUTHOR

Darren Duncan (C<darren@DarrenDuncan.net>)

=head1 LICENSE AND COPYRIGHT

Set::Relation is Copyright © 2006-2010, Muldis Data Systems, Inc.

L<http://www.muldis.com/>

Set::Relation is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License (LGPL) as
published by the Free Software Foundation (L<http://www.fsf.org/>); either
version 3 of the License, or (at your option) any later version.  You
should have received a copy of the LGPL as part of the Set::Relation
distribution, in the files named "LICENSE/LGPL" and "LICENSE/GPL" (the
LGPLv3 is defined as the terms of the GPLv3 plus extra permissions); if
not, see L<http://www.gnu.org/licenses/>.

If it is not feasible for you to employ Set::Relation subject to the terms
of the LGPL, then the copyright holder of Set::Relation can provide you a
customized proprietary license, often at no cost, so that it is still
possible for you to employ Set::Relation to meet your needs.

Any versions of Set::Relation that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits.  Set::Relation is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  However, for an
additional fee, the copyright holders of Set::Relation can sell you a
warranty for it.

While it is by no means required, the copyright holder of Set::Relation
would appreciate being informed any time you create a modified version of
Set::Relation that you are willing to distribute, because that is a
practical way of suggesting improvements to the standard version.

=head1 ACKNOWLEDGEMENTS

=over

=item Todd Hepler (C<thepler@employees.org>)

Thanks for proposing significant module design improvements and bug fixes,
providing files for the test suite, and giving other constructive input.

=back

=head1 FORUMS

Several public email-based forums exist whose main topic is all
implementations of the L<Muldis D|Muldis::D> language, especially the
L<Muldis Rosetta|Muldis::Rosetta> reference implementation, but also the
L<Set::Relation> module.  All of these you can reach via
L<http://mm.DarrenDuncan.net/mailman/listinfo>; go there to manage your
subscriptions to, or view the archives of, the following:

=over

=item C<muldis-db-announce@mm.DarrenDuncan.net>

This low-volume list is mainly for official announcements from the Muldis
Rosetta developers, though developers of Muldis Rosetta extensions can also
post their announcements here.  This is not a discussion list.

=item C<muldis-db-users@mm.DarrenDuncan.net>

This list is for general discussion among people who are using Muldis
Rosetta, which is not concerned with the implementation of Muldis Rosetta
itself.  This is the best place to ask for basic help in getting Muldis
Rosetta installed on your machine or to make it do what you want.  You
could also submit feature requests or report perceived bugs here, if you
don't want to use CPAN's RT system.

=item C<muldis-db-devel@mm.DarrenDuncan.net>

This list is for discussion among people who are designing or implementing
the Muldis Rosetta core API (including Muldis D language design), or who
are implementing Muldis Rosetta Engines, or who are writing core
documentation, tests, or examples.  It is not the place for
non-implementers to get help in using said.

=back

An official IRC channel for Muldis D and its implementations is also
intended, but not yet started.

Alternately, you can purchase more advanced commercial support for various
Muldis D implementations, particularly Muldis Rosetta, from its author by
way of Muldis Data Systems; see L<http://www.muldis.com/> for details.

=cut
