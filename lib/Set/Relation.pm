use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

###########################################################################
###########################################################################

{ package Set::Relation; # class
    use version 0.74; our $VERSION = qv('0.0.1');

    use Moose 0.64;

    has '_heading' => (
        is  => 'rw',
        isa => 'HashRef',
            # One elem per attribute:
                # hkey is Str attr name
                # hval is undef+unused
    );
    has '_degree' => (
        is     => 'rw',
        isa    => 'Int',
        reader => 'degree',
    );

    has '_body' => (
        is  => 'rw',
        isa => 'HashRef',
            # One elem per tuple:
                # hkey is Str identity generated from all tuple attrs
                # hval is HashRef that is the coll of separate tuple attrs:
                    # hkey is Str attr name
                    # hval is 2-elem ArrayRef that is the tuple attr value
                        # [0] is actual attr value
                        # [1] is Str identity generated from attr value
    );
    has '_cardinality' => (
        is     => 'rw',
        isa    => 'Int',
        reader => 'cardinality',
    );

    # If this is made true, no further mutation of S::R head/body allowed,
    # and then it can't be made false again.
    has '_has_frozen_identity' => (
        is       => 'rw',
        isa      => 'Bool',
        init_arg => 'has_frozen_identity',
        default  => 0,
        reader   => 'has_frozen_identity',
    );

    # This may only be made defined when _has_frozen_identity is true.
    has '_which' => (
        is  => 'rw',
        isa => 'Maybe[Str]',
    );

    has '_indexes' => (
        is      => 'rw',
        isa     => 'HashRef',
            # One elem per index:
                # hkey is index name;
                    # - is Str ident gen f head subset tha index ranges ovr
                # hval is 2-elem ArrayRef that is the index itself + meta
                    # [0] is HashRef of atnms that index ranges over
                        # - structure same as '_heading'
                    # [1] is index itself;
                        # - HashRef; one elem per tup of projection of body
                            # on attrs that index ranges over
                        # hkey is Str ident gen fr distinct projection tupl
                        # hval is set of body tup having projection tuples
                            # in comn; is HashRef; one elem per body tuple
                            # - structure same as '_body', is slice o _body
        default => sub { {} },
    );

    use List::Util 'first';

###########################################################################

sub BUILD {
    my ($self, $args) = @_;
    my ($members) = @{$args}{'members'};

    # Note, $members may be in all of the same formats as a HDMD_Perl5_Tiny
    # Relation value literal payload, but with a few extra trivial options.

    if (!defined $members) {
        # Extra option 1.
        $members = [];
    }
    elsif (ref $members eq 'HASH') {
        # Extra option 2;
        $members = [$members];
    }
    confess q{new(): Bad :$members arg; it must be either undefined}
            . q{ or an array-ref or a hash-ref or a Set::Relation object.}
        if ref $members ne 'ARRAY'
            and not (blessed $members and $members->isa( __PACKAGE__ ));

    # If we get here, $members is either a Set::Relation or an array-ref.

    my ($heading, $body);

    if (blessed $members and $members->isa( __PACKAGE__ )) {
        # We will just copy another Set::Relation object's member-set.
        $heading = {%{$members->_heading()}};
        $body = {%{$members->_body()}};
    }
    elsif (@{$members} == 0) {
        # Input specifies zero attrs + zero tuples.
        $heading = {};
        $body = {};
    }
    else {
        # Input specifies at least one attr or at least one tuple.
        my $member0 = $members->[0];
        if (!defined $member0) {
            confess q{new(): Bad :$members arg; it is an array ref}
                . q{ but it directly has an undefined element.};
        }
        elsif (!ref $member0) {
            # Input spec at least 1 attr + zero tuples.
            for my $atnm (@{$members}) {
                confess q{new(): Bad :$members arg; it has a non-ref elem,}
                        . q{ indicating it should just be a list of attr}
                        . q{ names, but at least one other elem is}
                        . q{ undefined or is a ref.}
                    if !defined $atnm or ref $atnm;
            }
            $heading = {CORE::map { ($_ => undef) } @{$members}};
            confess q{new(): Bad :$members arg; it specifies a list of}
                    . q{ attr names with at least one duplicated name.}
                if (keys %{$heading}) != @{$members};
            $body = {};
        }
        elsif (ref $member0 eq 'HASH') {
            # Input spec at least 1 tuple, in named attr format.
            $heading = {CORE::map { ($_ => undef) } keys %{$member0}};
            $body = {};
            for my $tuple (@{$members}) {
                confess q{new(): Bad :$members arg; it has a hash-ref}
                        . q{ elem, indicating it should just be a list of}
                        . q{ tuples in named-attr format, but at least one}
                        . q{ other elem is not a hash-ref, or the 2 elems}
                        . q{ don't have exactly the same set of hkeys.}
                    if ref $tuple ne 'HASH'
                        or !$self->_is_identical_hkeys( $heading, $tuple );
                $tuple = $self->_import_nfmt_tuple( $tuple );
                $body->{$self->_ident_str( $tuple )} = $tuple;
            }
        }
        elsif (ref $member0 eq 'ARRAY') {
            # Input is in ordered attr format.
            my $member1 = $members->[1];
            confess q{new(): Bad :$members arg; it has an array-ref first}
                    . q{ elem, indicating it should just be a list of}
                    . q{ tuples in ordered-attr format, but either}
                    . q{ :$members doesn't have exactly 2 elements or its}
                    . q{ second element isn't also an array-ref.}
                if @{$members} != 2 or ref $member1 ne 'ARRAY';
            for my $atnm (@{$member0}) {
                confess q{new(): Bad :$members array-ref arg array-ref}
                        . q{ first elem; it should be just be a list of}
                        . q{ attr names, but at least one name}
                        . q{ is undefined or is a ref.}
                    if !defined $atnm or ref $atnm;
            }
            $heading = {CORE::map { ($_ => undef) } @{$member0}};
            confess q{new(): Bad :$members arg; it specifies a list of}
                    . q{ attr names with at least one duplicated name.}
                if (keys %{$heading}) != @{$member0};
            $body = {};
            for my $tuple (@{$member1}) {
                confess q{new(): Bad :$members array-ref arg array-ref}
                        . q{ second elem; at least one elem isn't an}
                        . q{ array-ref, or that doesn't have the same}
                        . q{ count of elems as the :$members first elem.}
                    if ref $tuple ne 'ARRAY' or @{$tuple} != @{$member0};
                $tuple = $self->_import_ofmt_tuple( $member0, $tuple );
                $body->{$self->_ident_str( $tuple )} = $tuple;
            }
        }
        else {
            confess q{new(): Bad :$members arg; it is an array-ref but it}
                . q{ has an elem that is neither a defined scalar nor}
                . q{ an array-ref nor a hash-ref.};
        }
    }

    $self->_heading( $heading );
    $self->_degree( scalar keys %{$heading} );
    $self->_body( $body );
    $self->_cardinality( scalar keys %{$body} );

    return;
}

###########################################################################

sub clone {
    my ($self) = @_;
    return __PACKAGE__->new( members => $self );
}

###########################################################################

sub export_for_new {
    my ($self, $want_ord_attrs) = @_;
    return {
        'members' => $self->_members(
            'export_for_new', '$want_ord_attrs', $want_ord_attrs ),
    };
}

###########################################################################

sub freeze_identity {
    my ($self) = @_;
    $self->_has_frozen_identity( 1 );
    return;
}

###########################################################################

sub which {
    my ($self) = @_;
    my $ident_str = $self->_which();
    if (!defined $ident_str) {
        $self->_has_frozen_identity( 1 );
        my $hs = $self->_heading_ident_str( $self->_heading() );
        my $bs = CORE::join qq{,\n}, sort keys %{$self->_body()};
        my $vstr = "H=$hs;\nB={$bs}";
        $ident_str = 'Relation:' . (length $vstr) . ':{' . $vstr . '}';
        $self->_which( $ident_str );
    }
    return $ident_str;
}

###########################################################################

sub members {
    my ($self, $want_ord_attrs) = @_;
    return $self->_members(
        'members', '$want_ord_attrs', $want_ord_attrs );
}

sub _members {
    my ($self, $rtn_nm, $arg_nm, $want_ord_attrs) = @_;

    my $heading = $self->_heading();
    my $body = $self->_body();

    my $members;

    if ($want_ord_attrs) {
        my $ord_attr_names = $self->_normalize_true_want_ord_attrs_arg(
            $rtn_nm, $arg_nm, $want_ord_attrs );
        $members = [$ord_attr_names, [CORE::map {
                $self->_export_ofmt_tuple( $ord_attr_names, $_ )
            } values %{$body}]];
    }
    elsif ((keys %{$body}) == 0) {
        # We have zero tuples, just export attr names.
        $members = [keys %{$heading}];
    }
    else {
        # We have at least one tuple, export in named-attr format.
        $members = [CORE::map { $self->_export_nfmt_tuple( $_ ) }
            values %{$body}];
    }

    return $members;
}

sub heading {
    my ($self) = @_;
    return [sort keys %{$self->_heading()}];
}

sub body {
    my ($self, $want_ord_attrs) = @_;

    my $body = $self->_body();

    if ($want_ord_attrs) {
        my $ord_attr_names = $self->_normalize_true_want_ord_attrs_arg(
            'body', '$want_ord_attrs', $want_ord_attrs );
        $body = [CORE::map { $self->_export_ofmt_tuple(
            $ord_attr_names, $_ ) } values %{$body}];
    }
    else {
        $body = [CORE::map { $self->_export_nfmt_tuple( $_ ) }
            values %{$body}];
    }

    return $body;
}

sub _normalize_true_want_ord_attrs_arg {
    my ($self, $rtn_nm, $arg_nm, $want_ord_attrs) = @_;

    my $heading = $self->_heading();

    my $attr_names = [keys %{$heading}];
    confess qq{$rtn_nm(): Bad $arg_nm arg;}
            . q{ it must be either undefined|false or the scalar value '1'}
            . q{ or an array-ref of attr names whose degree and}
            . q{ elements match the heading of the invocant.}
        if not ($want_ord_attrs eq '1'
            or ref $want_ord_attrs eq 'ARRAY'
                and @{$want_ord_attrs} == @{$attr_names}
                and !grep { !exists $heading->{$_} } @{$want_ord_attrs});

    return
        $want_ord_attrs eq '1' ? [sort @{$attr_names}] : $want_ord_attrs;
}

###########################################################################

sub evacuate {
    my ($topic) = @_;
    confess q{evacuate(): Can't mutate invocant having a frozen identity.}
        if $topic->_has_frozen_identity();
    $topic->_body( {} );
    $topic->_cardinality( 0 );
    return $topic;
}

sub insert {
    my ($r, $t) = @_;
    confess q{insert(): Can't mutate invocant that has a frozen identity.}
        if $r->_has_frozen_identity();
    $t = $r->_normalize_tuples_arg( 'insert', '$t', $t );
    return $r->_insert( $t );
}

sub _insert {
    my ($r, $t) = @_;

    my $r_b = $r->_body();
    my $r_indexes = $r->_indexes();

    for my $tuple (@{$t}) {
        $tuple = $r->_import_nfmt_tuple( $tuple );
        my $tuple_ident_str = $r->_ident_str( $tuple );
        if (!exists $r_b->{$tuple_ident_str}) {
            $r_b->{$tuple_ident_str} = $tuple;

            for my $subheading_ident_str (keys %{$r_indexes}) {
                my ($subheading, $index)
                    = @{$r_indexes->{$subheading_ident_str}};
                my $subtuple_ident_str = $r->_ident_str(
                    {CORE::map { ($_ => $tuple->{$_}) }
                        keys %{$subheading}} );
                my $matched_b = $index->{$subtuple_ident_str} ||= {};
                $matched_b->{$tuple_ident_str} = $tuple;
            }

        }
    }
    $r->_cardinality( scalar keys %{$r_b} );

    return $r;
}

sub delete {
    my ($r, $t) = @_;
    confess q{delete(): Can't mutate invocant that has a frozen identity.}
        if $r->_has_frozen_identity();
    $t = $r->_normalize_tuples_arg( 'delete', '$t', $t );
    return $r->_delete( $t );
}

sub _delete {
    my ($r, $t) = @_;

    my $r_b = $r->_body();
    my $r_indexes = $r->_indexes();

    for my $tuple (@{$t}) {
        $tuple = $r->_import_nfmt_tuple( $tuple );
        my $tuple_ident_str = $r->_ident_str( $tuple );
        if (exists $r_b->{$tuple_ident_str}) {
            CORE::delete $r_b->{$tuple_ident_str};

            for my $subheading_ident_str (keys %{$r_indexes}) {
                my ($subheading, $index)
                    = @{$r_indexes->{$subheading_ident_str}};
                my $subtuple_ident_str = $r->_ident_str(
                    {CORE::map { ($_ => $tuple->{$_}) }
                        keys %{$subheading}} );
                my $matched_b = $index->{$subtuple_ident_str};
                CORE::delete $matched_b->{$tuple_ident_str};
                if ((scalar keys %{$matched_b}) == 0) {
                    CORE::delete $index->{$subtuple_ident_str};
                }
            }

        }
    }
    $r->_cardinality( scalar keys %{$r_b} );

    return $r;
}

sub _normalize_tuples_arg {
    my ($r, $rtn_nm, $arg_nm, $t) = @_;

    my $r_h = $r->_heading();
    my $r_b = $r->_body();

    if (ref $t eq 'HASH') {
        $t = [$t];
    }
    confess qq{$rtn_nm(): Bad $arg_nm arg; it must be}
            . q{ an array-ref or a hash-ref.}
        if ref $t ne 'ARRAY';
    for my $tuple (@{$t}) {
        confess qq{$rtn_nm(): Bad $arg_nm arg elem;}
                . q{ it isn't a hash-ref, or it doesn't have exactly the}
                . q{ same set of attr names as the invocant.}
            if ref $tuple ne 'HASH'
                or !$r->_is_identical_hkeys( $r_h, $tuple );
    }

    return $t;
}

###########################################################################

sub _is_identical_hkeys {
    my ($self, $h1, $h2) = @_;
    my $h1_hkeys = [keys %{$h1}];
    my $h2_hkeys = [keys %{$h2}];
    return (@{$h1_hkeys} == @{$h2_hkeys}
        and !first { !exists $h1->{$_} } @{$h2_hkeys});
}

###########################################################################

sub _heading_ident_str {
    my ($self, $heading) = @_;
    my $vstr = CORE::join q{,}, CORE::map {
            'Atnm:' . (length $_) . ':<' . $_ . '>'
        } sort keys %{$heading};
    return 'Heading:' . (length $vstr) . ':{' . $vstr . '}';
}

sub _ident_str {
    # Note, we assume that any hash-ref arg we get is specifically in
    # internal tuple format, meaning each hval is a 2-elem array etc,
    # and that this is recursive for hash-ref hvals of said.
    my ($self, $value) = @_;
    my $ident_str;
    if (!defined $value) {
        # The Perl undef is equal to itself, distinct from all def values.
        $ident_str = 'Undef';
    }
    elsif (!ref $value) {
        # Treat all defined non-ref values as their string representation.
        $ident_str = 'Scalar:' . (length $value) . ':<' . $value . '>';
    }
    elsif (!blessed $value) {
        # By default, every non-object reference is distinct, and its
        # identity is its memory address; the exception is if the reference
        # is a hash-ref, in which case it is treated as an internal tuple.
        if (ref $value eq 'HASH') {
            my $vstr = CORE::join q{,}, CORE::map {
                my $atnm = 'Atnm:' . (length $_) . ':<' . $_ . '>';
                my $atvl = $value->{$_}->[1];
                "N=$atnm;V=$atvl";
            } sort keys %{$value};
            $ident_str = 'Tuple:' . (length $vstr) . ':{' . $vstr . '}';
        }
        else {
            my $vstr = "$value";
            $ident_str = 'Ref:' . (length $vstr) . ':<' . $vstr . '>';
        }
    }
    else {
        # By default, every object instance is distinct, and its identity
        # is its memory address; the exception is if the object is a
        # Set::Relation or if it overloads stringification.
        if ($value->isa( __PACKAGE__ )) {
            $ident_str = $value->which(); # 'Relation:...'
        }
        else {
            my $vstr = "$value";
            $ident_str = 'Object[' . (blessed $value) . ']:'
                . (length $vstr) . ':<' . $vstr . '>';
        }
    }
    return $ident_str;
}

###########################################################################

sub _import_nfmt_tuple {
    my ($self, $tuple) = @_;
    return {CORE::map {
        my $atnm = $_;
        my $atvl = $tuple->{$_};
        if (ref $atvl eq 'HASH') {
            $atvl = $self->_import_nfmt_tuple( $atvl );
        }
        ($atnm => [$atvl, $self->_ident_str( $atvl )]);
    } keys %{$tuple}};
}

sub _export_nfmt_tuple {
    my ($self, $tuple) = @_;
    return {CORE::map {
        my $atnm = $_;
        my $atvl = $tuple->{$_}->[0];
        if (ref $atvl eq 'HASH') {
            $atvl = $self->_export_nfmt_tuple( $atvl );
        }
        ($atnm => $atvl);
    } keys %{$tuple}};
}

sub _import_ofmt_tuple {
    my ($self, $atnms, $atvls) = @_;
    return {CORE::map {
        my $atnm = $atnms->[$_];
        my $atvl = $atvls->[$_];
        if (ref $atvl eq 'HASH') {
            $atvl = $self->_import_nfmt_tuple( $atvl );
        }
        ($atnm => [$atvl, $self->_ident_str( $atvl )]);
    } 0..$#{$atnms}};
}

sub _export_ofmt_tuple {
    my ($self, $atnms, $tuple) = @_;
    return [CORE::map {
        my $atvl = $tuple->{$_}->[0];
        if (ref $atvl eq 'HASH') {
            $atvl = $self->_export_nfmt_tuple( $atvl );
        }
        $atvl;
    } @{$atnms}];
}

###########################################################################

sub is_nullary {
    my ($topic) = @_;
    return $topic->degree() == 0;
}

###########################################################################

sub is_empty {
    my ($topic) = @_;
    return $topic->cardinality() == 0;
}

sub is_member {
    my ($r, $t) = @_;
    $t = $r->_normalize_tuples_arg( 'is_member', '$t', $t );
    my $r_b = $r->_body();
    return !first {
            !exists $r_b->{$r->_ident_str( $r->_import_nfmt_tuple( $_ ) )}
        } @{$t};
}

###########################################################################

sub empty {
    my ($topic) = @_;
    return __PACKAGE__->new( members => $topic->heading() );
}

sub insertion {
    my ($r, $t) = @_;
    $t = $r->_normalize_tuples_arg( 'insertion', '$t', $t );
    return $r->clone()->_insert( $t );
}

sub deletion {
    my ($r, $t) = @_;
    $t = $r->_normalize_tuples_arg( 'deletion', '$t', $t );
    return $r->clone()->_delete( $t );
}

###########################################################################

sub rename {
    my ($topic, $map) = @_;

    confess q{rename(): Bad $map arg; it must be a hash-ref.}
        if ref $map ne 'HASH';
    for my $atnm (values %{$map}) {
        confess q{rename(): Bad $map arg;}
                . q{ its hash elem values should be just be a list of attr}
                . q{ names, but at least one name is undefined or isa ref.}
            if !defined $atnm or ref $atnm;
    }
    my $inv_map = {reverse %{$map}};
    confess q{rename(): Bad $map arg;}
            . q{ its hash elem values specify a list of}
            . q{ attr names with at least one duplicated name.}
        if (keys %{$inv_map}) != (keys %{$map});

    my ($topic_attrs_to_ren, $topic_attrs_no_ren, $map_hkeys_not_in_topic)
        = $topic->_ptn_conj_and_disj( $topic->_heading(), $map );
    confess q{rename(): Bad $map arg; that list of attrs to be renamed,}
            . q{ the hash keys, isn't a subset of the invocant's heading.}
        if @{$map_hkeys_not_in_topic} > 0;

    my ($map_hvals_same_as_topic_no_ren, undef, undef)
        = $topic->_ptn_conj_and_disj(
            {CORE::map { ($_ => undef) } @{$topic_attrs_no_ren}},
            $inv_map );
    confess q{rename(): Bad $map arg; at least one value of that hash,}
            . q{ a new name for an attr of the invocant to rename,}
            . q{ duplicates an attr of the invocant not being renamed.}
        if @{$map_hvals_same_as_topic_no_ren} > 0;

    return $topic->_rename( $map );
}

sub _rename {
    my ($topic, $map) = @_;

    # Remove any explicit no-ops of an attr being renamed to the same name.
    $map = CORE::map { ($_ => $map->{$_}) }
        grep { $map->{$_} ne $_ } keys %{$map};

    if ((scalar keys %{$map}) == 0) {
        # Rename of zero attrs of input yields the input.
        return $topic;
    }

    # Expand map to specify all topic attrs being renamed to something.
    $map = CORE::map { ($_ => (
            exists $map->{$_} ? $map->{$_} : $_
        )) } keys %{$topic->_heading()};

    my $result = __PACKAGE__->new();

    $result->_heading( {CORE::map { ($_ => undef) } values %{$map}} );
    $result->_degree( $topic->degree() );

    my $result_b = $result->_body();

    for my $topic_t (values %{$topic->_body()}) {
        my $result_t = {CORE::map {
                ($map->{$_} => $topic_t->{$_})
            } keys %{$topic_t}};
        my $result_t_ident_str = $topic->_ident_str( $result_t );
        $result_b->{$result_t_ident_str} = $result_t;
    }
    $result->_cardinality( $topic->cardinality() );

    return $result;
}

###########################################################################

sub projection {
    my ($topic, $attrs) = @_;

    my $proj_h = $topic->_attrs_hr_from_assert_valid_attrs_arg(
        'projection', '$attrs', $attrs );
    my (undef, undef, $proj_only)
        = $topic->_ptn_conj_and_disj( $topic->_heading(), $proj_h );
    confess q{projection(): Bad $attrs arg; that attr list}
            . q{ isn't a subset of the invocant's heading.}
        if @{$proj_only} > 0;

    return $topic->_projection( $attrs );
}

sub _projection {
    my ($topic, $attrs) = @_;

    if (@{$attrs} == 0) {
        # Projection of zero attrs yields identity relation zero or one.
        if ($topic->is_empty()) {
            return __PACKAGE__->new();
        }
        else {
            return __PACKAGE__->new( members => [ {} ] );
        }
    }
    if (@{$attrs} == $topic->degree()) {
        # Projection of all attrs of input yields the input.
        return $topic;
    }

    my $result = __PACKAGE__->new();

    $result->_heading( {CORE::map { ($_ => undef) } @{$attrs}} );
    $result->_degree( scalar @{$attrs} );

    my $result_b = $result->_body();

    for my $topic_t (values %{$topic->_body()}) {
        my $result_t = {CORE::map { ($_ => $topic_t->{$_}) } @{$attrs}};
        my $result_t_ident_str = $topic->_ident_str( $result_t );
        if (!exists $result_b->{$result_t_ident_str}) {
            $result_b->{$result_t_ident_str} = $result_t;
        }
    }
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

sub cmpl_projection {
    my ($topic, $attrs) = @_;

    my $topic_h = $topic->_heading();

    my $cproj_h = $topic->_attrs_hr_from_assert_valid_attrs_arg(
        'cmpl_projection', '$attrs', $attrs );
    my (undef, undef, $cproj_only)
        = $topic->_ptn_conj_and_disj( $topic_h, $cproj_h );
    confess q{cmpl_projection(): Bad $attrs arg; that attr list}
            . q{ isn't a subset of the invocant's heading.}
        if @{$cproj_only} > 0;

    return $topic->_projection(
        [grep { !$cproj_h->{$_} } keys %{$topic_h}] );
}

###########################################################################

sub restriction {
    my ($topic, $func, $assuming) = @_;

    $topic->_assert_valid_func_arg( 'restriction', '$func', $func );

    my $result = $topic->empty();

    my $topic_b = $topic->_body();
    my $result_b = $result->_body();

    for my $tuple_ident_str (keys %{$topic_b}) {
        my $tuple = $topic_b->{$tuple_ident_str};
        if ($func->( $topic->_export_nfmt_tuple( $tuple ), $assuming )) {
            $result_b->{$tuple_ident_str} = $tuple;
        }
    }
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

sub cmpl_restriction {
    my ($topic, $func, $assuming) = @_;

    $topic->_assert_valid_func_arg( 'cmpl_restriction', '$func', $func );

    my $result = $topic->empty();

    my $topic_b = $topic->_body();
    my $result_b = $result->_body();

    for my $tuple_ident_str (keys %{$topic_b}) {
        my $tuple = $topic_b->{$tuple_ident_str};
        if (!$func->( $topic->_export_nfmt_tuple( $tuple ), $assuming )) {
            $result_b->{$tuple_ident_str} = $tuple;
        }
    }
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

###########################################################################

sub extension {
    my ($topic, $attrs, $func, $assuming) = @_;

    my $exten_h = $topic->_attrs_hr_from_assert_valid_attrs_arg(
        'extension', '$attrs', $attrs );
    $topic->_assert_valid_func_arg( 'extension', '$func', $func );

    if (@{$attrs} == 0) {
        # Extension of input by zero attrs yields the input.
        return $topic;
    }

    my $topic_h = $topic->_heading();

    my ($both, undef, undef)
        = $topic->_ptn_conj_and_disj( $topic_h, $exten_h );
    confess q{extension(): Bad $attrs arg; that attr list}
            . q{ isn't disjoint with the invocant's heading.}
        if @{$both} > 0;

    my $result = __PACKAGE__->new();

    $result->_heading( {%{$topic_h}, %{$exten_h}} );
    $result->_degree( $topic->degree() + scalar @{$attrs} );

    my $result_b = $result->_body();

    for my $topic_t (values %{$topic->_body()}) {
        my $exten_t
            = $func->( $topic->_export_nfmt_tuple( $topic_t ), $assuming );
        $topic->_assert_valid_tuple_result_of_func_arg(
            'extension', '$func', '$attrs', $exten_t, $exten_h );
        $exten_t = $topic->_import_nfmt_tuple( $exten_t );
        my $result_t = {%{$topic_t}, %{$exten_t}};
        my $result_t_ident_str = $topic->_ident_str( $result_t );
        $result_b->{$result_t_ident_str} = $result_t;
    }
    $result->_cardinality( $topic->cardinality() );

    return $result;
}

sub static_extension {
    my ($topic, $attrs) = @_;

    confess q{static_extension(): Bad $attrs arg; it isn't a hash-ref.}
        if ref $attrs ne 'HASH';

    if ((scalar keys %{$attrs}) == 0) {
        # Extension of input by zero attrs yields the input.
        return $topic;
    }

    my $topic_h = $topic->_heading();

    my ($both, undef, undef)
        = $topic->_ptn_conj_and_disj( $topic_h, $attrs );
    confess q{static_extension(): Bad $attrs arg; that attr list}
            . q{ isn't disjoint with the invocant's heading.}
        if @{$both} > 0;

    $attrs = $topic->_import_nfmt_tuple( $attrs );

    my $result = __PACKAGE__->new();

    $result->_heading(
        {%{$topic_h}, CORE::map { ($_ => undef) } keys %{$attrs}} );
    $result->_degree( $topic->degree() + (scalar keys %{$attrs}) );

    my $result_b = $result->_body();

    for my $topic_t (values %{$topic->_body()}) {
        my $result_t = {%{$topic_t}, %{$attrs}};
        my $result_t_ident_str = $topic->_ident_str( $result_t );
        $result_b->{$result_t_ident_str} = $result_t;
    }
    $result->_cardinality( $topic->cardinality() );

    return $result;
}

###########################################################################

sub map {
    my ($topic, $result_attrs, $func, $assuming) = @_;

    my $result_h = $topic->_attrs_hr_from_assert_valid_attrs_arg(
        'map', '$result_attrs', $result_attrs );
    $topic->_assert_valid_func_arg( 'map', '$func', $func );

    if (@{$result_attrs} == 0) {
        # Map to zero attrs yields identity relation zero or one.
        if ($topic->is_empty()) {
            return __PACKAGE__->new();
        }
        else {
            return __PACKAGE__->new( members => [ {} ] );
        }
    }

    my $result = __PACKAGE__->new();

    $result->_heading( $result_h );
    $result->_degree( scalar @{$result_attrs} );

    my $result_b = $result->_body();

    for my $topic_t (values %{$topic->_body()}) {
        my $result_t
            = $func->( $topic->_export_nfmt_tuple( $topic_t ), $assuming );
        $topic->_assert_valid_tuple_result_of_func_arg(
            'map', '$func', '$result_attrs', $result_t, $result_h );
        $result_t = $topic->_import_nfmt_tuple( $result_t );
        my $result_t_ident_str = $topic->_ident_str( $result_t );
        if (!exists $result_b->{$result_t_ident_str}) {
            $result_b->{$result_t_ident_str} = $result_t;
        }
    }
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

###########################################################################

sub _attrs_hr_from_assert_valid_attrs_arg {
    my ($self, $rtn_nm, $arg_nm, $attrs) = @_;

    confess qq{$rtn_nm(): Bad $arg_nm arg;}
            . q{ it must be an array-ref.}
        if ref $attrs ne 'ARRAY';
    for my $atnm (@{$attrs}) {
        confess qq{$rtn_nm(): Bad $arg_nm arg;}
                . q{ it should be just be a list of attr names,}
                . q{ but at least one name is undefined or is a ref.}
            if !defined $atnm or ref $atnm;
    }
    my $heading = {CORE::map { ($_ => undef) } @{$attrs}};
    confess qq{$rtn_nm(): Bad $arg_nm arg;}
            . q{ it specifies a list of}
            . q{ attr names with at least one duplicated name.}
        if (keys %{$heading}) != @{$attrs};

    return $heading;
}

sub _assert_valid_func_arg {
    my ($self, $rtn_nm, $arg_nm, $func) = @_;
    confess qq{$rtn_nm(): Bad $arg_nm arg;}
            . q{ it must be a Perl subroutine reference.}
        if ref $func ne 'CODE';
}

sub _assert_valid_tuple_result_of_func_arg {
    my ($self, $rtn_nm, $arg_nm_func, $arg_nm_attrs, $result_t, $heading)
        = @_;
    confess qq{$rtn_nm(): Bad $arg_nm_func arg;}
            . q{ at least one result of executing that Perl subroutine}
            . q{ reference was not a hash-ref or it didn't have the same}
            . qq{ set of hkeys as specified by the $arg_nm_attrs arg.}
        if ref $result_t ne 'HASH'
            or !$self->_is_identical_hkeys( $heading, $result_t );
}

###########################################################################

sub is_identical {
    my ($topic, $other) = @_;
    confess q{is_identical(): Bad $other arg;}
            . q{ it must be a Set::Relation object.}
        if !blessed $other or !$other->isa( __PACKAGE__ );
    return ($topic->_is_identical_hkeys(
            $topic->_heading(), $other->_heading() )
        and $topic->_is_identical_hkeys(
            $topic->_body(), $other->_body() ));
}

###########################################################################

sub is_subset {
    my ($look_in, $look_for) = @_;
    $look_in->_assert_same_heading_relation_arg(
        'is_subset', '$look_for', $look_for );
    my $look_in_b = $look_in->_body();
    return !first { !exists $look_in_b->{$_} } keys %{$look_for->_body()};
}

sub _assert_same_heading_relation_arg {
    my ($self, $rtn_nm, $arg_nm, $other) = @_;
    confess qq{$rtn_nm(): Bad $arg_nm arg; it isn't a Set::Relation}
            . q{ object, or it doesn't have exactly the}
            . q{ same set of attr names as the invocant.}
        if !blessed $other or !$other->isa( __PACKAGE__ )
            or !$self->_is_identical_hkeys(
                $self->_heading(), $other->_heading() );
    return;
}

sub is_proper_subset {
    my ($look_in, $look_for) = @_;
    $look_in->_assert_same_heading_relation_arg(
        'is_proper_subset', '$look_for', $look_for );
    my $look_in_b = $look_in->_body();
    return ($look_for->cardinality() < $look_in->cardinality()
        and !first { !exists $look_in_b->{$_} }
            keys %{$look_for->_body()});
}

sub is_disjoint {
    my ($topic, $other) = @_;
    $topic->_assert_same_heading_relation_arg(
        'is_disjoint', '$other', $other );
    return $topic->_intersection( $other )->is_empty();
}

###########################################################################

sub union {
    my ($topic, $other) = @_;

    $topic->_assert_same_heading_relation_arg( 'union', '$other', $other );

    my ($sm, $lg) = ($topic->cardinality() < $other->cardinality())
        ? ($topic, $other) : ($other, $topic);

    if ($sm->is_empty()) {
        return $lg;
    }

    my $result = $lg->clone();

    my $sm_b = $sm->_body();
    my $result_b = $result->_body();

    for my $tuple_ident_str (keys %{$sm_b}) {
        if (!exists $result_b->{$tuple_ident_str}) {
            $result_b->{$tuple_ident_str} = $sm_b->{$tuple_ident_str};
        }
    }
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

###########################################################################

sub exclusion {
    # Also known as symmetric_difference().
    my ($topic, $other) = @_;

    $topic->_assert_same_heading_relation_arg(
        'exclusion', '$other', $other );

    my ($sm, $lg) = ($topic->cardinality() < $other->cardinality())
        ? ($topic, $other) : ($other, $topic);

    if ($sm->is_empty()) {
        return $lg;
    }

    my $result = $lg->clone();

    my $sm_b = $sm->_body();
    my $lg_b = $lg->_body();
    my $result_b = $result->_body();

    for my $tuple_ident_str (keys %{$sm_b}) {
        if (exists $lg_b->{$tuple_ident_str}) {
            CORE::delete $result_b->{$tuple_ident_str};
        }
        else {
            $result_b->{$tuple_ident_str} = $sm_b->{$tuple_ident_str};
        }
    }
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

###########################################################################

sub intersection {
    my ($topic, $other) = @_;
    $topic->_assert_same_heading_relation_arg(
        'intersection', '$other', $other );
    return $topic->_intersection( $other );
}

sub _intersection {
    my ($topic, $other) = @_;

    my ($sm, $lg) = ($topic->cardinality() < $other->cardinality())
        ? ($topic, $other) : ($other, $topic);

    if ($sm->is_empty()) {
        return $sm;
    }

    my $result = $lg->empty();

    my $sm_b = $sm->_body();
    my $lg_b = $lg->_body();
    my $result_b = $result->_body();

    for my $tuple_ident_str (keys %{$sm_b}) {
        if (exists $lg_b->{$tuple_ident_str}) {
            $result_b->{$tuple_ident_str} = $sm_b->{$tuple_ident_str};
        }
    }
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

###########################################################################

sub difference {
    my ($source, $filter) = @_;
    $source->_assert_same_heading_relation_arg(
        'difference', '$other', $filter );
    return $source->_difference( $filter );
}

sub _difference {
    my ($source, $filter) = @_;
    if ($source->is_empty() or $filter->is_empty()) {
        return $source;
    }
    return $source->_regular_difference( $filter );
}

sub _regular_difference {
    my ($source, $filter) = @_;

    my $result = $source->empty();

    my $source_b = $source->_body();
    my $filter_b = $filter->_body();
    my $result_b = $result->_body();

    for my $tuple_ident_str (keys %{$source_b}) {
        if (!exists $filter_b->{$tuple_ident_str}) {
            $result_b->{$tuple_ident_str} = $source_b->{$tuple_ident_str};
        }
    }
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

###########################################################################

sub semidifference {
    my ($source, $filter) = @_;
    confess q{semidifference(): Bad $filter arg;}
            . q{ it isn't a Set::Relation object.}
        if !blessed $filter or !$filter->isa( __PACKAGE__ );
    if ($source->is_empty() or $filter->is_empty()) {
        return $source;
    }
    return $source->_regular_difference( $source->_semijoin( $filter ) );
}

sub semijoin {
    my ($source, $filter) = @_;
    confess q{semijoin(): Bad $filter arg;}
            . q{ it isn't a Set::Relation object.}
        if !blessed $filter or !$filter->isa( __PACKAGE__ );
    return $source->_semijoin( $filter );
}

sub _semijoin {
    my ($source, $filter) = @_;

    if ($source->is_empty()) {
        return $source;
    }
    if ($filter->is_empty()) {
        return $source->empty();
    }

    # If we get here, both inputs have at least one tuple.

    if ($source->is_nullary() or $filter->is_nullary()) {
        return $source;
    }

    # If we get here, both inputs also have at least one attribute.

    my ($both, $source_only, $filter_only) = $source->_ptn_conj_and_disj(
        $source->_heading(), $filter->_heading() );

    if (@{$both} == 0) {
        # The inputs have disjoint headings; result is source.
        return $source;
    }
    if (@{$source_only} == 0 and @{$filter_only} == 0) {
        # The inputs have identical headings; result is intersection.
        return $source->_intersection( $filter );
    }

    # If we get here, the inputs also have overlapping non-ident headings.

    return $source->_regular_semijoin( $filter, $both );
}

sub _regular_semijoin {
    my ($source, $filter, $both) = @_;

    my $result = $source->empty();

    my ($sm, $lg) = ($source->cardinality() < $filter->cardinality())
        ? ($source, $filter) : ($filter, $source);

    my $sm_index = $sm->_want_index( $both );
    my $lg_index = $lg->_want_index( $both );
    my $source_index = $source->_want_index( $both );
    my $result_b = $result->_body();

    for my $subtuple_ident_str (keys %{$sm_index}) {
        if (exists $lg_index->{$subtuple_ident_str}) {
            my $matched_source_b = $source_index->{$subtuple_ident_str};
            for my $tuple_ident_str (keys %{$matched_source_b}) {
                $result_b->{$tuple_ident_str}
                    = $matched_source_b->{$tuple_ident_str};
            }
        }
    }
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

###########################################################################

sub join {
    my ($topic, $other) = @_;
    confess q{join(): Bad $other arg; it isn't a Set::Relation object.}
        if !blessed $other or !$other->isa( __PACKAGE__ );
    return $topic->_join( $other );
}

sub _join {
    my ($topic, $other) = @_;

    my ($both, $topic_only, $other_only) = $topic->_ptn_conj_and_disj(
        $topic->_heading(), $other->_heading() );

    if ($topic->is_empty() or $other->is_empty()) {
        # At least one input has zero tuples; so does result.
        return __PACKAGE__->new(
            members => [@{$both}, @{$topic_only}, @{$other_only}] );
    }

    # If we get here, both inputs have at least one tuple.

    if ($topic->is_nullary()) {
        # First input is identity-one tuple; result is second input.
        return $other;
    }
    if ($other->is_nullary()) {
        # Second input is identity-one tuple; result is first input.
        return $topic;
    }

    # If we get here, both inputs also have at least one attribute.

    if (@{$both} == 0) {
        # The inputs have disjoint headings; result is cross-product.
        return $topic->_regular_product( $other );
    }
    if (@{$topic_only} == 0 and @{$other_only} == 0) {
        # The inputs have identical headings; result is intersection.
        return $topic->_intersection( $other );
    }

    # If we get here, the inputs also have overlapping non-ident headings.

    if (@{$topic_only} == 0) {
        # The first input's attributes are a proper subset of the second's;
        # result has same heading as second, a subset of second's tuples.
        return $other->_regular_semijoin( $topic, $both );
    }
    if (@{$other_only} == 0) {
        # The second input's attributes are a proper subset of the first's;
        # result has same heading as first, a subset of first's tuples.
        return $topic->_regular_semijoin( $other, $both );
    }

    # If we get here, both inputs also have at least one attr of their own.

    return $topic->_regular_join(
        $other, $both, $topic_only, $other_only );
}

sub _regular_join {
    my ($topic, $other, $both, $topic_only, $other_only) = @_;

    my $result = __PACKAGE__->new();

    $result->_heading( {CORE::map { ($_ => undef) }
        @{$both}, @{$topic_only}, @{$other_only}} );
    $result->_degree( @{$both} + @{$topic_only} + @{$other_only} );

    my ($sm, $lg) = ($topic->cardinality() < $other->cardinality())
        ? ($topic, $other) : ($other, $topic);

    my $sm_index = $sm->_want_index( $both );
    my $lg_index = $lg->_want_index( $both );
    my $result_b = {};

    for my $subtuple_ident_str (keys %{$sm_index}) {
        if (exists $lg_index->{$subtuple_ident_str}) {
            my $matched_sm_b = $sm_index->{$subtuple_ident_str};
            my $matched_lg_b = $lg_index->{$subtuple_ident_str};
            for my $t1 (values %{$matched_sm_b}) {
                for my $t2 (values %{$matched_lg_b}) {
                    my $result_t = {%{$t1}, %{$t2}};
                    $result_b->{$topic->_ident_str( $result_t )}
                        = $result_t;
                }
            }
        }
    }
    $result->_body( $result_b );
    $result->_cardinality( scalar keys %{$result_b} );

    return $result;
}

###########################################################################

sub product {
    my ($topic, $other) = @_;

    confess q{product(): Bad $other arg; it isn't a Set::Relation object.}
        if !blessed $other or !$other->isa( __PACKAGE__ );

    my ($both, $topic_only, $other_only) = $topic->_ptn_conj_and_disj(
        $topic->_heading(), $other->_heading() );

    confess q{product(): Bad $other arg;}
            . q{ its heading isn't disjoint with the invocant.}
        if @{$both} > 0;

    if ($topic->is_empty() or $other->is_empty()) {
        # At least one input has zero tuples; so does result.
        return __PACKAGE__->new(
            members => [@{$topic_only}, @{$other_only}] );
    }

    # If we get here, both inputs have at least one tuple.

    if ($topic->is_nullary()) {
        # First input is identity-one tuple; result is second input.
        return $other;
    }
    if ($other->is_nullary()) {
        # Second input is identity-one tuple; result is first input.
        return $topic;
    }

    # If we get here, both inputs also have at least one attribute.

    return $topic->_regular_product( $other );
}

sub _regular_product {
    my ($topic, $other) = @_;

    my $result = __PACKAGE__->new();

    $result->_heading( {%{$topic->_heading()}, %{$other->_heading()}} );
    $result->_degree( $topic->degree() + $other->degree() );

    my ($sm, $lg) = ($topic->cardinality() < $other->cardinality())
        ? ($topic, $other) : ($other, $topic);

    my $sm_b = $sm->_body();
    my $lg_b = $lg->_body();
    my $result_b = {};

    for my $t1 (values %{$sm_b}) {
        for my $t2 (values %{$lg_b}) {
            my $result_t = {%{$t1}, %{$t2}};
            $result_b->{$topic->_ident_str( $result_t )} = $result_t;
        }
    }
    $result->_body( $result_b );
    $result->_cardinality( $topic->cardinality() * $other->cardinality() );

    return $result;
}

###########################################################################

sub quotient {
    my ($dividend, $divisor) = @_;

    confess q{quotient(): Bad $divisor arg;}
            . q{ it isn't a Set::Relation object.}
        if !blessed $divisor or !$divisor->isa( __PACKAGE__ );

    my (undef, $dividend_only, $divisor_only)
        = $dividend->_ptn_conj_and_disj(
            $dividend->_heading(), $divisor->_heading() );

    confess q{quotient(): Bad $divisor arg;}
            . q{ its heading isn't a subset of the invocant's heading.}
        if @{$divisor_only} > 0;

    my $proj_of_dividend_only = $dividend->_projection( $dividend_only );

    if ($dividend->is_empty() or $divisor->is_empty()) {
        # At least one input has zero tup; res has all tup o dividend proj.
        return $proj_of_dividend_only;
    }

    # If we get here, both inputs have at least one tuple.

    if ($dividend->is_nullary() or $divisor->is_nullary()) {
        # Both inputs or just divisor is ident-one tup; result is dividend.
        return $dividend;
    }

    # If we get here, divisor has at least one attribute,
    # and divisor heading is proper subset of dividend heading.

    return $proj_of_dividend_only
        ->_difference( $proj_of_dividend_only
            ->_product( $divisor )
            ->_difference( $dividend )
            ->_projection( $dividend_only )
        );
}

###########################################################################

sub composition {
    my ($topic, $other) = @_;

    confess q{composition(): Bad $other arg;}
            . q{ it isn't a Set::Relation object.}
        if !blessed $other or !$other->isa( __PACKAGE__ );

    my ($both, $topic_only, $other_only) = $topic->_ptn_conj_and_disj(
        $topic->_heading(), $other->_heading() );

    if ($topic->is_empty() or $other->is_empty()) {
        # At least one input has zero tuples; so does result.
        return __PACKAGE__->new(
            members => [@{$topic_only}, @{$other_only}] );
    }

    # If we get here, both inputs have at least one tuple.

    if ($topic->is_nullary()) {
        # First input is identity-one tuple; result is second input.
        return $other;
    }
    if ($other->is_nullary()) {
        # Second input is identity-one tuple; result is first input.
        return $topic;
    }

    # If we get here, both inputs also have at least one attribute.

    if (@{$both} == 0) {
        # The inputs have disjoint headings; result is cross-product.
        return $topic->_regular_product( $other );
    }
    if (@{$topic_only} == 0 and @{$other_only} == 0) {
        # The inputs have identical headings; result is ident-one relation.
        return __PACKAGE__->new( members => [ {} ] );
    }

    # If we get here, the inputs also have overlapping non-ident headings.

    if (@{$topic_only} == 0) {
        # The first input's attributes are a proper subset of the second's;
        # result has same heading as second, a subset of second's tuples.
        return $other->_regular_semijoin( $topic, $both )
            ->_projection( $other_only );
    }
    if (@{$other_only} == 0) {
        # The second input's attributes are a proper subset of the first's;
        # result has same heading as first, a subset of first's tuples.
        return $topic->_regular_semijoin( $other, $both )
            ->_projection( $topic_only );
    }

    # If we get here, both inputs also have at least one attr of their own.

    return $topic->_regular_join(
        $other, $both, $topic_only, $other_only )
            ->_projection( [@{$topic_only}, @{$other_only}] );
}

###########################################################################

sub _ptn_conj_and_disj {
    # inputs are hashes, results are arrays
    my ($self, $src1, $src2) = @_;
    my $both = [grep { exists $src1->{$_} } keys %{$src2}];
    my $both_h = {CORE::map { ($_ => undef) } @{$both}};
    my $only1 = [grep { !exists $both_h->{$_} } keys %{$src1}];
    my $only2 = [grep { !exists $both_h->{$_} } keys %{$src2}];
    return ($both, $only1, $only2);
}

sub _want_index {
    my ($self, $atnms) = @_;
    my $subheading = {CORE::map { ($_ => undef) } @{$atnms}};
    my $subheading_ident_str = $self->_heading_ident_str( $subheading );
    my $indexes = $self->_indexes();
    if (!exists $indexes->{$subheading_ident_str}) {
        my $index_and_meta = $indexes->{$subheading_ident_str}
            = [ $subheading, {} ];
        my $index = $index_and_meta->[1];
        my $body = $self->_body();
        for my $tuple_ident_str (keys %{$body}) {
            my $tuple = $body->{$tuple_ident_str};
            my $subtuple_ident_str = $self->_ident_str(
                {CORE::map { ($_ => $tuple->{$_}) } @{$atnms}} );
            my $matched_b = $index->{$subtuple_ident_str} ||= {};
            $matched_b->{$tuple_ident_str} = $tuple;
        }
    }
    return $indexes->{$subheading_ident_str}->[1];
}

###########################################################################

} # class Set::Relation

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

This document describes Set::Relation version 0.0.1 for Perl 5.

=head1 SYNOPSIS

    use Set::Relation;

    my $r1 = Set::Relation->new( members => [ [ 'x', 'y' ], [
        [ 4, 7 ],
        [ 3, 2 ],
    ] ] );

    my $r2 = Set::Relation->new( members => [
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
third-party Set:: modules available for Perl.  This is a standalone Perl 5
object class that represents a L<Muldis D|Muldis::D> quasi-relation value,
and its methods implement all the Muldis D relational operators.

B<WARNING:  This module is still experimental and may change in
incompatible ways between releases.  It is also under construction and
about a third of the planned short term features are yet missing.  However,
those features that are present have complete code and documentation.
While the existing features should be fully useable now and a review of
their code makes them look correct, most features have in fact not yet been
tested in running code and so might actually be broken.  This module might
in fact work for you now, but it is officially pre-alpha quality.  Please
treat it mainly as a developer preview, to experiment possible future use.>

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
'difference' etc, but it also provides operators like 'join', 'quotient',
'projection', 'group', 'summary', 'semijoin', 'restriction',
'semidifference', 'transitive_closure', 'outer_join' etc.

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

=head2 Matters of Value Identity

The relational model of data fundamentally involves values as being
immutable and distinct values having distinct identities, and the type of a
value is part of its identity.

The relational model has 3 kinds of types and values, called scalar
types/values, tuple types/values, and relation types/values; the 3 are all
mutually disjoint and no value in one kind can be identical to a value in
another.  This Set::Relation module treats its own objects as relation
values, it treats ordinary un-blessed Perl hash-refs as tuple values, and
it treats all other Perl values (and Perl undef) as scalar values.

Or strictly speaking, a Set::Relation object doesn't just represent a
relation as its allowed structure is more liberal than a true relation;
rather, a Set::Relation object directly represents the concept of a
"quasi-relation" value as defined by the L<Muldis D|Muldis::D> language,
which is more natural for integration with Perl's liberal way of doing
things and is also simpler to implement.  But if your use of the module is
appropriately more restricted than what the module itself allows, then you
can be working with just true relation values.  For simplicity, the rest of
this documentation will refer to the concept of "quasi-relation" less
formally as just "relation".

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

Loosely speaking, Set::Relation treats its own objects and Perl Hash-refs
and non-reference or undefined Perl values as being of value types, that
compare on their actual values, while treating all other Perl reference or
blessed values as being reference types, that compare on their memory
addresses.  Value types can't be mutated while the containers that the
reference values point to can be mutated.  If you want some other class'
object treated as a value type, make it overload stringification I<(or an
alternate/additional convention can be devised like Set::Relation's own
'which' convention)>.  If you want a Hash-ref/tuple or Set::Relation object
to be treated as a reference type, then pass it around using another layer
of reference indirection, such as by adding a scalar-ref up front.

I<Note: Were this Perl 6, we would basically just use the standard C<WHICH>
method or C<===> comparison operator to determine identity; but Perl 5
doesn't have that so we do the aforementioned instead.>

Now, while a relation value is conceptually immutable, the Set::Relation
class allows you to mutate a Set::Relation object under some circumstances
as a convenience to users, in a similar manner to how you can mutate a Perl
Hash or Array by inserting or deleting its elements.  By default, a newly
created Set::Relation object is mutable, that is its identity is said to
not be frozen; but when you perform certain operations with one, it will
become immutable, gaining a frozen identity, and this change can not be
reversed, though you can clone said object to get an un-frozen duplicate.

There are 3 main ways to make a Set::Relation object immutable.  The first
is explicitly, by invoking its C<freeze_identity> method.  The second is
implicitly, by invoking its C<which> method I<(this one may be reconsidered
on users' request)>.  The third is if another Set::Relation object is
constructed that is given the first object as a tuple attribute value; this
was done rather than cloning the input object under the assumption that
most of the time you wouldn't want to mutate the input object afterwards,
for efficiency.

=head2 Matters of Performance

Set::Relation by itself is strictly an in-memory data structure, same as
Perl's built-in arrays and hashes.  Its design focuses on providing
correct-behaving features in a relatively simple manner.

Performance is made as good as possible, using multiple design techniques,
while not becoming too complicated.  Set::Relation keeps a variety of
indexes automatically and makes a trade-off of being willing to use more
RAM (by storing multiple copies of data in hashed form, at least 3 copies
total) in order to get better CPU performance.

Loosely speaking, each Set::Relation object is a Perl hash-ref with one
element per tuple, where the hash value is the tuple itself as a Perl
hash-ref and the key is a unique hash / serialization of the entire deep
value of said tuple.

This basic structure means that fundamental operations of taking a whole
arbitrary tuple and querying whether or not it is in the relation is an
O(1) / constant-time operation, same as testing the existence of a key in a
Perl hash; likewise, inserting a tuple into or deleting a tuple from a
relation is also an O(1) / constant-time operation.

All basic set operations, like relational union or difference, are all O(N)
/ linear-time due to that basic structure alone.  When comparing 2 input
relations for a set operation, only the smaller one needs to be fully (at
the worst) or partially scanned, and the other does not; the scan produces
a list of tuples to search for, and each search for a tuple in the second
relation is O(1).  Similarly, many basic relational operations like
projection and extension are 0(N).  No such operations are in
polynomial-time such as O(N^2); that would simply be unacceptable.

Set::Relation also automatically generates more indexes to help with the
general cases of relational joins or semijoins where the arguments have
some but not all attributes in common (the common ones only providing the
join criteria).  Without the extra indexes, a generic join would be in
polynomial time since it would have to pair up every tuple of one argument
with every one of another to see if parts of each tuple match.  However,
this is changed to linear time by first creating (or reusing) an index on
each argument that is a hash of just the portion of the tuple attributes
that overlap with the other argument.  Creating each index is also linear
time.  So then using those indexes, doing an ordinary join or semijoin then
has the same performance characteristics as relational union or difference.

Now to be more accurate concerning relational join operations, finding out
what set of tuples in each input match each other is always a linear time
operation like relational intersection (what is actually happening on the
indexes), but producing the result set of tuples is an O(N*M) operation.
Now if the attributes overlapped between both inputs are superkeys of each
input, then producing the result set reduces to linear / O(N) time;
otherwise it is appropriately slower since the then multiple tuples on
each side of a match are then cartesian joined.  If the main operation is a
semijoin, that is always O(N) since we are actually just filtering one
input by the other, not joining them for a result.

Of course, a regular cartesian product, a join between 2 relations having
no attributes in common, can't be helped by an index (and generates none),
and so does have O(N*M) performance all the time.  This can't be helped
since we know that the result will always have a cardinality that is the
multiplication of input relations' cardinalities.

For the various few more complicated operators provided by Set::Relation,
which are conceptually defined in terms of simpler operators, their
performance is generally based on what they are defined in terms of.

To keep things simple, creation of indexes (besides the single fundemental
one) is strictly automatic and you can not explicitly add or remove an
index on a Set::Relation object.  Creation is just done the first time the
indexes would be I<used>, so they only happen say if you do a regular join
or such operation.  Once an index is created, it is automatically kept up
to date by any Set::Relation mutator methods; the design of said indexes
also makes it such that keeping them up to date during tuple inserts or
deletes is also O(1) per index.

To keep things simple, when new Set::Relation objects are generated from
relational operations, that new object starts out with no indexes (other
than the fundamental), even if conceivably the parent's could be copied.

The various Set::Relation operators know about and look for certain special
cases of inputs which allow them to short-circuit the operation.  In some
cases they may return certain constant values, or they may just return one
of their input objects directly.  They may also use a cheaper operation
than you requested which for example doesn't involve creating or using
indexes.  For example, if you use C<join> on 2 input relations that have
all the same attributes, it will short circuit to C<intersection>.  Or for
example if you do C<union> and one input relation has zero tuples, it will
simply return the other input object.

Now in the general relational model where relations are immutable, that
makes no semantical difference, but it is important to know if you plan to
mutate the result object of a relational operation, as you might then be
mutating an argument too.  So take appropriate precautions and do
appropriate tests where necessary so that you don't have undesired
side-effects in your program.

=head1 INTERFACE

The interface of Set::Relation is entirely object-oriented; you use it by
creating Set::Relation objects by invoking the C<new()> submethod on the
Set::Relation class name, and then invoking methods on those objects.  All
of their attributes are private, so you must use accessor methods.

The usual way that Set::Relation indicates a failure is to throw an
exception; most often this is due to invalid input.  If an invoked routine
simply returns, you can assume that it has succeeded, even if the return
value is undefined.

=head2 Constructor Submethods

This is currently the only routine declared by Set::Relation that you
invoke off of the class name; currently you invoke all other routines off
of a Set::Relation object.

=over

=item C<submethod new of Set::Relation (Array|Hash|Set::Relation
:$members?)>

This constructor submethod creates and returns a new C<Set::Relation>
object, representing a single relation value, that is initialized primarily
using the multi-dimensional Perl data structure given in its C<$members>
argument, which defines the member attributes and tuples of the new
relation.  The allowed formats of a C<$members> argument mostly are those
defined by the Muldis D language dialect
L<HDMD_Perl5_Tiny|Muldis::D::Dialect::HDMD_Perl5_Tiny> for a C<QRelation>
(or C<Relation>) value literal node payload, except that attribute values
are just ordinary Perl values and not HDMD_Perl5_Tiny value literal nodes.
Examples are:

    # Zero attrs + zero tuples.
    my $r1 = Set::Relation->new( members => [] );

    # 3 attrs + zero tuples.
    my $r2 = Set::Relation->new( members => [ 'x', 'y', 'z' ] );

    # Zero attrs + 1 tuple
    my $r3 = Set::Relation->new( members => [ {} ] );

    # Named attributes format: 3 attrs + 1 tuple.
    my $r4 = Set::Relation->new( members => [
        {
            'login_name' => 'hartmark',
            'login_pass' => 'letmein',
            'is_special' => 1,
        },
    ] );

    # Ordered attributes format: 2 attrs + 1 tuple.
    my $r5 = Set::Relation->new( members => [ [ 'name', 'age' ], [
        [ 'Michelle', 17 ],
    ] ] );

However, Set::Relation also supports a few additional, trivial formats for
C<$members>, as illustrated here:

    # The default value of a Set::Relation has zero attrs + zero tuples.
    my $r6 = Set::Relation->new();

    # How to wrap any single tuple in a relation.
    my $r7 = Set::Relation->new( members => {
        'login_name' => 'hartmark',
        'login_pass' => 'letmein',
        'is_special' => 1,
    } );

    # One way to clone a relation object.
    my $r8 = Set::Relation->new( members => $r5 );

The new Set::Relation is initially a mutable object; its identity is not
frozen.

=back

=head2 Accessor Methods

These Set::Relation object methods are mainly about extracting object
attributes, essentially the reverse process of an object constructor; but
some of these will mutate aspects of objects besides what relation
attributes and tuples they have, and some do other misc things.

=over

=item C<method clone of Set::Relation ($self:)>

This method results in a new Set::Relation object that has an exact clone
of its invocant's attributes and tuples.  The new Set::Relation is
initially a mutable object; its value identity is not frozen, regardless of
whether the invocant is frozen or not.

=item C<method export_for_new of Hash ($self: Bool|Array $want_ord_attrs?)>

This method results in a Perl Hash value whose Hash keys and values you can
give as argument names and values to C<new> such that the latter would
result in a clone of this method's invocant, as if you had used C<clone>.
In other words, C<export_for_new> is the inverse function to C<new>.  If
this method's C<$want_ord_attrs> argument is missing or false, then the
exported attributes and tuples are in named attributes format; if that
argument is true, they are in ordered attributes format.  If
C<$want_ord_attrs> is a Perl Array then it is assumed to contain a list of
attribute names matching those of the invocant, and so it is specifying
what order the attributes should be in the result; otherwise if
C<$want_ord_attrs> is the Perl string value C<1>, then the result will have
its attributes ordered alphabetically by attribute name (see the C<heading>
method docs for why that is the case).

=item C<method freeze_identity ($self:)>

This mutator method causes the invocant to become immutable when invoked;
it freezes the invocant's value identity.  This change is not reversible
(an immutable Set::Relation object can't be made mutable again), however
invoking C<clone> on said object will give you a mutable duplicate.

=item C<method which of Str ($self:)>

This mutator method results in a character string representation of the
invocant's value identity, and when invoked it has the side-effect of
making the invocant immutable (as per C<freeze_identity>) if it isn't
already.  The identity value result of this method is essentially a
serialization of all the invocant's attribute names and tuple values, all
of which are encoded and sorted in such a way that any 2 Set::Relation
values having the same attributes and tuples are guaranteed to have the
same value identity, and any 2 with different attributes or tuples are
guaranteed to have different ones.  This method is analagous to the special
C<WHICH> method of Perl 6 and lets you treat Set::Relation as a "value
type".

=item C<method members of Array ($self: Bool|Array $want_ord_attrs?)>

This method results in a Perl Array value as per the 'members' element of
the Hash that C<export_for_new> would result in with the same invocant and
with the same arguments.

=item C<method heading of Array ($self:)>

This method results in a Perl Array value whose elements are the attribute
names of the invocant.  The attribute names are sorted alphabetically so
that if this method's result is used together with the result of invoking
C<body> on the same invocant with a non-Array but true valued
C<$want_ord_attrs> then the default order of the ordered attributes
resulting from C<body> matches the default order resulting from C<heading>;
in contrast, if C<body> was invoked to return attributes in named format,
it doesn't matter what order C<heading> returns their names in.

=item C<method body of Array ($self: Bool|Array $want_ord_attrs?)>

This method results in a Perl Array value whose elements are the tuples of
the invocant.  Each tuple is either a Perl Hash or a Perl Array depending
on the value of the C<$want_ord_attrs>, like with the C<members> method.

=back

=head2 Mutator Methods

Invocations of these Set::Relation object methods will cause their
invocants to mutate.  But they do not mutate any of their non-invocant
arguments.  These methods also result in their invocants post-mutation, for
the convenience of users that like to chain method calls.

=over

=item C<method evacuate of Set::Relation ($topic:)>

This mutator method deletes all of the tuples in its invocant relation.
For a non-mutating equivalent, see the C<empty> functional method.

=item C<method insert of Set::Relation ($r: Array|Hash $t)>

This mutator method inserts its tuples argument into its invocant relation.
For a non-mutating equivalent, see the C<insertion> functional method.

=item C<method delete of Set::Relation ($r: Array|Hash $t)>

This mutator method deletes its tuples argument from its invocant relation.
For a non-mutating equivalent, see the C<deletion> functional method.

=back

=head2 Single Input Relation Functional Methods

These Set::Relation object methods are pure functional, each one whose
execution results in a value and each one not mutating anything or having
any side-effects; the sole exception to the latter is that some may mutate
certain internals of some existing Set::Relation objects in ways that don't
affect their value identity, such as by creating indexes to speed up
current and future operations with said objects.

These methods each have a single Set::Relation object as input, which is
the invocant.  Some of them also result in a Set::Relation object while
others do not.

=over

=item C<method degree of UInt ($topic:)>

This functional method results in the degree of its invocant (that is, the
count of attributes it has).

=item C<method is_nullary of Bool ($topic:)>

This functional method results in true iff its invocant has a degree of
zero (that is, it has zero attributes), and false otherwise.

=item C<method cardinality of UInt ($topic:)>

This functional method results in the cardinality of its invocant (that is,
the count of tuples its body has).

=item C<method is_empty of Bool ($topic:)>

This functional method results in true iff its invocant has a cardinality
of zero (that is, it has zero tuples), and false otherwise.

=item C<method is_member of Bool ($r: Array|Hash $t)>

This functional method results in true iff all of the tuples of its C<$t>
argument match tuples of its invocant (that is, iff conceptually C<$t> is a
member of C<$r>), and false otherwise.  This method is like C<is_subset>
except that the tuples being looked for don't have to be wrapped in a
relation.

=item C<method empty of Set::Relation ($topic:)>

This functional method results in the empty relation of the same heading of
its invocant, that is having the same degree and attribute names; it has
zero tuples.

=item C<method insertion of Set::Relation ($r: Array|Hash $t)>

This functional method results in a relation that is the relational union
of C<$r> and a relation whose tuples are C<$t>; that is, conceptually the
result is C<$t> inserted into C<$r>.  As a trivial case, if all of C<$t>
already exist in C<$r>, then the result is just C<$r>.

=item C<method deletion of Set::Relation ($r: Array|Hash $t)>

This functional method results in a relation that is the relational
difference from C<$r> of a relation whose tuples are C<$t>; that is,
conceptually the result is C<$t> deleted from C<$r>.  As a trivial case, if
all of C<$t> already doesn't exist in C<$r>, then the result is just C<$r>.

=item C<method rename of Set::Relation ($topic: Hash $map)>

This functional method results in a relation value that is the same as its
C<$topic> invocant but that some of its attributes have different names.
Each element of the Hash argument C<$map> specifies how to rename one
C<$topic> attribute, with the element's Hash key and Hash value
representing the old and new names of a C<$topic> attribute, respectively.
As a trivial case, this method's result is C<$topic> if C<$map> has no
elements.  This method supports renaming attributes to each others' names.
This method will fail if C<$map> specifies any old names that C<$topic>
doesn't have, or any new names that are the same as C<$topic> attributes
that aren't being renamed.

=item C<method projection of Set::Relation ($topic: Array $attrs)>

This functional method results in the relational projection of its
C<$topic> invocant that has just the subset of attributes of C<$topic>
which are named in its C<$attrs> argument.  As a trivial case, this
method's result is C<$topic> if C<$attrs> lists all attributes of
C<$topic>; or, it is a nullary relation if C<$attrs> is empty.  This method
will fail if C<$attrs> specifies any attribute names that C<$topic> doesn't
have.

=item C<method cmpl_projection of Set::Relation ($topic: Array $attrs)>

This functional method is the same as C<projection> but that it results in
the complementary subset of attributes of its invocant when given the same
argument.

=item C<method restriction of Set::Relation ($topic: Code $func, Any
$assuming?)>

This functional method results in the relational restriction of its
C<$topic> invocant as determined by applying the Bool-resulting Perl
subroutine reference (having signature C<of Bool (Hash $topic, Any
$assuming?)>) given in its C<$func> argument when said subroutine is
curried by its C<$assuming> argument.  The result relation has the same
heading as C<$topic>, and its body contains the subset of C<$topic> tuples
where, for each tuple, the subroutine given in C<$func> results in true
when passed the tuple as its C<$topic> argument and C<$assuming> as its
C<$assuming> argument.  As a trivial case, if C<$func> is defined to
unconditionally result in true, then this method results simply in
C<$topic>; or, for an unconditional false, this method results in the empty
relation with the same heading.  Note that this operation is also
legitimately known as I<where>.  See also the C<semijoin> method, which is
a simpler-syntax alternative for C<restriction> in its typical usage where
restrictions are composed simply of anded or ored tests for attribute value
equality.

=item C<method cmpl_restriction of Set::Relation ($topic: Code $func, Any
$assuming?)>

This functional method is the same as C<restriction> but that it results in
the complementary subset of tuples of C<$topic> when given the same
arguments.  See also the C<semidifference> method.

=item C<method extension of Set::Relation ($topic: Array $attrs, Code
$func, Any $assuming?)>

This functional method results in the relational extension of its C<topic>
invocant as determined by applying the tuple-resulting Perl subroutine
reference (having signature C<of Hash (Hash $topic, Any $assuming?)>) given
in its C<$func> argument when said subroutine is curried by its
C<$assuming> argument.  The result relation has a heading that is a
superset of that of C<$topic>, and its body contains the same number of
tuples, with all attribute values of C<$topic> retained, and possibly extra
present, determined as follows; for each C<$topic> tuple, the subroutine
given in C<$func> results in a second tuple when passed the first tuple as
its C<$topic> argument and C<$assuming> as its C<$assuming> argument; the
first and second tuples must have no attribute names in common, and the
result tuple is derived by joining (cross-product) the tuples together.  As
a trivial case, if C<$func> is defined to unconditionally result in the
degree-zero tuple, then this function results simply in C<$topic>.  Now,
C<extension> requires the extra C<$attrs> argument to prevent ambiguity in
the general case where C<$topic> might have zero tuples, because in that
situation, C<$func> would never be invoked, and the names of the attributes
to add to C<$topic> are not known (we don't generally assume that
C<extension> can reverse-engineer C<$func> to see what attributes it would
have resulted in).  This function will fail if C<$topic> has at least 1
tuple and the result of C<$func> does not have matching attribute names to
those named by C<$attrs>.

=item C<method static_extension of Set::Relation ($topic: Hash $attrs)>

This functional method is a simpler-syntax alternative to both C<extension>
and C<product> in the typical scenario of extending a relation, given in
the C<$topic> invocant, such that every tuple has mutually identical values
for each of the new attributes; the new attribute names and common values
are given in the C<$attrs> argument.

=item C<method map of Set::Relation ($topic: Array $result_attrs, Code
$func, Any $assuming?)>

This functional method provides a convenient one-place generalization of
per-tuple transformations that otherwise might require the chaining of up
to a half-dozen other operators like restriction, extension, and rename.
This method results in a relation each of whose tuples is the result of
applying, to each of the tuples of its C<$topic> invocant, the
tuple-resulting Perl subroutine reference (having signature C<of Hash (Hash
$topic, Any $assuming?)>) given in its C<$func> argument when said
subroutine is curried by its C<$assuming> argument.  There is no
restriction on what attributes the result tuple of C<$func> may have
(except that all tuples from C<$func> must have compatible headings); this
tuple from C<$func> would completely replace the original tuple from
C<$topic>.  The result relation has a cardinality that is the same as that
of C<$topic>, unless the result of C<$func> was redundant tuples, in which
case the result has appropriately fewer tuples.  As a trivial case, if
C<$func> is defined to unconditionally result in the same tuple as its own
C<$topic> argument, then this function results simply in C<$topic>; or, if
C<$func> is defined to have a static result, then this function's result
will have just 0..1 tuples.  Now, C<map> requires the extra
C<$result_attrs> argument to prevent ambiguity in the general case where
C<$topic> might have zero tuples, because in that situation, C<$func> would
never be invoked, and the names of the attributes of the result are not
known (we don't generally assume that C<map> can reverse-engineer C<$func>
to see what attributes it would have resulted in).  This method will fail
if C<$topic> has at least 1 tuple and the result of C<$func> does not have
matching attribute names to those named by C<$result_attrs>.

=back

=head2 Multiple Input Relation Functional Methods

These Set::Relation object methods are pure functional, each one whose
execution results in a value and each one not mutating anything or having
any side-effects; the sole exception to the latter is that some may mutate
certain internals of some existing Set::Relation objects in ways that don't
affect their value identity, such as by creating indexes to speed up
current and future operations with said objects.

These methods each have at least 2 Set::Relation objects as input, one of
which is the invocant and the other of which is an additional argument.
Some of them also result in a Set::Relation object while others do not.

=over

=item C<method is_identical of Bool ($topic: Set::Relation $other)>

This functional method results in true iff its (mutually commutative)
invocant and argument are exactly the same value (that is, Set::Relation
considers them to have the same value identity), and false otherwise.

=item C<method is_subset of Bool ($look_in: Set::Relation $look_for)>

This functional method results in true iff the set of tuples comprising
C<$look_for> is a subset of the set of tuples comprising C<$look_in> (both
must have the same heading regardless), and false otherwise.

=item C<method is_proper_subset of Bool ($look_in: Set::Relation
$look_for)>

This functional method is exactly the same as C<is_subset> except that it
results in false if C<$look_in> and C<$look_for> are identical.

=item C<method is_disjoint of Bool ($topic: Set::Relation $other)>

This functional method results in true iff the set of tuples comprising
each of its same-heading mutually commutative invocant and argument are
mutually disjoint, that is, iff the intersection of the invocant and
argument is empty; it results in false otherwise.

=item C<method union of Set::Relation ($topic: Set::Relation $other)>

This functional method results in the relational union/inclusive-or of its
same-heading invocant and argument.  The result relation has the same
heading as the input relations, and its body contains every tuple that is
in either of the inputs.  Relational union is both a commutative and
associative operation, and its identity value is the same-heading empty
relation value (having zero tuples).

=item C<method exclusion of Set::Relation ($topic: Set::Relation $other)>

This functional method results in the relational exclusion/exclusive-or of
its same-heading invocant and argument.  The result relation has the same
heading as the input relations, and its body contains every tuple that is
in just one of the two inputs.  Relational exclusion is both a commutative
and associative operation, and its identity value is the same as for
C<union>.  Note that this operation is also legitimately known as
I<symmetric difference>.

=item C<method intersection of Set::Relation ($topic: Set::Relation
$other)>

This functional method results in the relational intersection/and of its
same-heading invocant and argument.  The result relation has the same
heading as the input relations, and its body contains only the tuples that
are in both of the inputs.  Relational intersection is both a commutative
and associative operation, and its identity value is the same-heading
universal relation value (having all the tuples that could possible exist
together in a common relation value with that heading; this is impossibly
large to represent in the general case, except perhaps lazily).

=item C<method difference of Set::Relation ($source: Set::Relation
$filter)>

This functional method results in the relational difference when its
C<$filter> argument is subtracted from its same-heading C<$source>
invocant.  The result relation has the same heading as the input relations,
and its body contains only the tuples that are in C<$source> and are not in
C<$filter>.  Note that this I<difference> operator is conceptually a
special case of I<semidifference>, applicable when the headings of the
inputs are the same.

=item C<method semidifference of Set::Relation ($source: Set::Relation
$filter)>

This functional method is the same as C<semijoin> but that it results in
the complementary subset of tuples of C<$source> when given the same
arguments.  Note that this operation is also legitimately known as
I<antijoin> or I<anti-semijoin>.

=item C<method semijoin of Set::Relation ($source: Set::Relation $filter)>

This functional method results in the relational semijoin of its invocant
and argument.  The result relation has the same heading as C<$source>, and
its body contains the subset of C<$source> tuples that match those of
C<$filter> as per C<join>.  Note that relational semijoin is conceptually a
short-hand for first doing an ordinary relational join between C<$source>
and C<$filter>, and then performing a relational projection on all of the
attributes that just C<$source> has.

=item C<method join of Set::Relation ($topic: Set::Relation $other)>

This functional method results in the relational join (natural inner join)
of its invocant and argument.  The result relation has a heading that is a
union of the headings of the input relations, and its body is the result of
first pairwise-matching every tuple of the input relations, then where each
member of a tuple pair has attribute names in common, eliminating pairs
where the values of those attributes differ and unioning the remaining said
tuple pairs, then eliminating any result tuples that duplicate others.
Relational join is both a commutative and associative operation, and its
identity value is the relation value having zero attributes and a single
tuple.  As a trivial case, if either input relation has zero tuples, then
the method's result will too; or, if either input is the nullary relation
with one tuple, the result is the other input (see identity value); or, if
the inputs have no attribute names in common, then the join of those is a
cartesian product; or, if the inputs have all attribute names in common,
then the join of those is an intersection; or, if one input's set of
attribute names is a proper subset of the other's, then the join of just
those two is a semijoin with the former filtering the latter.

=item C<method product of Set::Relation ($topic: Set::Relation $other)>

This functional method results in the relational cartesian/cross product of
its invocant and argument; it is conceptually a special case of C<join>
where the input relations have mutually distinct attribute names; unlike
C<join>, C<product> will fail if any inputs have attribute names in common.

=item C<method quotient of Set::Relation ($dividend: Set::Relation
$divisor)>

This functional method results in the quotient when its C<$dividend>
invocant is divided by its C<$divisor> argument using relational division.
Speaking informally, say the relations C<$dividend> and C<$divisor> are
called C<A> and C<B>, and their attribute sets are respectively named
C<{X,Y}> and C<{Y}>, then the result relation has a heading composed of
attributes C<{X}> (so the result and C<$divisor> headings are both
complementary subsets of the C<$dividend> heading); the result has all
tuples C<{X}> such that a tuple C<{X,Y}> appears in C<A> for all tuples
C<{Y}> appearing in C<B>; that is, C<A / B> is shorthand for C<A{X} -
((A{X} * B) - A){X}>.

=item C<method composition of Set::Relation ($topic: Set::Relation $other)>

This functional method results in the relational composition of its
mutually commutative invocant and argument.  It is conceptually a
short-hand for first doing an ordinary relational join between the input
relations, and then performing a relational projection on all of the
attributes that only one of the arguments has; that is, the result has all
of and just the attributes that were not involved in matching the tuples of
the inputs.

=back

=head1 DIAGNOSTICS

I<This documentation is pending.>

=head1 CONFIGURATION AND ENVIRONMENT

I<This documentation is pending.>

=head1 DEPENDENCIES

This file requires any version of Perl 5.x.y that is at least 5.8.1, and
recommends one that is at least 5.10.0.

It also requires these Perl 5 packages that are bundled with any version of
Perl 5.x.y that is at least 5.10.0, and are also on CPAN for separate
installation by users of earlier Perl versions:
L<version-ver(0.74..*)|version>.

It also requires these Perl 5 packages that are on CPAN:
L<Moose-ver(0.64..*)|Moose>.

=head1 INCOMPATIBILITIES

None reported.

=head1 SEE ALSO

The separate all-documentation distribution L<Muldis::D> is the formal
definition of the Muldis D language, a portion of which Set::Relation is
mainly based on.

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

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENSE AND COPYRIGHT

Set::Relation is Copyright © 2006-2009, Darren Duncan.  All rights
reserved.

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

None yet.

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
way of Muldis Data Systems; see (L<http://www.muldis.com/>) for details.

=cut
