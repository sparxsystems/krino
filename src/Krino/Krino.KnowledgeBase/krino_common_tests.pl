:- use_module(krino_common).

has(a, b).
has(b, c).
has(c, d).

:- begin_tests(krino_common).

test(k_transitive_relation) :-
    k_transitive_relation(has, a, d), !.

test(k_transitive_relation) :-
    \+ k_transitive_relation(has, a, a), !.

test(k_exists) :-
    k_exists(has(a, b)).

:- end_tests(krino_common).
