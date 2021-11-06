:- module(krino_common,
    [
        k_transitive_relation/3,
        k_exists/1
    ]).


k_reflexive_relation(Goal, X) :-
    call(Goal, X, X).

k_symmetric_relation(Goal, X, Y) :- 
    call(Goal, X, Y),
    call(Goal, Y, X).


k_transitive_relation(Goal, X, Y) :- k_transitive_relation(Goal, X, Y, []).
k_transitive_relation(Goal, X, Y, L) :-
    Term =.. [Goal, X, Y],
    Term,
    \+ memberchk((X, Y), L).
k_transitive_relation(Goal, X, Y, L) :-
    Term =.. [Goal, X, Z],
    Term,
    \+ memberchk((X, Z), L),
    k_transitive_relation(Goal, Z, Y, [(X, Z) | L]).


k_exists(Term) :- current_predicate(_, Term).