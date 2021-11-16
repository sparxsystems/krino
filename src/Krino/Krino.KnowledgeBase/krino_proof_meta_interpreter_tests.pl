:- use_module(krino_proof_meta_interpreter).


:- import(krino_proof_meta_interpreter:k_add_proof/3).


has(a,b).
check_has(A, B) :- has(A, B), ! ; has(B, A).


:- begin_tests(krino_proof_meta_interpreter).

test(k_add_proof) :-
    k_add_proof(has(a, b), [], Result), !,
    %write(Result),
    Result = [has(a, b)].

test(k_add_proof_true) :-
    k_add_proof(true, [], Result), !,
    %write(Result),
    Result = [true].

test(k_add_proof_double_true) :-
    k_add_proof(true, [true], Result), !,
    %write(Result),
    Result = [true].

test(k_add_proof_complex) :-
    k_add_proof((has(a, b), has(b, a)), [], Result), !,
    %write(Result),
    Result = [(has(a, b), has(b, a))].


test(k_proof_true) :-
    k_proof(true, Proof), !,
    %write(Proof),
    Proof = [true].

test(k_proof) :-
    k_proof(has(a, b), Proof), !,
    %write(Proof),
    Proof = [has(a, b), true].
    

test(k_proof_disjunction) :-
    k_proof(check_has(b, a), Proof), !,
    %write(Proof),
    Proof = [check_has(b, a), (has(b, a), ! ; has(a, b)), has(a, b), true].

test(k_proof_disjunction_not_true) :-
    \+ k_proof(check_has(b, bla), _), !.


:- end_tests(krino_proof_meta_interpreter).