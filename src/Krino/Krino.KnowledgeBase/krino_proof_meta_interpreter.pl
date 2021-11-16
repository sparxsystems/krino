:- module(krino_proof_meta_interpreter,
    [
        k_proof/2
    ]).

% Export for the testing purposes.
:- export(krino_proof_meta_interpreter:k_add_proof/3).

k_proof(Goal, Proof) :-
    k_proof(Goal, [], ProofTmp),
    k_add_proof(Goal, ProofTmp, Proof).

% Goal is true.
k_proof(true, [], [true]).

% Conjunction
k_proof((A, B), Previous, Proof) :-
    k_proof(A, Previous, ProofTmp1),
    k_proof(B, ProofTmp1, ProofTmp2),
    k_add_proof(A, ProofTmp2, ProofTmp3),
    k_add_proof(B, ProofTmp3, Proof).

% Disjunction
k_proof((A; B), Previous, Proof) :-
    k_proof(A, Previous, ProofTmp1),
    k_add_proof(A, ProofTmp1, Proof)
    ;
    k_proof(B, Previous, ProofTmp2),
    k_add_proof(B, ProofTmp2, Proof).

% Prolog-built_in goal.
k_proof(Goal, Previous, Previous) :-
    Goal \= true,
    Goal \= (_ , _), % it is not a conjunction
    Goal \= (_ ; _), % it is not a disjunction
    predicate_property(Goal, built_in),
    call(Goal).

k_proof(Goal, Previous, Proof) :-
    Goal \= true,
    Goal \= (_ , _), % it is not a conjunction
    Goal \= (_ ; _), % it is not a disjunction
    \+ predicate_property(Goal, built_in),
    clause(Goal, Body),
    k_proof(Body, Previous, ProofTmp1),
    k_add_proof(Body, ProofTmp1, Proof).



% true, if adding Goal to PreviousProof creates NewProof.
k_add_proof(Goal, PreviousProof, NewProof) :-
    % if Goal is compound or not builtin.
    (compound(Goal) ; \+ predicate_property(Goal, built_in)),
    NewProof = [Goal | PreviousProof], !
    ;
    % if Goal is true and true is not in the list yet.
    Goal = true,
    PreviousProof \= [true | _],
    NewProof = [true | PreviousProof], !
    ;
    % if Goal is true but true is already in the list or Goal is some other builtin.
    NewProof = PreviousProof.

