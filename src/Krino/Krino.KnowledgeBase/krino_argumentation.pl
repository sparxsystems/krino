:- module(krino_argumentation,
    [
        k_argument_evaluate/2,
        k_argument_form/2,
        k_argument/3,
        k_clause/3
    ]).

:- use_module(krino_proof_meta_interpreter).


%k_argument(ConclusionClause, PremiseClause, Lever) :-
%    k_tracking_call(PremiseClause, [], PremiseTrackingResult),
%    k_tracking_call(ConclusionClause, [], ConclusionTrackingResult),
%    intersection(ConclusionTrackingResult, PremiseTrackingResult, Lever).


k_argument_evaluate(ArgumentTerm, Lever) :-
    % get the form of the argument.
    k_argument_form(ArgumentTerm, Form),
    k_argument(ArgumentTerm, ConclusionClause, PremiseClause),

    % evaluate the conclusion and provide the proof chain if true.
    k_proof(ConclusionClause, ConclusionProof),
    %write(ConclusionProof), nl,

    % get the first reason why the conclusion is true.
    ConclusionProof = [_ | [ConclusionReason | _]],
    (
        % if same subjects and different predicates.
        Form = aXaY,
        PremiseClause,
        % conclusion must be same as the premise.
        ConclusionReason = PremiseClause
        ;
        % or if different subjects and same predicates.
        Form = aXbX,
        k_proof(PremiseClause, PremiseProof),
        % get the first reason why the premise is true.
        PremiseProof = [_ | [PremiseReason | _]],
        k_clause(ConclusionReason, _, ConclusionReasonPredicate),
        k_clause(PremiseReason, _, PremiseReasonPredicate),
        % conclusion predicate must be same as the reason predicate from the premise.
        ConclusionReasonPredicate = PremiseReasonPredicate
    ),
    Lever = ConclusionReason,
    Lever \= true.



    

k_argument_form(ArgumentTerm, Form) :-
    k_argument(ArgumentTerm, ConclusionClause, PremiseClause),
    k_clause(ConclusionClause, ConclusionSubject, ConclusionPredicate),
    k_clause(PremiseClause, PremiseSubject, PremisePredicate),
    (
        ConclusionSubject = PremiseSubject,
        Form = aXaY, !
        ;
        ConclusionPredicate = PremisePredicate,
        Form = aXbX, !
    ).

k_argument(ArgumentTerm, ConclusionClause, PremiseClause) :- ArgumentTerm =.. [_, ConclusionClause, PremiseClause].

k_clause(ClauseTerm, SubjectTerm, PredicateTerm) :- ClauseTerm =.. [PredicateTerm, SubjectTerm | _].
