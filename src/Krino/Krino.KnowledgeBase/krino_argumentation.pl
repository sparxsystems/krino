:- module(krino_argumentation,
    [
        k_argument_evaluate/2,
        k_argument_form/2,
        k_argument/3,
        k_clause/3
    ]).

:- use_module(krino_proof_meta_interpreter).



k_argument_evaluate(ArgumentTerm, Lever) :-
    % get the form of the argument.
    k_argument_form(ArgumentTerm, Form),
    k_argument(ArgumentTerm, ConclusionClause, PremiseClause),

    % evaluate the conclusion and provide the proof chain if true.
    k_proof(ConclusionClause, ConclusionProof),

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


% k_clause(?ArgumentTerm, ?ConclusionClause, ?PremiseClause)
% true if ArgumentTerm = ArgumentTerm(ConclusionClause, PremiseClause)
k_argument(ArgumentTerm, ConclusionClause, PremiseClause) :- ArgumentTerm =.. [_, ConclusionClause, PremiseClause].


% k_clause(?ClauseTerm, ?SubjectTerm, ?PredicateTerm)
% true if ClauseTerm = PredicateTerm(Subject, ...).
k_clause(ClauseTerm, SubjectTerm, PredicateTerm) :-
    % If getting SubjectTerm and PredicateTerm from ClauseTerm.
    \+ var(ClauseTerm),
    ClauseTerm =.. [PredicateTerm, SubjectTerm | _], !
    ;
    % If ClauseTerm is not a variable and shall be resolved.
    (
        % If PredicateTerm is not a variable.
        \+ var(PredicateTerm), !
        ;
        % If PredicateTerm then resolve it.
        var(PredicateTerm),
            % resolve PredicateTerm
            VerbTerm =.. [is_verb, PredicateTerm],
            call(VerbTerm)
    ),
    (
        ClauseTermTmp =.. [PredicateTerm, _],
        % if such term exist then resolve SubjectTerm
        current_predicate(_, ClauseTermTmp),
        ClauseTerm =.. [PredicateTerm, SubjectTerm]
        ;
        ClauseTermTmp =.. [PredicateTerm, _, _],
        % if such term exist then resolve SubjectTerm
        current_predicate(_, ClauseTermTmp),
        ClauseTerm =.. [PredicateTerm, SubjectTerm, _]
    ).

    
