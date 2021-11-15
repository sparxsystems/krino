:- module(krino_argumentation,
    [
        k_argument_evaluate/2,
        k_argument_form/2,
        k_argument/3,
        k_clause/3
    ]).

:- use_module(krino_proof_meta_interpreter).


k_verbs(List).


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
    % Getting SubjectTerm and PredicateTerm from ClauseTerm.
    \+ var(ClauseTerm),
    ClauseTerm =.. [PredicateTerm, SubjectTerm | _]
    ;
    % Getting ClauseTerm from SubjectTerm and PredicateTerm.
    var(ClauseTerm), \+ var(SubjectTerm), \+ var(PredicateTerm),
    (
        ClauseTerm =.. [PredicateTerm, SubjectTerm],
        current_predicate(_, ClauseTerm)
        ;
        ClauseTermTmp =.. [PredicateTerm, _, _],
        current_predicate(_, ClauseTermTmp),
        % resolve DirectObjectTerm
        call(PredicateTerm, SubjectTerm, DirectObjectTerm),
        ClauseTerm =.. [PredicateTerm, SubjectTerm, DirectObjectTerm]
    )
    ;
    % Getting ClauseTerm and SubjectTerm from PredicateTerm.
    var(ClauseTerm), var(SubjectTerm), \+ var(PredicateTerm),
    (
        ClauseTermTmp =.. [PredicateTerm, _],
        % if such term exists.
        current_predicate(_, ClauseTermTmp),
        % resolve SubjectTerm
        call(PredicateTerm, SubjectTerm),
        ClauseTerm =.. [PredicateTerm, SubjectTerm]
        ;
        ClauseTermTmp =.. [PredicateTerm, _, _],
        % if such term exists.
        current_predicate(_, ClauseTermTmp),
        % resolve SubjectTerm and DirectObjectTerm
        call(PredicateTerm, SubjectTerm, DirectObjectTerm),
        ClauseTerm =.. [PredicateTerm, SubjectTerm, DirectObjectTerm]
    )
    ;
    % Getting ClauseTerm and PredicateTerm from SubjectTerm.
    var(ClauseTerm), \+ var(SubjectTerm), var(PredicateTerm),
    (
        write(0), nl,
        % go via verbs and use them as predicates.
        member(PredicateTerm, verbs),
        (
            write(1), nl,
            ClauseTerm =.. [PredicateTerm, SubjectTerm],
            % if such term exists.
            current_predicate(_, ClauseTerm)
            ;
            ClauseTermTmp =.. [PredicateTerm, _, _],
            % if such term exists.
            current_predicate(_, ClauseTermTmp),
            % resolve SubjectTerm and DirectObjectTerm
            call(PredicateTerm, SubjectTerm, DirectObjectTerm),
            ClauseTerm =.. [PredicateTerm, SubjectTerm, DirectObjectTerm]
        )
    ).

    
