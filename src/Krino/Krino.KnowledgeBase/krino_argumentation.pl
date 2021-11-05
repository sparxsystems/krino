:- module(krino_argumentation,
    [
        k_argument_evaluate/1,
        k_argument_form/2,
        k_argument/3,
        k_clause/3
    ]).

:- use_module(krino_common).


%k_argument(ConclusionClause, PremiseClause, Lever) :-
%    k_tracking_call(PremiseClause, [], PremiseTrackingResult),
%    k_tracking_call(ConclusionClause, [], ConclusionTrackingResult),
%    intersection(ConclusionTrackingResult, PremiseTrackingResult, Lever).


k_argument_evaluate(ArgumentTerm) :-
    k_argument(ArgumentTerm, ConclusionClause, PremiseClause),
    PremiseClause,
    ConclusionClause.

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
