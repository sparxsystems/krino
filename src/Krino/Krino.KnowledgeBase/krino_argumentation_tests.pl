:- use_module(krino_argumentation).
:- use_module(krino_proof_meta_interpreter).


% Demaging the grass is prohibited.
% Walking on the grass demages the grass.
% Cycling on the grass demages the grass.
% Cycling on the grass is prohibited because walking on the grass is prohibited.

% Form: a is X because b is X
% Form: a is X because a is Y



% Demaging grass is prohibited.
prohibited(Activity) :- demages(Activity, grass).

% Touching the grass demages the grass.
demages(Activity, grass) :- touches(Activity, grass).

% Walking on the grass touches the grass.
touches(on(walking, grass), grass).

% Cycling on the grass touches the grass.
touches(on(cycling, grass), grass).

% Cycling on the grass is prohibited because walking on the grass is prohibited.
%k_argument_form(k_because(k_prohibited(k_on(cycling, grass)), k_prohibited(k_on(cycling, grass))), Form).

r :-
    has(a, b);
    has(a, c).


has(a, b).
has(a, c).


:- begin_tests(krino_argumentation).

test(k_clause) :-
    k_clause(has(a, b), SubjectTerm, PredicateTerm),
    SubjectTerm = a,
    PredicateTerm = has.

test(k_argument) :-
    k_argument(because(has(a, b), has(a, c)), ConclusionClause, PremiseClause),
    ConclusionClause = has(a, b),
    PremiseClause = has(a, c).
    
test(k_argument_form_aXbX) :-
    k_argument_form(because(prohibited(cycling), prohibited(walking)), Form),
    Form = aXbX.

test(k_argument_form_aXaY) :-
    k_argument_form(because(has(a, b), like(a, b)), Form),
    Form = aXaY.

test(k_argument_evaluate) :-
    k_argument_evaluate(because(has(a, b), has(a, c))).

test(k_argument_evaluate_proof) :-
    k_proof(k_argument_evaluate(because(has(a, b), has(a, c))), Proof),
    write(Proof),
    Proof = [].


:- end_tests(krino_argumentation).