:- use_module(krino_argumentation).


% If an activity demages the grass then it is prohibited.
is_prohibited(Activity) :- demages(Activity, grass).

% If an activity offends the grass then it is prohibited.
is_prohibited(Activity) :- offends(Activity, grass).

% If an activity touches the grass then it demages the grass.
demages(Activity, grass) :- touches(Activity, grass).

% If an activity moves the grass then it touches the grass.
touches(Activity, grass) :- moves(Activity, grass).

% Walking on the grass moves the grass.
moves(on(walking, grass), grass).

% Cycling on the grass moves the grass.
moves(on(cycling, grass), grass).

% Gazing on the grass offends the grass.
offends(on(gazing, grass), grass).

is_verb(is_prohibited).
is_verb(demages).
is_verb(touches).
is_verb(moves).
is_verb(offends).
is_verb(has).


has(a).
has(a, b).
has(a, c).



:- begin_tests(krino_argumentation).


test(k_clause) :-
    k_clause(has(a, b), SubjectTerm, PredicateTerm), !,
    SubjectTerm = a,
    PredicateTerm = has.

test(k_clause) :-
    k_clause(Clause, a, has), !,
    Clause = has(a).


test(k_argument) :-
    k_argument(because(has(a, b), has(a, c)), ConclusionClause, PremiseClause),
    ConclusionClause = has(a, b),
    PremiseClause = has(a, c).
    
test(k_argument_form_aXbX) :-
    k_argument_form(because(is_prohibited(cycling), is_prohibited(walking)), Form),
    Form = aXbX.

test(k_argument_form_aXaY) :-
    k_argument_form(because(has(a, b), like(a, b)), Form),
    Form = aXaY.

% Cycling on the grass is prohibited because walking on the grass is prohibited.
test(k_argument_evaluate_aXbX) :-
    k_argument_evaluate(because(is_prohibited(on(cycling, grass)), is_prohibited(on(walking, grass))), Lever), !,
    %write(["Lever: " | Lever]),
    % Argument is correct because cycling on the grass demages the grass.
    Lever = demages(on(cycling,grass),grass).

% Cycling on the grass is prohibited because gazing on the grass is prohibited.
% Incorrect argument because cycling and gazing are prohibited due to different reasons - therefore there is no a lever.
test(k_argument_evaluate_aXbX_no_lever) :-
    \+ k_argument_evaluate(because(is_prohibited(on(cycling, grass)), is_prohibited(on(gazing, grass))), _), !.

% Cycling on the grass is prohibited because walking on the grass touches the grass.
% Incorrect argument because there is no a common term (fulcrum) between the conclusion and the premise.
test(k_argument_evaluate_no_form) :-
    \+ k_argument_evaluate(because(is_prohibited(on(cycling, grass)), touches(on(walking, grass), grass)), _), !.

% Walking on the grass demages the grass because it touches the grass.
test(k_argument_evaluate_aXaY) :-
    k_argument_evaluate(because(demages(on(walking, grass), grass), touches(on(walking, grass), grass)), Lever), !,
    %write(["Lever: " | Lever]),
    Lever = touches(on(walking, grass), grass).

% Walking on the grass is prohibited because it touches the grass.
% Incorrect argument because the prohibition is caused by demaging and not touching.
test(k_argument_evaluate_aXaY_unrelated_predicates) :-
    \+ k_argument_evaluate(because(is_prohibited(on(walking, grass)), touches(on(walking, grass), grass)), _), !.

test(k_argument_evaluate_no_lever) :-
    \+ k_argument_evaluate(because(has(a, b), has(a, c)), _).


:- end_tests(krino_argumentation).