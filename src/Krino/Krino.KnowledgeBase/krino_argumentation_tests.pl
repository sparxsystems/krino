:- use_module(krino_argumentation).


% If an activity damages the grass then it is prohibited.
is_prohibited(Activity) :- damages(Activity, grass).

% If an activity offends the grass then it is prohibited.
is_prohibited(Activity) :- offends(Activity, grass).

% If an activity moves the grass then it damages the grass.
damages(Activity, grass) :- moves(Activity, grass).

% If an activity touches the grass then it moves the grass.
moves(Activity, grass) :- touches(Activity, grass).

% Walking on the grass touches the grass.
touches(on(walking, grass), grass).

% Cycling on the grass touches the grass.
touches(on(cycling, grass), grass).

% Gazing on the grass offends the grass.
offends(on(gazing, grass), grass).


is_verb(is_prohibited).
is_verb(damages).
is_verb(touches).
is_verb(moves).
is_verb(offends).



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
    k_argument_form(because(is_prohibited(cycling), is_prohibited(walking)), Conclusion, Premise, Form),
    Conclusion = is_prohibited(cycling),
    Premise = is_prohibited(walking),
    Form = aXbX.

test(k_argument_form_aXaY) :-
    k_argument_form(because(has(a, b), like(a, b)), Conclusion, Premise, Form), !,
    Conclusion = has(a, b),
    Premise = like(a, b),
    Form = aXaY.

% Cycling on the grass is prohibited because walking on the grass is prohibited.
test(k_argument_evaluate_aXbX) :-
    k_argument_evaluate(because(is_prohibited(on(cycling, grass)), is_prohibited(on(walking, grass))), Lever), !,
    %write(["Lever: " | Lever]),
    % Argument is correct because cycling on the grass damages the grass.
    Lever = damages(on(cycling,grass),grass).

% Cycling on the grass is prohibited because gazing on the grass is prohibited.
% Incorrect argument because cycling and gazing are prohibited due to different reasons - therefore there is no a lever.
test(k_argument_evaluate_aXbX_no_lever) :-
    \+ k_argument_evaluate(because(is_prohibited(on(cycling, grass)), is_prohibited(on(gazing, grass))), _), !.

% Cycling on the grass is prohibited because walking on the grass moves the grass.
% Incorrect argument because there is no a common term (fulcrum) between the conclusion and the premise.
test(k_argument_evaluate_no_form) :-
    \+ k_argument_evaluate(because(is_prohibited(on(cycling, grass)), moves(on(walking, grass), grass)), _), !.

% Walking on the grass damages the grass because it moves the grass.
test(k_argument_evaluate_aXaY) :-
    k_argument_evaluate(because(damages(on(walking, grass), grass), moves(on(walking, grass), grass)), Lever), !,
    %write(["Lever: " | Lever]),
    Lever = moves(on(walking, grass), grass).

% Walking on the grass is prohibited because it moves the grass.
% Incorrect argument because the prohibition is caused by demaging and not touching.
test(k_argument_evaluate_aXaY_unrelated_predicates) :-
    \+ k_argument_evaluate(because(is_prohibited(on(walking, grass)), moves(on(walking, grass), grass)), _), !.

test(k_argument_evaluate_no_lever) :-
    \+ k_argument_evaluate(because(has(a, b), has(a, c)), _).


:- end_tests(krino_argumentation).