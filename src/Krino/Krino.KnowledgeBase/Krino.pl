:- use_module(library(apply)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).

% Uncomment to enable http logging.
% :- use_module(library(http/http_log)).

:- dynamic clause/1.

%:- initialization(server(8123)).
server(Port) :- http_server(http_dispatch, [port(Port)]).

:- http_handler(root(add), handle_add, [method(post)]).
:- http_handler(root(remove), handle_remove, [method(post)]).
:- http_handler(root(clear), handle_clear, [method(delete)]).
:- http_handler(root(list), handle_list, [method(get)]).
:- http_handler(root(evaluate), handle_evaluate, [method(post)]).


handle_add(Request) :-
    print_message(information, 'ADD'),
    http_read_json(Request, Query),
    maplist(term_string, Terms, Query),
    add_multiple(Terms),
    reply_json_dict(_{}).
    
handle_remove(Request) :-
    print_message(information, 'REMOVE'),
    http_read_json(Request, Query),
    maplist(term_string, Terms, Query),
    remove_multiple(Terms),
    reply_json_dict(_{}).

handle_clear(_) :-
    print_message(information, 'CLEAR'),
    clear,
    reply_json_dict(_{}).

handle_list(_) :-
    print_message(information, 'LIST'),
    reply_html_page(
        [title('list')],
        [h1('list')]
    ).

handle_evaluate(Request) :-
    print_message(information, 'EVALUATE'),
    http_read_json(Request, Query),
    term_string(Term, Query),
    (evaluate(Term) -> reply_json_dict(_{result:true}); reply_json_dict(_{result:false})).


add_multiple(Statements) :- maplist(add, Statements).
add(Statement) :- remove(Statement), assertz(clause(Statement)).

remove_multiple(Statements) :- maplist(remove, Statements).
remove(Statement) :- retractall(clause(Statement)).
clear :- retractall(clause(_)).

list :- listing(clause/1).

evaluate(Statement) :- clause(Statement).



has(a, b).
has(b, c).
has(c, d).

% Demaging the grass is prohibited.
k_prohibited(Activity, Tracking) :- k_prohibited(Activity, [], Tracking).
k_prohibited(Activity, PreviousTracking, NewTracking) :-
    k_tracking_call(k_demages(Activity, grass), PreviousTracking, NewTracking).

% Walking on the grass demages the grass.
k_demages(k_on(walking, grass), grass).

% Cycling on the grass demages the grass.
k_demages(k_on(cycling, grass), grass).

% Cycling on the grass is prohibited because walking on the grass is prohibited.
% Form: a is X because b is X

% He is loudly because he is deaf.
% Form: a is X because a is Y




k_argument(Conclusion, Premise, Lever) :-
    k_tracking_call(Premise, [], PremiseTrackingResult),
    k_tracking_call(Conclusion, [], ConclusionTrackingResult),
    intersection(ConclusionTrackingResult, PremiseTrackingResult, Lever).



relation(Goal, X, reflexive) :- call(Goal, X, X).

relation(Goal, X, Y, symmetric) :-
    call(Goal, X, Y),
    call(Goal, Y, X).

relation(Goal, X, Y, transitive, TrackingResult) :- relation(Goal, X, Y, [], transitive, [], TrackingResult).
relation(Goal, X, Y, L, transitive, PreviousTracking, NewTracking) :-
    Term =.. [Goal, X, Y],
    k_tracking_call(Term, PreviousTracking, NewTracking),
    \+ memberchk((X, Y), L).
relation(Goal, X, Y, L, transitive, PreviousTracking, NewTracking) :-
    Term =.. [Goal, X, Z],
    k_tracking_call(Term, PreviousTracking, NewTrackingTmp),
    \+ memberchk((X, Z), L),
    relation(Goal, Z, Y, [(X, Z) | L], transitive, NewTrackingTmp, NewTracking).


k_tracking_call(Goal, PreviousTracking, NewTracking) :-
    % Get the goal which incudes parameteres for the tracking.
    Goal =.. [GoalName | _],
    functor(Goal, GoalName, GoalArity),
    TrackableGoalArity is GoalArity + 2,
    functor(TrackingGoal, GoalName, TrackableGoalArity),

    % if the goal with the arity for the tracking exists.
    current_predicate(_, TrackingGoal),
    % call the goal with the tracking parameters
    call(Goal, PreviousTracking, NewTracking)
    ;
    % if the goal exists.
    current_predicate(_, Goal),
    call(Goal),
    append(PreviousTracking, [Goal], NewTracking).

    

