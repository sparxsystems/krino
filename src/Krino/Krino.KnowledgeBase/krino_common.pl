:- module(krino_common,
    [
        k_tracking_call/3,
        k_exists/1
    ]).



transitive_relation(Goal, X, Y, TrackingResult) :- transitive_relation(Goal, X, Y, [], [], TrackingResult).
transitive_relation(Goal, X, Y, L, PreviousTracking, NewTracking) :-
    Term =.. [Goal, X, Y],
    k_tracking_call(Term, PreviousTracking, NewTracking),
    \+ memberchk((X, Y), L).
transitive_relation(Goal, X, Y, L, PreviousTracking, NewTracking) :-
    Term =.. [Goal, X, Z],
    k_tracking_call(Term, PreviousTracking, NewTrackingTmp),
    \+ memberchk((X, Z), L),
    transitive_relation(Goal, Z, Y, [(X, Z) | L], NewTrackingTmp, NewTracking).


k_tracking_call(Goal, PreviousTracking, NewTracking) :-

    % Get the goal which includes parameteres for the tracking.
    Goal =.. [GoalName | _],
    functor(Goal, GoalName, GoalArity),
    TrackableGoalArity is GoalArity + 2,
    functor(TrackingGoal, GoalName, TrackableGoalArity),

    k_exists(TrackingGoal),
    % call the goal with the tracking parameters
    call(Goal, PreviousTracking, NewTracking)
    ;
    k_exists(Goal),
    call(Goal),
    append(PreviousTracking, [Goal], NewTracking).

k_exists(Term) :- current_predicate(_, Term).