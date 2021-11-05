
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


k_verb_2(Verb, Subject, Object, Term) :- Term =.. [Verb, Subject, Object].


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

    


