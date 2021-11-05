:- use_module(krino_common).

has(a, b).
has(b, c).
has(c, d).

:- begin_tests(krino_common).

test(k_tracking_call) :-
    k_tracking_call(has(a, b), [], TrackingResult),
    TrackingResult = [has(a, b)].

test(k_exists) :-
    k_exists(has(a, b)).

:- end_tests(krino_common).
