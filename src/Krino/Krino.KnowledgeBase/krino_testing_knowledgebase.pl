

has(a,b).
check_has(A, B) :- has(A, B), ! ; has(B, A).