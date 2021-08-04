is_kind_of(speaker, actor).
is_kind_of(actor, actant).

is_specialization_of(X, Z) :- is_kind_of(X, Z); is_kind_of(X, Y), is_specialization_of(Y, Z).
