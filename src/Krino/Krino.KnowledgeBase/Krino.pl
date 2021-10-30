:- use_module(library(apply)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).

:- dynamic clause/1.

:- use_module(library(http/http_log)).

:- initialization(server(8123)).
server(Port) :- http_server(http_dispatch, [port(Port)]).

:- http_handler('/add', handle_add, [method(post)]).
:- http_handler(root(add_multiple), handle_add_multiple, [method(post)]).
:- http_handler(root(remove), handle_remove, [method(delete)]).
:- http_handler(root(clear), handle_clear, [method(delete)]).
:- http_handler(root(list), handle_list, [method(get)]).
:- http_handler(root(evaluate), handle_evaluate, [method(get)]).

:- http_handler('/sum', handle_sum, []).

handle_sum(Request) :-
    print_message(information, 'SUM'),
    http_read_json_dict(Request, Query),
    solve(Query, Solution),
    reply_json_dict(Solution).

solve(_{a:X, b:Y}, _{answer:N}) :-
    print_message(information, 'bla'),
    number(X),
    number(Y),
    N is X + Y.


handle_add(Request) :-
    print_message(information, 'ADD'),
    print_message(information, Request),
    http_read_json(Request, Query),
    print_message(information, Query),
    add(Query),
    reply_html_page(
        [title('add')],
        [h1('add')]
    ).

handle_add_multiple(_) :-
    reply_html_page(
        [title('list')],
        [h1('list')]
    ).

handle_remove(_) :-
    reply_html_page(
        [title('list')],
        [h1('list')]
    ).

handle_clear(Request) :-
    print_message(information, 'CLEAR'),
    print_message(information, Request),
    reply_html_page(
        [title('list')],
        [h1('list')]
    ).

handle_list(_) :-
    reply_html_page(
        [title('list')],
        [h1('list')]
    ).

handle_evaluate(Request) :-
    print_message(information, 'EVALUATE'),
    print_message(information, Request),
    reply_html_page(
        [title('list')],
        [h1('list')]
    ).



add(Statement) :- remove(Statement), assertz(clause(Statement)).
add_multiple(Statements) :- maplist(add, Statements).

remove(Statement) :- retractall(clause(Statement)).
clear :- retractall(clause(_)).

list :- listing(clause/1).

evaluate(Statement) :- clause(Statement).

