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
:- http_handler('/remove', handle_remove, [method(post)]).
:- http_handler(root(clear), handle_clear, [method(delete)]).
:- http_handler(root(list), handle_list, [method(get)]).
:- http_handler(root(evaluate), handle_evaluate, [method(post)]).


handle_add(Request) :-
    print_message(information, 'ADD'),
    http_read_json(Request, Query),
    maplist(term_string, Terms, Query),
    add_multiple(Terms),
    reply_json_dict(_{result:"OK"}).
    
handle_remove(Request) :-
    print_message(information, 'REMOVE'),
    http_read_json(Request, Query),
    maplist(term_string, Terms, Query),
    remove_multiple(Terms),
    reply_json_dict(_{result:"OK"}).

handle_clear(Request) :-
    print_message(information, 'CLEAR'),
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
    http_read_json(Request, Query),
    term_string(Term, Query),
    (evaluate(Term) -> reply_json_dict(_{result:"TRUE"}); reply_json_dict(_{result:"FALSE"})).


add_multiple(Statements) :- maplist(add, Statements).
add(Statement) :- remove(Statement), assertz(clause(Statement)).

remove_multiple(Statements) :- maplist(remove, Statements).
remove(Statement) :- retractall(clause(Statement)).
clear :- retractall(clause(_)).

list :- listing(clause/1).

evaluate(Statement) :- clause(Statement).

    


