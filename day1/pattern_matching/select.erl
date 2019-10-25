%% @author Anatolii Kosorukov <java1cprog@yandex.ru>
%% @copyright 2019 by rustkas
%% @version 1.0

-module(select).
-export([test/0]).
-export([do_it/1]).
-export([print/1]).
	
%% File: "select.erl"
%% ------------------
-spec test() -> test_worked.

test() ->
	%true = success =:= do_it(success),
	
	Message = "test",
	true = error =:= do_it({error, Message}),
	true = success =:= do_it(success),
	
	test_worked.

-spec do_it('success' | {'error',string()}) -> 'error' | 'success'.


do_it({error, Message}) ->
	print(Message),
	error;

do_it(success) ->
	print(success),
	success.

-spec print(atom() | string()) -> 'ok'.

print(Message) ->
	io:format("~p~n",[Message]),
	ok.

%% commands
%
% c(select).
% c(select). select:test().

% erlc select.erl && erl -noshell -s select test -s init stop	

% typer select.erl
% dialyzer --no_check_plt --src select.erl



	