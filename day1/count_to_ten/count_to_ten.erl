%% @author Anatolii Kosorukov <java1cprog@yandex.ru>
%% @copyright 2019 by rustkas
%% @version 1.0

-module(count_to_ten).
-export([test/0]).
-export([start/0]).
	
%% File: "count_to_ten.erl"
%% ------------------------
-spec test() -> 'test_worked'.
	
test() ->
	start(),
	test_worked.

-spec start() -> ok.
	
start() ->
	start(9).

-spec start(non_neg_integer()) -> ok.	

start(0) ->
	io:format("Start!~n"),
	ok;
	
start(Counter) ->
	io:format("~p~n",[Counter]),
	start(Counter-1).

%% commands
%
% c(count_to_ten).
% c(count_to_ten). count_to_ten:test().

% erlc count_to_ten.erl && erl -noshell -s count_to_ten test -s init stop	

% typer count_to_ten.erl
% dialyzer --no_check_plt --src count_to_ten.erl

	