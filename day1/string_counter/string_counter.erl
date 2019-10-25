%% @author Anatolii Kosorukov <java1cprog@yandex.ru>
%% @copyright 2019 by rustkas
%% @version 1.0
%% Implementating  BIF tuple_to_list(T) 
-module(string_counter).
-export([test/0]).
-export([words_count/2]).

%% File: "string_counter.erl"
%% --------------------------

-spec test2() -> 'ok'.

test2() ->
	Separator = " ",
	true = 0 == words_count("",Separator),
	ok.
	
-spec test3() -> 'ok'.

test3() ->
	Separator = " ",
	true = 2 == words_count("1 2",Separator),
	ok.

-spec test4() -> 'ok'.

test4() ->
	Separator = " ",
	true = 3 == words_count("Hello, Erlang World!",Separator),
	ok.

-spec test5() -> 'ok'.

test5() ->
	Separator = " ",
	true = 5 == words_count("Seven Languages in Seven Weeks",Separator),
	ok.

	
-spec test() -> 'test_worked'.

test() ->
	test2(),	
	test3(),
	test4(),
	test5(),
	io:format("OK~n",[]),
	test_worked.
	
-spec words_count(Stirng :: string(), _Separator :: string()) -> Result :: 0 | pos_integer().
words_count("" = String, Separator) when is_list(String) andalso is_list(Separator) andalso  length(String) =:= 0 ->
	0;

words_count(String,Separator) when is_list(String) andalso is_list(Separator) ->
	
	% split words by separator
	L = string:split(String,Separator, all),
	
	% get list size
	Size = length(L),
	Size.
	
%% commands
%
% c(string_counter).
% c(string_counter). string_counter:test().

% erlc string_counter.erl && erl -noshell -s string_counter test -s init stop	
% c(string_counter). string_counter:words_count("Hello, Erlang World!"," ").
	