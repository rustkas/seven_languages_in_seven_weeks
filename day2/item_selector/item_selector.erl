%% @author Anatolii Kosorukov <java1cprog@yandex.ru>
%% @copyright 2019 by rustkas
%% @version 1.0

-module(item_selector).
-export([test/0]).
-export([keyfind/2, keyword/2,filter/2]).

%% File: "item_selector.erl"
%% -------------------------

-spec language_list() -> [{atom(), string()}].

language_list() ->
	[{erlang, "a functional language"}, 
	{ruby, "an OO language"}, 
	{scala, "a hybrid language"}, 
	{prolog, "a declarative language"}, 
	{c, "a procedural language"},
	{io, "a prototype language"},
	{java, "an OO language"},
	{assembler,"a machine language"}].

-spec test_keyfind_1([{atom(),string()}]) -> test_keyfind_1_worked.

test_keyfind_1(Language_list) ->
	
	Language = scala,
	
	TrueDescription = "a hybrid language",
	
	true = TrueDescription =:= keyfind(Language,Language_list),
	
	test_keyfind_1_worked.

-spec test_keyfind_2([{atom(),string()}]) -> test_keyfind_2_worked.

test_keyfind_2(Language_list) ->
	
	Language = ada,
	
	true = false =:= keyfind(Language,Language_list),
	
	test_keyfind_2_worked.

-spec test_keyfind_3([{atom(),string()}]) -> test_keyfind_3_worked.

test_keyfind_3(Language_list) ->
	
	Language = assembler,
	
	New_Language_list = [Tuple || {CurrentLanguage, _} = Tuple <- Language_list, CurrentLanguage =/= Language],
	
	%io:format("~n~p~n~n", [New_Language_list]),
	
	true = false =:= keyfind(Language,New_Language_list),
	
	test_keyfind_3_worked.

-spec test_keyfind_4([{atom(),string()}]) -> test_keyfind_4_worked.

test_keyfind_4(_Language_list) ->
	
	Language = assembler,
	
		
	true = false =:= keyfind(Language,[]),
	
	test_keyfind_4_worked.



% --------------------
-spec test_list_comprehension_1([{atom(),string()}]) -> test_list_comprehension_1_worked.

test_list_comprehension_1(Language_list) ->
	
	Tuple = {assembler, "a machine language"},
	
	Language = element(1, Tuple),
	Description = element(2, Tuple),
	
	Result = keyword(Language, Language_list),
	true = Description =:= Result,
	
	test_list_comprehension_1_worked.

-spec test_list_comprehension_2([{atom(),string()}]) -> test_list_comprehension_2_worked.

test_list_comprehension_2(_Language_list) ->
	
	Tuple = {assembler, "a machine language"},
	
	Language = element(1, Tuple),
	Description = element(2, Tuple),
	
	Result = keyword(Language, []),
	true = Description =/= Result,
	true = false =:= Result,
	
	test_list_comprehension_2_worked.

% --------------------------------
-spec test_list_filter_1([{atom(),string()}]) -> test_list_filter_1_worked.

test_list_filter_1(Language_list) ->
	
	Map = maps:from_list(Language_list),
	Language = java,
	Description = maps:get(Language, Map),
	%io:format("~n==>> ~n~p - ~p~n~n", [Language,Description]),
	Description = filter(Language, Language_list),
	
	%io:format("~n==>> ~n~p - ~p~n~n", [Language,Description]),
	%io:format("~nResult: ~p~n~n", [Result]),
	test_list_filter_1_worked.

-spec test_list_filter_2([{atom(),string()}]) -> test_list_filter_2_worked.

test_list_filter_2(_Language_list) ->
	
	false = filter(clojure, []),
	
	%io:format("~n==>> ~n~p - ~p~n~n", [Language,Description]),
	%io:format("~nResult: ~p~n~n", [Result]),
	test_list_filter_2_worked.

	
-spec test() -> test_worked.
	
test() -> 
	Language_list = language_list(),
	
	% sort list by a key
	%io:format("~n"),
	Sorted_language_list = lists:sort(Language_list),
	%io:format("~p~n~n", [Sorted_language_list]),
	L = Sorted_language_list,
	test_keyfind_1(L),
	test_keyfind_2(L),
	test_keyfind_3(L),	
	test_keyfind_4(L),	
	
	test_list_comprehension_1(L),
	test_list_comprehension_2(L),	
	
	test_list_filter_1(Sorted_language_list),
	test_list_filter_2(Sorted_language_list),
	
	test_worked.

%% Using lists:keyfind
-spec keyfind(Key :: atom(), List :: [{atom(),string()}]) -> false | string().
		
keyfind(K, L) when is_list(L) andalso length(L) > 0 andalso is_atom(K) ->	
	Result = lists:keyfind(K,1,L),
	Function_result = if 
		is_tuple(Result) ->
			{_, Value} = Result,
			Value;
		is_boolean(Result) ->
			false;
		true ->
			false
	end,
	Function_result;
keyfind(_, []) ->
	false.

%% Using list comprehension
-spec keyword(Key :: atom(), List :: [{atom(),string()}]) -> false | string().
	
keyword(K, L) when is_list(L) andalso length(L) > 0 andalso is_atom(K) -> 
	[Value] = [Value || {Key, Value} <- L, Key =:= K],
	Value;
keyword(_, []) -> 
	false.

%% Using lists:filter
-spec filter(Key :: atom(), List :: [{atom(),string()}]) -> false | string().

filter(K, L) when is_list(L) andalso length(L) > 0 andalso is_atom(K) -> 
	Result = lists:filter(fun({Key,_} = Tuple) -> Key =:= K andalso size(Tuple) =:= 2 end, L),
	%io:format("~n==>> ~n~p~n~n", [Result]),
	If_result = if 
		length(Result) =:= 0 ->
			false;
		true ->
			element(2,hd(Result))
			
	end,
	If_result;
filter(_, []) ->
	false.

	
%% commands
%
% c(item_selector).
% c(item_selector). item_selector:test().

% typer item_selector.erl
% dialyzer --no_check_plt --src item_selector.erl