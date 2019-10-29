%% @author Anatolii Kosorukov <java1cprog@yandex.ru>
%% @copyright 2019 by rustkas
%% @version 1.0

-module(tic_tac_toe).
-export([test/0]).
-export([statisctics/1,string_to_list/1,result/1]).

-define(EMPTY, " ").
-define(X, x).
-define(O, o).
-define(NO_WINNER, no_winner).
-define(CAT, cat). 

-spec make_list(String :: string(), List :: [] | [string()]) -> [Result :: string()].

make_list(String,L) when length(String) > 0 ->
	List_item = string:slice(String, 0, 1),
	Char = list_to_atom(List_item),
	Rest = string:slice(String, 1, length(String)),
	
	TrimmedChar = string:trim(List_item),
	NewList = if 
		length(TrimmedChar) =:= 0 ->
			make_list(Rest,[?EMPTY|L]);
		true ->
			make_list(Rest,[Char|L])
	end,		
	%io:format("~p <<< ~p || ~p || ~p ~n",[String, Char, Rest,L]),
	NewList;
make_list("",L)->	
	L.

-spec string_to_list(String :: string()) -> [Result :: string()].

string_to_list(String) ->
	List = lists:reverse(make_list(String,[])),
	List.

%% No winner	
-spec test0() -> 'test_worked'.

test0() ->
	String = 
		" xx" ++
		"oxo" ++
		"oxo",
	
	List = string_to_list(String),		
	%io:format("~p~n",[List]),
	
	Result = result(List),
	%io:format("~p~n",[Result]),
	true = ?NO_WINNER =:= Result,
	
	test_worked.

%% x is winner
-spec test1() -> 'test_worked'.

test1() ->
	Strings = 
	[
	io_lib:format("~s~s~s~s~s~s~s~s~s",
	[?X, ?X, ?X, 
	 ?O, ?X, ?O,
	 ?O, ?X, ?O]),
	
	io_lib:format("~s~s~s~s~s~s~s~s~s",
	[?O, ?X, ?O, 
	 ?X, ?X, ?X,
	 ?O, ?X, ?O]),
	
	io_lib:format("~s~s~s~s~s~s~s~s~s",
	[?O, ?X, ?O, 
	 ?O, ?X, ?O,
	 ?X, ?X, ?X]),
	
	io_lib:format("~s~s~s~s~s~s~s~s~s",
	[?X, ?O, ?O, 
	 ?X, ?X, ?X,
	 ?X, ?O, ?O]),
	
	io_lib:format("~s~s~s~s~s~s~s~s~s",
	[?O, ?O, ?X, 
	 ?X, ?X, ?X,
	 ?O, ?O, ?X]),
	
	io_lib:format("~s~s~s~s~s~s~s~s~s",
	[?O, ?O, ?X, 
	 ?O, ?X, ?O,
	 ?X, ?O, ?O]),
	
	io_lib:format("~s~s~s~s~s~s~s~s~s",
	[?X, ?O, ?O, 
	 ?O, ?X, ?O,
	 ?O, ?O, ?X])
	],
	%List = string_to_list(String),		
	%io:format("~p~n",[List]),
	%Result = result(List),
	%io:format("~p~n",[Result]),
	Check_compinations = fun(S)-> 
		List = string_to_list(S),
		Result = result(List),
		%io:format("~p | ~p~n",[List, Result]),
		true = x =:= Result
	end,
	true = lists:all(Check_compinations, Strings),
	%true = x =:= Result,
	
	test_worked.
	
%% They played a draw.
-spec test2() -> 'test_worked'.

test2() ->
	String = 
		"xox" ++
		"xxo" ++
		"oxo",

	List = string_to_list(String),
	Result = result(List),
	true = ?CAT =:= Result,
	
	test_worked.

test3() ->
	String = 
		"   " ++
		"   " ++
		"   ",
	
	List = string_to_list(String),		
	%io:format("~p~n",[List]),
	
	Result = result(List),
	%io:format("~p~n",[Result]),
	true = ?NO_WINNER =:= Result,
	
	test_worked.

-spec test() -> 'test_worked'.
	
test() ->
	
	test0(),
	test1(),
	test2(),
	test3(),
	
	test_worked.

%% Return statistics information
-spec statisctics(InputString :: string()) -> Result :: ?X|?O|?NO_WINNER|?NO_WINNER|?CAT.

statisctics(String) when is_list(String), length(String) =:=9 ->
	List = string_to_list(String),		
	Result = result(List),
	Result.
	
%% function check input values.
%% The valid values of an input item are "", x, o.
-spec checkInput(_) -> boolean().
	
checkInput(X) -> 
	Result = if 
				X =:= ?EMPTY; X =:= ?X; X =:= ?O  ->
												true;
										true -> false
			end,
	Result.

	
-spec result(Board :: [string()]) -> Result :: ?X|?O|?NO_WINNER|?NO_WINNER|?CAT.

result(Board) when is_list(Board) andalso length(Board) =:= 9 ->
	%io:format("~p~n",[Board]),
	true = lists:all(fun checkInput/1, Board),
	
	Has_empty_cell = lists:any(fun(X) -> X =:= ?EMPTY end, Board),
	
	
	Result = if 
		% Is one of cells empty?
		Has_empty_cell ->
			?NO_WINNER;
		true -> 
			% Do we have a winning combination?

			Have_win = case Board of
						[M,M,M | _]  -> M;
						[_,_,_,
						 M,M,M | _]  -> M;
						[_,_,_,
						_,_,_,
						M,M,M]       -> M;
						[M,_,_,
						_,M,_,
						_,_,M]       -> M;
						[_,_,M,
						_,M,_,
						M,_,_]       -> M;
					   [M,_,_,
						M,_,_,
						M,_,_]       -> M;
						[_,M,_,
						_, M,_,
						_, M,_]      -> M;
						[_,_,M,
						_,_, M,
						_,_, M]      -> M;
						
						_            -> ?CAT
						
						end,
			Have_win			
	end,	
	Result.

%% commands
%
% c(tic_tac_toe).
% c(tic_tac_toe). tic_tac_toe:test().

% typer tic_tac_toe.erl
% dialyzer --no_check_plt --src tic_tac_toe.erl
%