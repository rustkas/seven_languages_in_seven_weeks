%% @author Anatolii Kosorukov <java1cprog@yandex.ru>
%% @copyright 2019 by rustkas
%% @version 1.0

-module(total_price).
-export([test/0]).
-export([total_price/1]).

-spec test_total_price_1([]) -> 'test_worked'.

test_total_price_1(Items) when length(Items) =:= 0 ->
	false = total_price(Items),
	test_worked.

-spec test_total_price_2([{atom(),integer(), float()}]) -> 'test_worked'.

test_total_price_2(Items) when length(Items) > 0 ->
	List = total_price(Items),
	true = length(Items) =:= length(List),
	
	%io:format("~p~n",[List]),
	true = lists:all(fun(T)-> is_tuple(T) andalso size(T) =:=2 end, List),
	
	test_worked.

-spec test_total_sum_1([]) -> 'test_worked'.
	
test_total_sum_1(Items) when length(Items) =:= 0 ->
	Value = total_sum(Items),	
	true = is_float(Value),
	true = 0.0 =:= Value,
	
	test_worked.

-spec test_total_sum_2([{atom(),integer(), float()}]) -> 'test_worked'.

test_total_sum_2(Items) when length(Items) > 0 ->
	Value = total_sum(Items),	
	%io:format("~p~n",[Value]),
	true = is_float(Value),
	true = 101.0 =:= Value,
	
	test_worked.

-spec test() -> test_worked.
	
test() -> 
	% setup encoding
	io:setopts([{encoding, unicode}]),
	
	Items = price_info(),
	
	test_total_price_1([]),
	test_total_price_2(Items),
	
	test_total_sum_1([]),
	test_total_sum_2(Items),
	
	%io:format("~p~n",[Items]),
	String = "Привет",
	io:format("~ts~n",[String]),
	
	%Total_price = element(2, hd(Items)),
	%true = is_float(Total_price),
	%io:format("~ts~n",[String]),
	
	%Bin = unicode:characters_to_binary(String),
	%io:format("~ts~n", [Bin]),
	
	test_worked.

price_info() ->
	Info = [
	{book,1,70.7},
	{table,1,20.2},
	{screwdriver,1,10.1}
	],
	
	Info.

-spec total_price([{atom(),integer(), float()}]) -> 'false' | [{atom(),float()}].

total_price(Items) when is_list(Items) andalso length(Items) > 0 ->
	Price_info = [{Product,Count * Price} || {Product,Count, Price} <- Items],
	Price_info;
total_price([]) -> false.	

-spec total_sum([{atom(),integer(), float()}]) -> float().


total_sum(Items) when is_list(Items) andalso length(Items) > 0 ->
	%io:format("~p~n",[Items]),
	Price_info = total_price(Items),
	%io:format("==>>> ~p~n",[Price_info]),
	
	
	Total_sum = lists:foldl(fun({_, Product_sum}, Sum) -> Product_sum + Sum end, 0, Price_info),
	Total_sum;
total_sum([])  -> 0.0.	
	
	
%% commands
%
% c(total_price).
% c(total_price). total_price:test().

% typer total_price.erl
% dialyzer --no_check_plt --src total_price.erl
%