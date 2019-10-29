%% @author Anatolii Kosorukov <java1cprog@yandex.ru>
%% @copyright 2019 by rustkas
%% @version 1.0

-module(tic_tac_toe_game).

-export([test/0]).

-export([start/0]).

-include("players.hrl").

-define(UNDEFINED, '-').
-define(NO_WIN, 'no_win').
-define(GO, 'go').

-spec test_check_winner_state_1() -> 'test_worked'.

test_check_winner_state_1() ->
	Board = {
	{x,x,x},
	{o,o,?UNDEFINED},
	{o,?UNDEFINED,x}
	},
	
	Result = check_winner_state(Board,x),
	Result = win,
	%io:format("Winner state: ~p~n",[Result]),
	test_worked.


-spec test_print_no_win_1() -> 'test_worked'.

test_print_no_win_1()->
	X_player = #player{name=x, move=5},
	Y_player = #player{name=o, move=4},
	Players = {X_player,Y_player},
	Board = create_game_board(),	
	print_no_win(Players,Board),
	
	test_worked.

-spec test_check_game_state_1() -> 'test_worked'.

test_check_game_state_1() ->
	Board = create_game_board(),	
	?GO = check_game_state(Board),

	test_worked.

-spec test_check_game_state_2() -> 'test_worked'.

test_check_game_state_2() ->
	Board = {{x, o, x},
			 {x, o, o},
			 {o, x, x}},
	?NO_WIN = check_game_state(Board),

	test_worked.

-spec test_to_string_1() -> 'test_worked'.

test_to_string_1() ->
	Board = create_game_board(),
	Result = to_string(Board),
	true = 9 =:= length(Result),
	Result = "         ",
	
	test_worked.

-spec test_to_string_2() -> 'test_worked'.

test_to_string_2() ->
	Board = {{x, o, x},
			 {x, o, o},
			 {o, x, x}},
	Result = to_string(Board),			 
	true = 9 =:= length(Result),
	Result = "xoxxoooxx",
	test_worked.

-spec test() -> 'test_worked'.
	
test() ->

	test_check_winner_state_1(),

	
	test_check_game_state_1(),
	test_check_game_state_2(),
	test_print_no_win_1(),

	test_to_string_1(),
	test_to_string_2(),
	
	test_worked.

-spec to_string(Board :: tuple()) -> string().

to_string(Board) ->
	Row1 = element(1,Board),
	Row2 = element(2,Board),
	Row3 = element(3,Board),
	
	Result = to_string(Row1, 1) ++ 
	to_string(Row2, 1) ++
	to_string(Row3, 1),
	
	%io:format("TO STRING: ~p~n",[Result]),
	Result.

-spec to_string(tuple(),1 | 2 | 3 | 4) -> string().

to_string(Row, N) when N =< 3 ->
	Result = case element(N,Row) of
		?UNDEFINED ->
			" ";
		x -> "x";
		o -> "o"
		end,
		Result ++ to_string(Row,N+1);
to_string(_Row, 4) -> "".
		
		
-spec create_game_board() -> {{'-','-','-'},{'-','-','-'},{'-','-','-'}}.

create_game_board() ->
	Board = {{?UNDEFINED, ?UNDEFINED, ?UNDEFINED},
				  {?UNDEFINED, ?UNDEFINED, ?UNDEFINED},
				  {?UNDEFINED, ?UNDEFINED, ?UNDEFINED}},
	Board.
	
%% Start the game.
-spec start() -> 'ok'.
	
start() ->

	io:format("~nThis Tic Tac Toe game (3x3)~n"),
	io:format("---------------------------~n"),
	io:format("It is 2 player game.~n"),
	io:format("One player is 'x'-player, second is 'o'-player.~n"),
	io:format("Each player enters location \x{28}row column\x{29} and press Enter.~n"),
	io:format("To stop the game, enter the word \"stop\".~n"),
	io:format("This is a play board: ~n"),
	
	Board = create_game_board(),
	
	print(Board),
	io:format("Let's start! ~n"),
	
	X_player = #player{name=x, move=0},
	Y_player = #player{name=o, move=0},
	game({X_player,Y_player},1,Board,?GO),
	ok.

%-spec game({{'plyer',{'name',x}},{'plyer',{'name',o}}},1 | 2, tuple(),?GO) -> no_return()|stop.

-spec player_info(1 | 2,Players :: tuple()) -> {Name :: atom(),Moves :: non_neg_integer()}.

player_info(Id,Players) ->
	Player = element(Id,Players),
	#player{name=Name,move=Moves} = Player,
	{Name, Moves}.

-spec game(tuple(),Id :: 1 | 2,Board :: tuple(),'go' | 'stop' | [pos_integer(),...]) -> 'ok' | 'stop'.

game(Players,Id,Board,?GO)->
		{Name, _Moves} = player_info(Id,Players),
		Title = io_lib:format("~s-player enter>",[Name]),
		
		
		try io:fread(Title, "~u ~u") of
			{ok, Board_location} -> 
					%io:format("~p~n",[Board_location]),
					% check input data
					[Row, Column] = Board_location,
				try if 
						not is_integer(Row) ->
							error({rowIsNotAnInteger, Row});
						not is_integer(Column) ->
							error({columnIsNotAnInteger, Column});
						(Row < 1) or (Row > 3) ->
							error({rowIsNotValidInteger, Row});
						(Column < 1) or (Column > 3) ->
							error({columnIsNotValidInteger, Column});
						true ->
							ok
						end
					
				catch
					
					error:{rowIsNotAnInteger, X} ->
						io:fwrite("Row value ~p isn't an integer.~n",[X]),
						io:fwrite("Please, enter the integer!~n"),
						game(Players, Id, Board, ?GO);
					error:{columnIsNotAnInteger, X} ->
						io:fwrite("Column value ~p isn't an integer.~n",[X]),
						io:fwrite("Please, enter the integer!~n"),
						game(Players, Id, Board, ?GO);
					error:{rowIsNotValidInteger, X} ->
						io:fwrite("Row value ~p is invalid integer value.~n",[X]),
						io:fwrite("Please, enter a valid one ( > 0 and =< 3)!~n"),
						game(Players, Id, Board, ?GO);
					error:{columnIsNotValidInteger, X} ->
						io:fwrite("Column value ~p is invalid integer value.~n",[X]),
						io:fwrite("Please, enter a valid one ( > 0 and =< 3)!~n"),
						game(Players, Id, Board, ?GO);
					error:X -> 
						io:fwrite("You write ~p~n",[X]),
						io:fwrite("Please, enter two valid ids!~n"),
						game(Players, Id, Board, ?GO)
					end,
					
					game(Players,Id, Board, Board_location);
		{error, {fread, _X}} ->
			print_no_win(Players,Board)
		catch
			throw -> 
				%{N, caught, thrown, X};
				game(Players, Id, Board, ?GO);
			exit -> stop;
			error:{error,{fread,unsigned}} ->
				print_no_win(Players,Board);
			error:_ -> 
				print_no_win(Players,Board);
			_ -> stop
		end,

		
		% function return 
		
		stop;		

game(Players,Id, Board,Position) when is_list(Position) ->
	{Name, Moves} = player_info(Id,Players),
	{Modified_board, Check_board_result} = check_board_state(Board,Position,Name),
	case Check_board_result of 
			
		try_again ->
			io:fwrite("The selected cell has already been used in the game.~n"),
			io:fwrite("Please select a different cell.~n"),
				
			game(Players,Id, Board, ?GO);
		
		?GO ->
			Modified_moves = Moves + 1,
							
			% check current game state
			Game_state_result = check_winner_state(Modified_board,Name),
			case Game_state_result of 
				win ->
					print_win_info(Name, Modified_moves,Board),
					game(Players,Id, Board, stop);
				?GO -> 
					New_id = case Id of
								1 -> 2;
								_ -> 1
							end,
					Player = element(Id,Players),		
					Modified_player = Player#player{move = Modified_moves},
					Modified_players = erlang:insert_element(Id, erlang:delete_element(Id, Players), Modified_player),
					game(Modified_players, New_id, Modified_board, ?GO)
			end;
		?NO_WIN ->
			print_no_win(Players,Board)
	end;		
		
game(_Tuple,_Id, _List_board, stop) -> 
		stop.		

-spec print_game_board(Board :: tuple()) -> 'ok'.

print_game_board(Board) ->
	Result = to_string(Board),
	Statistics = tic_tac_toe:statisctics(Result),
	io:fwrite("Game result id: ~s~n",[Statistics]),
	io:fwrite("Game board: ~s~n",[Result]).

-spec print_no_win(Players :: tuple(), Board :: tuple()) -> 'ok'.

print_no_win(Players,Board)->
	io:fwrite("Nobody won.~n"),
	Fun = fun(X)-> 
			#player{name=Current_name, move=Current_moves}=X, 
			io:format("~p has ~p moves.~n",[Current_name, Current_moves]) end,
	lists:keymap(Fun, 1, [Players]),
	lists:keymap(Fun, 2, [Players]),
	print_game_board(Board),	
	io:fwrite("Good luck in future games!~n"),
	ok.

-spec print_win_info(Name :: 'o' | 'x', Moves :: non_neg_integer(),Board :: tuple()) -> 'ok'.

print_win_info(Name,Moves,Board) ->
	io:format("\"~s\" won in ~p moves.~n",[Name, Moves]),
	print_game_board(Board),
	ok.

-spec check_winner_state(Board  :: tuple(),Name :: atom())-> Result_state :: ?GO | 'win'.

check_winner_state(Board,N) ->
	Win = case Board of
						{{N,N,N},
						{_,_,_},
						{_,_,_}} -> win;
						{{_,_,_},
						{N,N,N},
						{_,_,_}} -> win;
						{{_,_,_},
						{_,_,_},
						{N,N,N}} -> win;
						{{N,_,_},
						{_,N,_},
						{_,_,N}} -> win;
						{{_,_,N},
						 {_,N,_},
						 {N,_,_}} -> win;
						{{N,_,_},
						 {N,_,_},
						 {N,_,_}} -> win;
					    {{_,N,_},
						 {_,N,_},
						 {_,N,_}} -> win;
						{{_,_,N},
						 {_,_,N},
						 {_,_,N}} -> win;
						_            -> ?GO
						
				end,
	Win.

-spec check_board_state(Board :: tuple(),Position :: [pos_integer()],x|o) -> Result_state :: {tuple(),?GO | 'try_again' | ?NO_WIN}.

check_board_state(Board, Position,Value) ->
	[Row, Column] = Position,
	Selected_row = element(Row,Board), 
	
	% get current value
	Current_value = element(Column,element(Row,Board)), 
	Result = case Current_value of
		?UNDEFINED ->
			
			%  modify current cell
			Modified_row = erlang:insert_element(Column, erlang:delete_element(Column, Selected_row), Value),
			%io:format("Modified_row: ~p | Board: ~p~n",[Modified_row, Board]),
			Modified_board =  erlang:insert_element(Row, erlang:delete_element(Row, Board), Modified_row),
			%io:format("Modified_board: ~p ~n",[Modified_board]),
			print(Modified_board),
			
			Game_state = check_game_state(Modified_board),
			
			case Game_state of
				?GO ->
					{Modified_board, ?GO};
				?NO_WIN ->
					{Modified_board, ?NO_WIN}
			end;
		_ ->
			% try again
			{Board, try_again}
		end,
	Result.

-spec check_game_state(tuple()) -> 'go' | 'no_win'.
	
check_game_state(Board) ->
	List = [element(1, Board), element(2, Board), element(3, Board)],
	Size = size(element(1, Board)),
	check_game_state(Size,List).

-spec check_game_state(non_neg_integer(),[any(),...]) -> 'go' | 'no_win'.
	
check_game_state(Id, List) when Id > 0 ->
	Is_keymember = lists:keymember(?UNDEFINED, Id, List),
	
	Result = if
			Is_keymember -> ?GO;
		    true -> check_game_state(Id - 1, List)
	end,
	Result;
check_game_state(0, _Board) ->
	?NO_WIN.		

-spec print(Board :: tuple()) -> [].
		
print(Board) ->
	print(1,Board).

-spec print(1 | 2 | 3 | 4,tuple()) -> [].

print(N,Board) when N =< 3 ->
	Tuple = element(N,Board),
	{A1,A2,A3} = Tuple,
	io:format("~s\t ~s\t ~s ~n",[A1,A2,A3]),
	print(N+1,Board);
print(4,_Board)-> [].	
	
%% commands
%
% c(tic_tac_toe_game).
% c(tic_tac_toe_game). tic_tac_toe_game:test().
% c(tic_tac_toe_game). tic_tac_toe_game:start().

% typer tic_tac_toe_game.erl
% dialyzer --no_check_plt --src -pa ..\tic_tac_toe tic_tac_toe_game.erl ..\tic_tac_toe\tic_tac_toe.erl
%