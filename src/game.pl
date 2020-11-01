
:- consult('board.pl').


initial(GameState) :-
	board(GameState).

initial_player(Player) :-
	Player is 0.

toggle_player(CurrentPlayer, Out) :-
	(CurrentPlayer == 0 -> Out is 1; Out is 0).

get_player(CurrentPlayer, Out) :-
	(ground(CurrentPlayer) -> toggle_player(CurrentPlayer, Out) ; initial_player(Out)).

% Updates the game state; not functional yet
update_game_state(_, NextGameState) :-
	board(NextGameState).

game_loop(GameState, Player) :-
	display_game(GameState, Player),
	% read player's moves,
	update_game_state(GameState, NextGameState),
	get_player(Player, NextPlayer),
	game_loop(NextGameState, NextPlayer).

play :-
	initial(GameState),
	get_player(_, NextPlayer),
	game_loop(GameState, NextPlayer).



