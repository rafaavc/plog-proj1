
:- consult('board.pl').

% initial(-GameState)
initial(GameState) :-
	board(GameState).

initial_player(Player) :-
	Player is 0.

toggle_player(CurrentPlayer, Out) :-
	(CurrentPlayer == 0 -> Out is 1; Out is 0).

get_player(CurrentPlayer, Out) :-
	(ground(CurrentPlayer) -> toggle_player(CurrentPlayer, Out) ; initial_player(Out)).

% Updates the game state; not functional yet
% update_game_state(+CurrentGameState, -NextGameState)
update_game_state(_, NextGameState) :-
	board(NextGameState).

% The C arg is just a counter to simulate the game end (Ends when C == 3)
game_loop(GameState, Player, C) :-
	display_game(GameState, Player),
	% read player's moves,
	update_game_state(GameState, NextGameState),
	get_player(Player, NextPlayer),
	C1 is C+1,
	(C1 < 3 -> game_loop(NextGameState, NextPlayer, C1); print('Game ended (simulating game loop with a counter).\n')).

play :-
	initial(GameState),
	get_player(_, NextPlayer),
	game_loop(GameState, NextPlayer, 0).



