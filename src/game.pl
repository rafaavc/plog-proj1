
:- consult('board.pl').

% initial(-GameState)
initial(GameState) :-
	board(GameState).

% initial player is player 0 (white pieces)
% initial_player(-Player) 
initial_player(0).

% toggle_player(+CurrentPlayer, -NextPlayer)
toggle_player(0, 1).
toggle_player(1, 0).
toggle_player(P, _) :- 
	print('Invalid current player: '),
	print(P),
	nl,
	fail.

% Updates the game state; not functional yet
% update_game_state(+CurrentGameState, -NextGameState)
update_game_state(_, NextGameState) :-
	board(NextGameState).

% The C arg is just a counter to simulate the game end (Ends when C == 3)
% In reality, the game ends when one of the players only has one piece, and the winner is the other
% game_loop(+GameState, +Player, +C)
game_loop(GameState, Player, C) :-
	display_game(GameState, Player),
	% read player's moves,
	update_game_state(GameState, NextGameState),
	toggle_player(Player, NextPlayer),
	C1 is C+1,
	(C < 3 -> game_loop(NextGameState, NextPlayer, C1); print('Game ended (simulating game loop with a counter; not changin game state, only toggling the current player).\n')).

play :-
	initial(GameState),
	initial_player(Player),
	game_loop(GameState, Player, 0).

