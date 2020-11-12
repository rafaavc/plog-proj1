
:- consult('board.pl').
:- consult('input.pl').

% initial(-GameState)
initial(game_state(white, npieces(8, 8), Board)) :- board(Board).


% toggle_player(+CurrentPlayer, -NextPlayer)
toggle_player(game_state(white, NPieces, Board), game_state(black, NPieces, Board)).
toggle_player(game_state(black, NPieces, Board), game_state(white, NPieces, Board)).
toggle_player(P, _) :- 
	print('Invalid current player: '),
	print(P),
	nl,
	fail.


% Applies a move
% apply_move(+Move, +CurrentGameState, -NextGameState)
apply_move(move(position(X1, Y1), position(X2, Y2), Piece), CurrentGameState, NextGameState) :-
	%find_piece(position(X1, Y1), Piece),
	%place_piece(position(X2, Y2), Piece)
	toggle_player(CurrentGameState, NextGameState).

get_desired_position_input(position(X1, Y1), GameBoard, DesiredOccupant):-
	get_position_input(position(XTemp, YTemp)),
	(
		(nth1(YTemp, GameBoard, YLine), nth1(XTemp, YLine, DesiredOccupant))
			-> (X1 = XTemp, Y1 = YTemp)
			; (print('The coordinates you inserted are not \''), print(DesiredOccupant), print('\'.\n'), get_desired_position_input(position(X1, Y1), GameBoard, DesiredOccupant))
	).

% get_move(-Move, +GameState)
get_move(move(Position1, Position2, Piece), game_state(CurrentPlayer, _, GameBoard)) :-
	% read player's moves
	print('Piece to move:\n'),
	get_desired_position_input(position(X1, Y1), GameBoard, CurrentPlayer),
	
	print('Desired place:\n'),
	get_desired_position_input(position(X2, Y2), GameBoard, empty),

	Position1 = position(X1, Y1), Position2 = position(X2, Y2).

% The C arg is just a counter to simulate the game end (Ends when C == 3)
% In reality, the game ends when one of the players only has one piece, and the winner is the other
% game_loop(+GameState, +Player, +C)
game_loop(GameState, C) :-
	display_game(GameState),
	get_move(Move, GameState),
	apply_move(Move, GameState, NextGameState),% (Moves the piece and sees if any piece was eaten)
	C1 is C+1,
	(C < 3 -> game_loop(NextGameState, C1); print('Game ended (simulating game loop with a counter; not changin game state, only toggling the current player).\n')).

play :-
	initial(GameState),
	game_loop(GameState, 0).

