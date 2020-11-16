
:- consult('board.pl').
:- consult('input.pl').

% initial(-GameState)
initial(game_state(white, npieces(8, 8), Board)) :- board(Board).


% toggle_player(+CurrentPlayer, -NextPlayer)
toggle_player(white, black).
toggle_player(black, white).
toggle_player(P, _) :- 
	print('Invalid current player: '),
	print(P),
	nl,
	fail.

updateBoardColumn(OldH, NewH, move(position(X1, Y1), position(X2, Y2), Piece), ColumnNumber, LineNumber) :-
	(X1 =:= ColumnNumber, Y1 =:= LineNumber -> NewH = empty;
		(X2 =:= ColumnNumber, Y2 =:= LineNumber
			-> NewH = Piece
			; NewH = OldH
		)
	).

iterateBoardColumn([OldH|[]], [NewH|_], Move, ColumnNumber, LineNumber) :-
	updateBoardColumn(OldH, NewH, Move, ColumnNumber, LineNumber).

iterateBoardColumn([OldH|OldT], [NewH|NewT], Move, ColumnNumber, LineNumber) :-
	updateBoardColumn(OldH, NewH, Move, ColumnNumber, LineNumber),
	NextColumnNumber is ColumnNumber + 1,
	iterateBoardColumn(OldT, NewT, Move, NextColumnNumber, LineNumber).


updateBoardLine(OldH, NewH, move(position(X1, Y1), position(X2, Y2), Piece), RowNumber) :-
	(
		Y1 \= RowNumber, Y2 \= RowNumber 
			-> NewH = OldH
			; iterateBoardColumn(OldH, NewH, move(position(X1, Y1), position(X2, Y2), Piece), 1, RowNumber)
	).

iterateBoardLine([OldH|[]], [NewH|_], Move, RowNumber) :-
	updateBoardLine(OldH, NewH, Move, RowNumber).

iterateBoardLine([OldH|OldT], [NewH|NewT], Move, RowNumber) :-
	updateBoardLine(OldH, NewH, Move, RowNumber),
	NextRowNumber is RowNumber + 1,
	iterateBoardLine(OldT, NewT, Move, NextRowNumber).


move_piece(Move, CurrentBoard, NextBoard) :-
	iterateBoardLine(CurrentBoard, NextBoard, Move, 1).


% Applies a move
% apply_move(+Move, +CurrentGameState, -NextGameState) 
apply_move(Move, game_state(CurrentPlayer, CurrentNPieces, CurrentBoard), game_state(NextPlayer, CurrentNPieces, NextBoard)) :-
	%find_piece(position(X1, Y1), Piece, CurrentGameState, IntermediateGameState),
	%place_piece(position(X2, Y2), Piece, IntermediateGameState, NextGameState),
	move_piece(Move, CurrentBoard, NextBoard),
	toggle_player(CurrentPlayer, NextPlayer).

get_desired_position_input(position(X1, Y1), GameBoard, DesiredOccupant):-
	get_position_input(position(XTemp, YTemp)),
	(
		(nth1(YTemp, GameBoard, YLine), nth1(XTemp, YLine, DesiredOccupant))
			-> (X1 = XTemp, Y1 = YTemp)
			; (print('The coordinates you inserted are not \''), print(DesiredOccupant), print('\'.\n'), get_desired_position_input(position(X1, Y1), GameBoard, DesiredOccupant))
	).

verify_orthogonal(position(X1, Y1), position(X2, Y2), GameBoard) :-
	get_desired_position_input(position(TempX2, TempY2), GameBoard, empty),
	(
		(X1 \= TempX2, Y1 \= TempY2)
			-> print('Not orthogonal\n'), verify_orthogonal(position(X1, Y1), position(X2, Y2), GameBoard);
			X2 is TempX2, Y2 is TempY2
	).

% get_move(-Move, +GameState)
get_move(move(Position1, Position2, Piece), game_state(CurrentPlayer, _, GameBoard)) :-
	% read player's moves
	print('Piece to move:\n'),
	get_desired_position_input(position(X1, Y1), GameBoard, CurrentPlayer),
	
	print('Desired place:\n'),
	verify_orthogonal(position(X1, Y1), position(X2, Y2), GameBoard),

	Position1 = position(X1, Y1), Position2 = position(X2, Y2), Piece = CurrentPlayer.

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

