
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


is_eaten_horizontal(piecePosition(Position, Piece), GameBoard) :-
	getPieceWithOffset(Position, 1, 0, GameBoard, RightOfPiece),
	getPieceWithOffset(Position, -1, 0, GameBoard, LeftOfPiece),
	(RightOfPiece =\= empty, RightOfPiece =\= Piece, LeftOfPiece =\= empty, RightOfPiece =\= Piece)).

is_eaten_vertical(piecePosition(Position, Piece), GameBoard) :-
	getPieceWithOffset(Position, 0, 1, GameBoard, BottomOfPiece),
	getPieceWithOffset(Position, 0, -1, GameBoard, TopOfPiece),
	(BottomOfPiece =\= empty, BottomOfPiece =\= Piece, TopOfPiece =\= empty, TopOfPiece =\= Piece).

% getPieceWithXYOffset(+PiecePosition, +Vvar, +Yvar, +GameBoard, -Piece)
getPieceWithXYOffset(position(X, Y), Xvar, Yvar, GameBoard, Piece) :-
	Y1 is Y+Yvar,
	Y1 > 0,
	Y2 <= 9,
	nth1(Y1, GameBoard, Line),
	X1 is X+Xvar,
	X1 > 0,
	X1 <= 9,
	nth1(X1, Line, Piece).

% Checks if a piece is eaten
% is_eaten(+PiecePosition, +GameBoard)
is_eaten(PiecePosition, GameBoard) :- is_eaten_horizontal(PiecePosition, GameBoard).
is_eaten(PiecePosition, GameBoard) :- is_eaten_vertical(PiecePosition, GameBoard).

% Adds pieces to list containing pieces to remove from the board
% add_pieces_to_remove(+Pieces, -PiecesToRemoveT)
add_pieces_to_remove([], []).
add_pieces_to_remove([Hp|Tp], [H|T]) :-
	(
		is_eaten(Hp), ground(Hp) -> (H is Hp, add_pieces_to_remove(Tp, T));
		add_pieces_to_remove(Tp, [H|T])
	).

%get_pieces_diff(+Move, +GameState, -PiecesToAdd, -PiecesToRemove)
get_pieces_diff(move(MoveStartPosition, MoveEndPosition, Piece), game_state(_, _, GameBoard), [PiecesToAddH|PiecesToAddT], [PiecesToRemoveH|PiecesToRemoveT]) :-
	PiecesToAddH is piecePosition(MoveEndPosition, Piece),
	PiecesToRemoveH is position(MoveStartPosition),

	(getPieceWithOffset(Position, 1, 0, GameBoard, RightOfPiece);true),
	(getPieceWithOffset(Position, -1, 0, GameBoard, LeftOfPiece);true),
	(getPieceWithOffset(Position, 0, 1, GameBoard, BottomOfPiece);true),
	(getPieceWithOffset(Position, 0, -1, GameBoard, TopOfPiece);true),
	
	add_pieces_to_remove([RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], PiecesToRemoveT);
.

% Applies a move
% apply_move(+Move, +CurrentGameState, -NextGameState) 
apply_move(Move, game_state(CurrentPlayer, CurrentNPieces, CurrentBoard), game_state(NextPlayer, CurrentNPieces, NextBoard)) :-
	move_piece(Move, CurrentBoard, NextBoard),
	toggle_player(CurrentPlayer, NextPlayer).

% Gets a position with the desired type of piece
%get_desired_position_input(-Position, +GameBoard, +DesiredOccupant)
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
	get_pieces_diff(Move, GameState, PiecesToAdd, PiecesToRemove),
	apply_move(PiecesToAdd, PiecesToRemove, GameState, NextGameState),% (Moves the piece and sees if any piece was eaten)
	C1 is C+1,
	(C < 3 -> game_loop(NextGameState, C1); print('Game ended (simulating game loop with a counter; not changin game state, only toggling the current player).\n')).

play :-
	initial(GameState),
	game_loop(GameState, 0).

