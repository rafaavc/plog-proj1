
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

updateBoardColumn(Index, NewElement, Row, NewRow) :-
	nth1(Index, Row, _, Rest),
	nth1(Index, NewRow, NewElement, Rest).

updateBoardRow(Index, NewRow, Board, NewBoard) :-
	nth1(Index, Board, _, Rest),
	nth1(Index, NewBoard, NewRow, Rest).

%
add_pieces([], NextBoard, NextBoard).
add_pieces([piecePosition(position(X, Y), Piece)|PiecesT], CurrentBoard, TempBoard) :-
	nth1(Y, CurrentBoard, Row),
	updateBoardColumn(X, Piece, Row, NewRow),
	updateBoardRow(Y, NewRow, CurrentBoard, TempBoard),
	add_pieces(PiecesT, TempBoard, TempBoard).

%
remove_pieces([], NextBoard, NextBoard).
remove_pieces([piecePosition(position(X, Y), _)|PiecesT], CurrentBoard, NextBoard) :-
	nth1(Y, CurrentBoard, Row),
	updateBoardColumn(X, empty, Row, NewRow),
	updateBoardRow(Y, NewRow, CurrentBoard, TempBoard),
	remove_pieces(PiecesT, TempBoard, NextBoard).

% Applies a move
% apply_move(+PiecesToRemove, +CurrentGameState, -NextGameState)
apply_move(PiecesToRemove, game_state(CurrentPlayer, CurrentNPieces, CurrentBoard), game_state(NextPlayer, CurrentNPieces, NextBoard)) :-
	remove_pieces(PiecesToRemove, CurrentBoard, NextBoard),
	toggle_player(CurrentPlayer, NextPlayer).

% getPieceWithXYOffset(+PiecePosition, +Vvar, +Yvar, +GameBoard, -Piece)
getPieceWithXYOffset(position(X, Y), Xvar, Yvar, GameBoard, piecePosition(position(X1, Y1), Piece)) :-
	Y1 is Y+Yvar,
	Y1 > 0,
	Y1 =< 9,
	nth1(Y1, GameBoard, Line),
	X1 is X+Xvar,
	X1 > 0,
	X1 =< 9,
	nth1(X1, Line, Piece).

% Checks if a piece is eaten
% is_eaten(+PiecePosition, +GameBoard)
is_eaten(PiecePosition, GameBoard) :- is_eaten_horizontal(PiecePosition, GameBoard).
is_eaten(PiecePosition, GameBoard) :- is_eaten_vertical(PiecePosition, GameBoard).

is_eaten_horizontal(piecePosition(Position, Piece), GameBoard) :-
	getPieceWithXYOffset(Position, 1, 0, GameBoard, piecePosition(_, RightOfPiece)),
	getPieceWithXYOffset(Position, -1, 0, GameBoard, piecePosition(_, LeftOfPiece)),
	(RightOfPiece \== empty, RightOfPiece \== Piece, LeftOfPiece \== empty, RightOfPiece \== Piece).

is_eaten_vertical(piecePosition(Position, Piece), GameBoard) :-
	getPieceWithXYOffset(Position, 0, 1, GameBoard, piecePosition(_, BottomOfPiece)),
	getPieceWithXYOffset(Position, 0, -1, GameBoard, piecePosition(_, TopOfPiece)),
	(BottomOfPiece \== empty, BottomOfPiece \== Piece, TopOfPiece \== empty, TopOfPiece \== Piece).

% Adds pieces to list containing pieces to remove from the board
% add_pieces_to_remove(+GameBoard, +Pieces, +Remove, -PiecesToRemove)
add_pieces_to_remove(_, [], List, List).
add_pieces_to_remove(GameBoard, [Hp|Tp], List, PiecesToRemove) :-
	(
		is_eaten(Hp, GameBoard), ground(Hp) -> (add_pieces_to_remove(GameBoard, Tp, [Hp|List], PiecesToRemove));
		(add_pieces_to_remove(GameBoard, Tp, List, PiecesToRemove))
	).

%get_pieces_diff(+Move, +GameState, -PiecesToAdd, -PiecesToRemove, -TempGameState)
get_pieces_diff(move(MoveStartPosition, MoveEndPosition, Piece), game_state(Player, CurrentPieces, GameBoard), PiecesToRemove, game_state(Player, CurrentPieces, TempBoard)) :-

	%apply user move
	add_pieces([piecePosition(MoveEndPosition, Piece)], GameBoard, AddBoard),
	remove_pieces([piecePosition(MoveStartPosition, Piece)], AddBoard, TempBoard),

	(getPieceWithXYOffset(MoveEndPosition, 1, 0, TempBoard, RightOfPiece);true),
	(getPieceWithXYOffset(MoveEndPosition, -1, 0, TempBoard, LeftOfPiece);true),
	(getPieceWithXYOffset(MoveEndPosition, 0, 1, TempBoard, BottomOfPiece);true),
	(getPieceWithXYOffset(MoveEndPosition, 0, -1, TempBoard, TopOfPiece);true),

	Remove = [],
	add_pieces_to_remove(TempBoard, [RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], Remove, PiecesToRemove).

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
	get_pieces_diff(Move, GameState, PiecesToRemove, TempGameState),
	apply_move(PiecesToRemove, TempGameState, NextGameState),
	C1 is C+1,
	(C < 3 -> game_loop(NextGameState, C1); print('Game ended (simulating game loop with a counter; not changin game state, only toggling the current player).\n')).

play :-
	initial(GameState),
	game_loop(GameState, 0).

