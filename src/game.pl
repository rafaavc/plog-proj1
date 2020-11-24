
:- consult('board.pl').
:- consult('input.pl').

% initial(-GameState)
initial(game_state(white, npieces(8, 8), Board)) :- board(Board).

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

% apply user move
% apply_move(+move, +GameState, -TempGameState)
apply_move(move(MoveStartPosition, MoveEndPosition, Piece), game_state(Player, CurrentPieces, GameBoard), game_state(Player, CurrentPieces, TempBoard)):-
	add_pieces([piecePosition(MoveEndPosition, Piece)], GameBoard, AddBoard),
	remove_pieces([piecePosition(MoveStartPosition, Piece)], AddBoard, TempBoard).

% removes eaten pieces
% apply_move(+PiecesToRemove, +CurrentGameState, -NextGameState)
apply_pieces_to_remove(PiecesToRemove, game_state(CurrentPlayer, CurrentNPieces, CurrentBoard), game_state(CurrentPlayer, CurrentNPieces, NextBoard)) :-
	remove_pieces(PiecesToRemove, CurrentBoard, NextBoard).

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
	Piece \= empty, Piece \= mountain, Piece \= dragonCave,
	getPieceWithXYOffset(Position, 1, 0, GameBoard, piecePosition(_, RightOfPiece)),
	getPieceWithXYOffset(Position, -1, 0, GameBoard, piecePosition(_, LeftOfPiece)),
	(RightOfPiece \= empty, RightOfPiece \= Piece, LeftOfPiece \= empty, RightOfPiece \= Piece).

is_eaten_vertical(piecePosition(Position, Piece), GameBoard) :-
	Piece \= empty, Piece \= mountain, Piece \= dragonCave,
	getPieceWithXYOffset(Position, 0, 1, GameBoard, piecePosition(_, BottomOfPiece)),
	getPieceWithXYOffset(Position, 0, -1, GameBoard, piecePosition(_, TopOfPiece)),
	(BottomOfPiece \= empty, BottomOfPiece \= Piece, TopOfPiece \= empty, TopOfPiece \= Piece).

% Adds pieces to list containing pieces to remove from the board
% add_pieces_to_remove(+GameBoard, +Pieces, +Remove, -PiecesToRemove)
add_pieces_to_remove(_, [], List, List).
add_pieces_to_remove(GameBoard, [Hp|Tp], List, PiecesToRemove) :-
	(
		is_eaten(Hp, GameBoard), ground(Hp) -> (add_pieces_to_remove(GameBoard, Tp, [Hp|List], PiecesToRemove));
		(add_pieces_to_remove(GameBoard, Tp, List, PiecesToRemove))
	).

%get_pieces_diff(+Move, +GameState, -PiecesToAdd, -PiecesToRemove)
get_pieces_to_remove(move(_, MoveEndPosition, _), game_state(_, _, GameBoard), PiecesToRemove) :-
	(getPieceWithXYOffset(MoveEndPosition, 1, 0, GameBoard, RightOfPiece);true),
	(getPieceWithXYOffset(MoveEndPosition, -1, 0, GameBoard, LeftOfPiece);true),
	(getPieceWithXYOffset(MoveEndPosition, 0, 1, GameBoard, BottomOfPiece);true),
	(getPieceWithXYOffset(MoveEndPosition, 0, -1, GameBoard, TopOfPiece);true),

	Remove = [],
	add_pieces_to_remove(GameBoard, [RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], Remove, PiecesToRemove).

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

% update_piece_count(+GameState, +PiecesToRemove, -NextGameState)
update_player_piece_count(game_state(white, npieces(WhiteCount, BlackCount), CurrentBoard), PiecesToRemove, game_state(black, npieces(WhiteCount, NextBlackCount), CurrentBoard)) :-
	length(PiecesToRemove, RemovedAmount),
	NextBlackCount is BlackCount - RemovedAmount.
update_player_piece_count(game_state(black, npieces(WhiteCount, BlackCount), CurrentBoard), PiecesToRemove, game_state(white, npieces(NextWhiteCount, BlackCount), CurrentBoard)) :-
	length(PiecesToRemove, RemovedAmount),
	NextWhiteCount is WhiteCount - RemovedAmount.

game_is_over(game_state(_, npieces(WhiteCount, BlackCount), _)) :-
	((WhiteCount =:= 0 -> Winner = black ; false);
	(BlackCount =:= 0 -> Winner = white ; false)),
	print('The game is over! The winners are the \''), print(Winner), print('\' pieces!').

% The C arg is just a counter to simulate the game end (Ends when C == 3)
% In reality, the game ends when one of the players only has one piece, and the winner is the other
% game_loop(+GameState, +Player, +C)
game_loop(GameState) :-
	display_game(GameState),
	get_move(Move, GameState),
	apply_move(Move, GameState, TempGameState),
	get_pieces_to_remove(Move, TempGameState, PiecesToRemove),
	update_player_piece_count(TempGameState, PiecesToRemove, TempGameState1),
	apply_pieces_to_remove(PiecesToRemove, TempGameState1, NextGameState),
	(game_is_over(NextGameState) -> true ; game_loop(NextGameState)).

play :-
	initial(GameState),
	game_loop(GameState).

