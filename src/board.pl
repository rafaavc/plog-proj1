% board(-Board)
% Unifies 'Board' with the initial board
board([
	[mountain, dice(black, 3), dice(black, 2), dice(black, 2), dice(black, 2), dice(black, 2), dice(black, 2), dice(black, 3), mountain],
	[empty, empty, empty, empty, dice(black, 4), empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[dragonCave(empty), empty, empty, empty, dragonCave(empty), empty, empty, empty, dragonCave(empty)],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, dice(white, 4), empty, empty, empty, empty],
	[mountain, dice(white, 3), dice(white, 2), dice(white, 2), dice(white, 2), dice(white, 2), dice(white, 2), dice(white, 3), mountain]
]).


% cave_position(+Position)
% Succeeds if 'Position' corresponds to a dragon cave.
% Else fails
cave_position(Position) :-
	Position = position(1, 5);
	Position = position(5, 5);
	Position = position(9, 5).


% update_board_column(+Index, +NewElement, +Row, -NewRow)
% Unifies 'NewRow' with the updated version of 'Row', after updating the row with index 'Index' to the 'NewElement'
update_board_column(Index, NewElement, Row, NewRow) :-
	nth1(Index, Row, _Element, Rest),
	nth1(Index, NewRow, NewElement, Rest).

% update_board_row(+Index, +NewRow, +Board, -NewBoard)
% Unifies 'NewBoard' with the updated version of 'Board', after updating the row with index 'Index' to the 'NewRow'
update_board_row(Index, NewRow, Board, NewBoard) :-
	nth1(Index, Board, _Row, Rest),
	nth1(Index, NewBoard, NewRow, Rest).

% add_pieces(+PiecePositions, +GameBoard, -NextGameBoard)
% Unifies 'NextBoard' with the updated version of the 'CurrentBoard', after adding the new 'Pieces'
add_pieces([], NextBoard, NextBoard).
add_pieces([piecePosition(position(X, Y), Piece)|PiecesT], CurrentBoard, TempBoard) :-
	nth1(Y, CurrentBoard, Row),
	update_board_column(X, Piece, Row, NewRow),
	update_board_row(Y, NewRow, CurrentBoard, TempBoard),
	add_pieces(PiecesT, TempBoard, TempBoard).

% remove_pieces(+Pieces, +CurrentBoard, -NextGameBoard)
% Unifies 'NextBoard' with the updated version of the 'CurrentBoard', after removing the 'Pieces'
remove_pieces([], NextBoard, NextBoard).
remove_pieces([piecePosition(position(X, Y), _Piece)|PiecesT], CurrentBoard, NextBoard) :-
	nth1(Y, CurrentBoard, Row),
	(cave_position(position(X, Y)) -> 
	update_board_column(X, dragonCave(invoked), Row, NewRow) ;
	update_board_column(X, empty, Row, NewRow)),
	update_board_row(Y, NewRow, CurrentBoard, TempBoard),
	remove_pieces(PiecesT, TempBoard, NextBoard).


% nth_board_element(+Board, +Position, -Element)
% Unifies 'Element' with the element that's in the given 'Position' of the 'Board'
nth_board_element(Board, position(X, Y), Element) :-
	nth1(Y, Board, Line),
	nth1(X, Line, Element).


% get_dice(+Dice, -Piece)
% Gets piece value from dice
get_dice(dice(Piece, Strength), Piece, Strength).

% get_value_from_dice(+Piece, -Value)
% Gets piece value from Piece either it is represented by a dice or not
get_value_from_dice(Piece, Value, Strength) :-
	(
		(
			Piece = empty ; Piece = dragonCave(empty) ; Piece = dragonCave(invoked) ; Piece = mountain
		) -> (Value = Piece, Strength = 0);
		get_dice(Piece, Value, Strength)
	).

% get_opposite_type(+Piece, -OppositePiece)
% Gets the opposite piece
get_opposite_type(black, white).
get_opposite_type(white, black).


% get_pieces_orthogonal_to_position(+Position, +GameBoard, -L)
% Unifies 'L' with the list of the (most commonly) 4 pieces that are orthogonally placed next to the 'Position' of the current 'GameBoard'
get_pieces_orthogonal_to_position(Position, GameBoard, L) :-
    (get_piece_with_xy_offset(Position, 1, 0, GameBoard, piecePosition(_PosRight, RightOfPiece)) ;true),
	(get_piece_with_xy_offset(Position, -1, 0, GameBoard, piecePosition(_PosLeft, LeftOfPiece)) ;true),
	(get_piece_with_xy_offset(Position, 0, 1, GameBoard, piecePosition(_PosBottom, BottomOfPiece)) ;true),
	(get_piece_with_xy_offset(Position, 0, -1, GameBoard, piecePosition(_PosTop, TopOfPiece)) ;true),
    
	build_valid_list([RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], [], L).

% get_pieces_with_offset_orthogonal_to_position(+Position, +GameBoard, -L)
% Unifies 'L' with the list of the (most commonly) 4 pieces, with their offsets from 'Position', that are orthogonally placed next to the 'Position' of the current 'GameBoard'
get_pieces_with_offset_orthogonal_to_position(Position, GameBoard, L) :-
    (get_piece_with_xy_offset(Position, 1, 0, GameBoard, piecePosition(_PosRight, RightOfPiece)) ;true),
	(get_piece_with_xy_offset(Position, -1, 0, GameBoard, piecePosition(_PosLeft, LeftOfPiece)) ;true),
	(get_piece_with_xy_offset(Position, 0, 1, GameBoard, piecePosition(_PosBottom, BottomOfPiece)) ;true),
	(get_piece_with_xy_offset(Position, 0, -1, GameBoard, piecePosition(_PosTop, TopOfPiece)) ;true),
    
	build_valid_list([pieceXYOffset(xyOffset(1, 0), RightOfPiece), pieceXYOffset(xyOffset(-1, 0), LeftOfPiece), pieceXYOffset(xyOffset(0, 1), BottomOfPiece), pieceXYOffset(xyOffset(0, -1), TopOfPiece)], [], L).

% get_piece_with_xy_offset(+PiecePosition, +Xvar, +Yvar, +GameBoard, -Piece)
% Gets piece in position that corresponds to the sum of the current PiecePosition coordinates with the given offset: Xvar, Yvar
get_piece_with_xy_offset(position(X, Y), Xvar, Yvar, GameBoard, piecePosition(position(X1, Y1), Piece)) :-
	Y1 is Y+Yvar,
	Y1 > 0,
	Y1 =< 9,
	nth1(Y1, GameBoard, Line),
	X1 is X+Xvar,
	X1 > 0,
	X1 =< 9,
	nth1(X1, Line, Piece).

% symbol(+Atom, -Symbol)
% establishes correspondence between each atom and the symbol to be displayed
symbol(mountain, 'M ').
symbol(empty, '  ').

symbol(dice(black, Value), Sym) :-
  number_chars(Value, [ValueAtom|_]),
  atom_concat('B', ValueAtom, Sym).

symbol(dice(white, Value), Sym) :-
  number_chars(Value, [ValueAtom|_]),
  atom_concat('W', ValueAtom, Sym).

symbol(dragonCave(empty), 'D ').
symbol(dragonCave(invoked), 'DI').


% display_board_middle_separator
% displays the board's middle horizontal line separator
display_board_middle_separator :-
	print('  \x251C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x2524\\n').

% display_board_top_separator
% displays the board's top horizontal border separator
display_board_top_separator :-
	print('    A    B    C    D    E    F    G    H    I\n'),
	print('  \x250C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x2510\ '),
	nl.	

% display_board_bottom_separator
% displays the board's ottom horizontal border separator
display_board_bottom_separator :-
	print('  \x2514\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2518\\n').


% display_board_row(+Row, +N)
% displays a board 'Row', including the left digit 'N' that indicates the line coordinate and the pieces that are there
display_board_row([], _N).
display_board_row([Symbol | T], N) :-
	(N \= -1 -> print(N); true),  % prints the left digit
	symbol(Symbol, Value),   % gets the symbol that corresponds to the piece that's in the current position
	print(' \x2502\ '),
	print(Value),
	display_board_row(T, -1).

% display_board(+GameBoard, +N)
% displays the whole 'GameBoard'. N is the line counter, from 1 to 9 (including)
display_board([H | []], N) :-
	display_board_row(H, N),
	print(' \x2502\\n'),
	display_board_bottom_separator.

display_board([H | T], N) :-
	(N == 1 -> display_board_top_separator; true),
	display_board_row(H, N),
	print(' \x2502\\n'),
	display_board_middle_separator,
	N1 is N+1,
	display_board(T, N1).

% display_player(+Player)
% displays the next 'Player'
display_player(-1).
display_player(Player) :-
	print(Player),
	print('\'s turn.'),
	nl.


% display_players_pieces(+NPieces)
% displays the amount of pieces that each player has in-field
display_players_pieces(npieces(NWhitePieces, NBlackPieces)) :- 
	print('White player has '),
	print(NWhitePieces), 
	print(' pieces.'), 
	nl,
	print('Black player has '),
	print(NBlackPieces),
	print(' pieces.'), 
	nl.

% display_game(+GameState, +Player)
% Displays the 'GameState', as well as the current 'Player'
display_game(game_state(_Player, NPieces, GameBoard), Player) :-
	display_board(GameBoard, 1),
	display_players_pieces(NPieces),
	display_player(Player).


