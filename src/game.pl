
:- consult('board.pl').
:- consult('input.pl').
:- consult('menus.pl').
:- consult('bots.pl').

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

cavePosition(Position) :-
	Position = position(1, 5);
	Position = position(5, 5);
	Position = position(9, 5).

%
remove_pieces([], NextBoard, NextBoard).
remove_pieces([piecePosition(position(X, Y), _)|PiecesT], CurrentBoard, NextBoard) :-
	nth1(Y, CurrentBoard, Row),
	(cavePosition(position(X, Y)) -> 
	updateBoardColumn(X, dragonCave(invoked), Row, NewRow) ;
	updateBoardColumn(X, empty, Row, NewRow)),
	updateBoardRow(Y, NewRow, CurrentBoard, TempBoard),
	remove_pieces(PiecesT, TempBoard, NextBoard).

%invoke_dragon(_, _, _, [], _, [])
invoke_dragon(position(X, Y), Strength, GameBoard, [piecePosition(_, Dice)|[]], piecePosition(_, PreviousDice), List) :-
	nth1(Y, GameBoard, Row),
	nth1(X, Row, Dragon),

	get_value_from_dice(Dice, Value, _),
	get_value_from_dice(PreviousDice, PreviousValue, _),
	(
		(Value = PreviousValue, Value \= empty, Dragon = dragonCave(empty)) -> List = [piecePosition(position(X, Y), dice(Value, Strength))] ; List = []
	).
invoke_dragon(position(X, Y), Strength, GameBoard, [piecePosition(_, Dice)|T], piecePosition(_, PreviousDice), List) :-
	nth1(Y, GameBoard, Row),
	nth1(X, Row, Dragon),

	get_value_from_dice(Dice, Value, _),
	get_value_from_dice(PreviousDice, PreviousValue, _),
	(
		(Value = PreviousValue, Value \= empty, Dragon = dragonCave(empty)) -> invoke_dragon(position(X, Y), Strength, GameBoard, T, piecePosition(_, Dice), List) ; List = []
	).


%verify_dragon_cave()
verify_dragon_cave(Position, Strength, GameBoard, Current, Next):-
	(getPieceWithXYOffset(Position, 1, 0, GameBoard, RightOfPiece) ;true),
	(getPieceWithXYOffset(Position, -1, 0, GameBoard, LeftOfPiece) ;true),
	(getPieceWithXYOffset(Position, 0, 1, GameBoard, BottomOfPiece) ;true),
	(getPieceWithXYOffset(Position, 0, -1, GameBoard, TopOfPiece) ;true),

	build_valid_list([RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], [], L),

	(
		(L \= []) ->
		(
			head(L, H), tail(L, T),
			invoke_dragon(Position, Strength, GameBoard, T, H, List),
			append(Current, List, Next)
		);
		true
	).

verify_new_pieces([], NewWhiteCount, NewBlackCount, NewWhiteCount, NewBlackCount).
/*verify_new_pieces([H|T], WhiteCount, BlackCount, NewWhiteCount, NewBlackCount) :-

.*/

% check_dragons(+GameState, -NextGameState)
% Checks if dragon caves have been occupied.
check_dragons(game_state(Player, npieces(WhiteCount, BlackCount), CurrentBoard), game_state(Player, npieces(WhiteCount, BlackCount), NextBoard)) :-
	verify_dragon_cave(position(1, 5), 3, CurrentBoard, [], Dragons1),
	verify_dragon_cave(position(5, 5), 5, CurrentBoard, Dragons1, Dragons2),
	verify_dragon_cave(position(9, 5), 3, CurrentBoard, Dragons2, Dragons),
	
	/*
	verify_new_pieces(Dragons, 0, 0, TempWhiteCount, TempBlackCount),
	NewWhiteCount is TempWhiteCount + WhiteCount,
	NewBlackCount is TemoWhiteCount + BlackCount,*/

	add_pieces(Dragons, CurrentBoard, NextBoard).

% apply user move
% apply_move(+move, +GameState, -TempGameState)
apply_move(move(MoveStartPosition, MoveEndPosition, Piece), game_state(Player, CurrentPieces, GameBoard), game_state(Player, CurrentPieces, TempBoard)):-
	remove_pieces([piecePosition(MoveStartPosition, Piece)], GameBoard, AddBoard),
	add_pieces([piecePosition(MoveEndPosition, Piece)], AddBoard, TempBoard).

% removes eaten pieces
% apply_move(+PiecesToRemove, +CurrentGameState, -NextGameState)
apply_changed_pieces(PiecesToRemove, PiecesToAdd, game_state(CurrentPlayer, CurrentNPieces, CurrentBoard), game_state(CurrentPlayer, CurrentNPieces, NextBoard)) :-
	add_pieces(PiecesToAdd, CurrentBoard, TempBoard),
	remove_pieces(PiecesToRemove, TempBoard, NextBoard).

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

% get_dice(+Dice, -Piece)
% Gets piece value from dice
get_dice(dice(Piece, Strength), Piece, Strength).

% get_value_from_dice(+Piece, -Value)
% Gets piece value from Piece either it is a dice or not
get_value_from_dice(Piece, Value, Strength) :-
	(
		(
			Piece == empty ; Piece == dragonCave(empty) ; Piece == mountain
		) -> Value = Piece, Strength = 0;
		get_dice(Piece, Value, Strength)
	).

get_opposite_type(black, white).
get_opposite_type(white, black).

% eats_by_strength(+Piece, +Neighboors)
eats_by_strength(_, [], PiecesToRemove, PiecesToRemove, _).
eats_by_strength(dice(Piece, Strength), [piecePosition(Position, Dice)|T], TempPiecesToRemove, PiecesToRemove, PieceToAdd) :-
	get_value_from_dice(Dice, TargetPiece, TargetStrength),
	(
		(get_opposite_type(Piece, TargetPiece), Strength > TargetStrength) -> 
		(
			(\+ member(Dice, TempPiecesToRemove)) -> 
			(
				append([piecePosition(Position, Dice)], TempPiecesToRemove, PiecesToRemove), Strength1 is Strength - 1, PieceToAdd = dice(Piece, Strength1)
			); 
			PiecesToRemove = TempPiecesToRemove
		);
		eats_by_strength(dice(Piece, Strength), T, TempPiecesToRemove, PiecesToRemove, PieceToAdd)
	).

% Checks if a piece is eaten
% is_eaten(+PiecePosition, +GameBoard)
is_eaten(PiecePosition, GameBoard) :- is_eaten_horizontal(PiecePosition, GameBoard).
is_eaten(PiecePosition, GameBoard) :- is_eaten_vertical(PiecePosition, GameBoard).

is_eaten_horizontal(piecePosition(Position, dice(Piece, _)), GameBoard) :-
	Piece \= empty, Piece \= mountain, Piece \= dragonCave(_),
	getPieceWithXYOffset(Position, 1, 0, GameBoard, piecePosition(_, RightOfPieceDice)),
	getPieceWithXYOffset(Position, -1, 0, GameBoard, piecePosition(_, LeftOfPieceDice)),
	get_value_from_dice(RightOfPieceDice, RightOfPiece, _),
	get_value_from_dice(LeftOfPieceDice, LeftOfPiece, _),
	(RightOfPiece \= empty, RightOfPiece \= Piece, LeftOfPiece \= empty, RightOfPiece \= Piece).

is_eaten_vertical(piecePosition(Position, dice(Piece, _)), GameBoard) :-
	Piece \= empty, Piece \= mountain, Piece \= dragonCave(_),
	getPieceWithXYOffset(Position, 0, 1, GameBoard, piecePosition(_, BottomOfPieceDice)),
	getPieceWithXYOffset(Position, 0, -1, GameBoard, piecePosition(_, TopOfPieceDice)),
	get_value_from_dice(BottomOfPieceDice, BottomOfPiece, _),
	get_value_from_dice(TopOfPieceDice, TopOfPiece, _),
	(BottomOfPiece \= empty, BottomOfPiece \= Piece, TopOfPiece \= empty, TopOfPiece \= Piece).

% Adds pieces to list containing pieces to remove from the board
% add_pieces_to_remove(+GameBoard, +Pieces, +Remove, -PiecesToRemove)
add_pieces_to_remove(_, [], List, List).
add_pieces_to_remove(GameBoard, [Hp|Tp], List, PiecesToRemove) :-
	(
		is_eaten(Hp, GameBoard), ground(Hp) -> (add_pieces_to_remove(GameBoard, Tp, [Hp|List], PiecesToRemove));
		(add_pieces_to_remove(GameBoard, Tp, List, PiecesToRemove))
	).

%build_valid_list(+List, +Helper, -Ret)
build_valid_list([], Helper, Ret):-
	reverse(Helper, Ret).
build_valid_list([H|T], Helper, Ret):-
	(
		ground(H) -> build_valid_list(T, [H|Helper], Ret);
		build_valid_list(T, Helper, Ret)
	).

%get_pieces_diff(+Move, +GameState, -PiecesToAdd, -PiecesToRemove)
get_changed_pieces(move(_, MoveEndPosition, Piece), game_state(_, _, GameBoard), PiecesToRemove, PiecesToAdd) :-
	(getPieceWithXYOffset(MoveEndPosition, 1, 0, GameBoard, RightOfPiece) ;true),
	(getPieceWithXYOffset(MoveEndPosition, -1, 0, GameBoard, LeftOfPiece) ;true),
	(getPieceWithXYOffset(MoveEndPosition, 0, 1, GameBoard, BottomOfPiece) ;true),
	(getPieceWithXYOffset(MoveEndPosition, 0, -1, GameBoard, TopOfPiece) ;true),

	Remove = [],
	add_pieces_to_remove(GameBoard, [RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], Remove, TempPiecesToRemove),

	build_valid_list([RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], [], List),

	%verify if eats and add to pieces to remove if not there already and add to pieces to add if eats
	eats_by_strength(Piece, List, TempPiecesToRemove, PiecesToRemove, PieceToAdd),
	(
		ground(PieceToAdd) ->
		PiecesToAdd = [piecePosition(MoveEndPosition, PieceToAdd)]; true
	).

% Gets a position with the desired type of piece
%get_desired_position_input(-Position, +GameBoard, +DesiredOccupant, -Piece)
get_desired_position_input(position(X1, Y1), GameBoard, DesiredOccupant, Piece):-
	get_position_input(position(XTemp, YTemp)),
	(
		(nth1(YTemp, GameBoard, YLine), (nth1(XTemp, YLine, dice(DesiredOccupant, _)); nth1(XTemp, YLine, DesiredOccupant)))
			-> (X1 = XTemp, Y1 = YTemp, nth1(XTemp, YLine, Piece))
			; (print('The coordinates you inserted are not \''), print(DesiredOccupant), print('\'.\n'), get_desired_position_input(position(X1, Y1), GameBoard, DesiredOccupant, Piece))
	).

verify_orthogonal(position(X1, Y1), position(X2, Y2), GameBoard) :-
	get_desired_position_input(position(TempX2, TempY2), GameBoard, empty, _),
	(
		(X1 \= TempX2, Y1 \= TempY2)
			-> print('Not orthogonal\n'), verify_orthogonal(position(X1, Y1), position(X2, Y2), GameBoard);
			X2 is TempX2, Y2 is TempY2
	).

% get_move(-Move, +GameState)
get_move(move(Position1, Position2, Piece), game_state(CurrentPlayer, _, GameBoard)) :-
	% read player's moves
	print('Piece to move:\n'),
	get_desired_position_input(position(X1, Y1), GameBoard, CurrentPlayer, Piece),
	
	print('Desired place:\n'),
	verify_orthogonal(position(X1, Y1), position(X2, Y2), GameBoard),

	Position1 = position(X1, Y1), Position2 = position(X2, Y2).

% update_piece_count(+GameState, +PiecesToRemove, -NextGameState)
update_player_piece_count(PiecesToRemove, game_state(white, npieces(WhiteCount, BlackCount), CurrentBoard), game_state(black, npieces(WhiteCount, NextBlackCount), CurrentBoard)) :-
	length(PiecesToRemove, RemovedAmount),
	NextBlackCount is BlackCount - RemovedAmount.
update_player_piece_count(PiecesToRemove, game_state(black, npieces(WhiteCount, BlackCount), CurrentBoard), game_state(white, npieces(NextWhiteCount, BlackCount), CurrentBoard)) :-
	length(PiecesToRemove, RemovedAmount),
	NextWhiteCount is WhiteCount - RemovedAmount.

game_is_over(game_state(_, npieces(WhiteCount, BlackCount), _)) :-
	((WhiteCount =:= 1 -> Winner = black ; false);
	(BlackCount =:= 1 -> Winner = white ; false)),
	print('The game is over! The winners are the \''), print(Winner), print('\' pieces!').

current_player(game_state(CurrPlayer, _, _), Player) :- Player = CurrPlayer.


player_play(GameState, NextGameState) :-
	current_player(GameState, Player),
	display_game(GameState),
	get_move(Move, GameState),
	apply_move(Move, GameState, TempGameState),
	get_changed_pieces(Move, TempGameState, PiecesToRemove, PiecesToAdd),
	update_player_piece_count(PiecesToRemove, TempGameState, TempGameState1),
	apply_changed_pieces(PiecesToRemove, PiecesToAdd, TempGameState1, TempGameState2),
	check_dragons(TempGameState2, NextGameState).

computer_play(GameState, Dfficulty, NextGameState) :-
	current_player(GameState, Player),
	display_game(GameState),
	choose_move(GameState, Player, Difficulty, Move),
	apply_move(Move, GameState, TempGameState),
	get_changed_pieces(Move, TempGameState, PiecesToRemove, PiecesToAdd),
	update_player_piece_count(PiecesToRemove, TempGameState, TempGameState1),
	apply_changed_pieces(PiecesToRemove, PiecesToAdd, TempGameState1, TempGameState2),
	check_dragons(TempGameState2, NextGameState).

% game_loop(+GameState, +Player, +C)
game_loop_PvP(GameState) :-
	player_play(GameState, NextGameState), !,
	(game_is_over(NextGameState) -> true ; game_loop_PvP(NextGameState)).

game_loop_PvM(GameState, Difficulty) :- game_loop_PvM(GameState, Difficulty, player).
game_loop_PvM(GameState, Difficulty, player) :-
	player_play(GameState, NextGameState), !,
	(game_is_over(NextGameState) -> true ; game_loop_PvM(NextGameState, Difficulty, computer)).
game_loop_PvM(GameState, Difficulty, computer) :-
	computer_play(GameState, Dfficulty, NextGameState), !,
	(game_is_over(NextGameState) -> true ; game_loop_PvM(NextGameState, Difficulty, player)).

start_PvP_game :-
	initial(GameState),
	game_loop_PvP(GameState).

start_PvM_game :-
	initial(GameState),
	game_loop_PvM(GameState, easy).

play :- main_menu.

