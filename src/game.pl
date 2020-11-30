:-use_module(library(lists)).
:- consult('utils.pl').
:- consult('board.pl').
:- consult('input.pl').
:- consult('menus.pl').
:- consult('bots.pl').

% initial(-GameState)
% Unifies 'GameState' with the initial game state
initial(game_state(white, npieces(8, 8), Board)) :- board(Board).


% invoke_dragon(+Position, +Strength, +GameBoard, +PiecePositions, +PreviousPiecePosition, -InvokedDragons)
% Verifies if there are dragon caves surround by pieces of the same type
% If it is teh case a Dragon is invoked, adding the correspondent piece to InvokedDragons
invoke_dragon(Position, Strength, GameBoard, [piecePosition(_DicePosition, Dice)|[]], piecePosition(_PreviousDicePosition, PreviousDice), InvokedDragons) :-
	nth_board_element(GameBoard, Position, Dragon),

	get_value_from_dice(Dice, Value, _DiceStrength),
	get_value_from_dice(PreviousDice, PreviousValue, _PreviousDiceStrength),
	(
		(Value = PreviousValue, Value \= empty, Dragon = dragonCave(empty)) -> InvokedDragons = [piecePosition(Position, dice(Value, Strength))] ; InvokedDragons = []
	).
invoke_dragon(Position, Strength, GameBoard, [piecePosition(_DicePosition, Dice)|T], piecePosition(_PreviousDicePosition, PreviousDice), InvokedDragons) :-
	nth_board_element(GameBoard, Position, Dragon),

	get_value_from_dice(Dice, Value, _DiceStrength),
	get_value_from_dice(PreviousDice, PreviousValue, _PreviousDiceStrength),
	(
		(Value = PreviousValue, Value \= empty, Dragon = dragonCave(empty)) -> invoke_dragon(Position, Strength, GameBoard, T, piecePosition(_DicePosition, Dice), InvokedDragons) ; InvokedDragons = []
	).


% verify_dragon_cave(+Position, +Strength, +GameBoard, +Current, +Next)
% Verifies if a dragon cave has been occupied, invoking the dragon correpondent to the cave being analysed
verify_dragon_cave(Position, Strength, GameBoard, Current, Next):-
	(get_piece_with_xy_offset(Position, 1, 0, GameBoard, RightOfPiece) ;true),
	(get_piece_with_xy_offset(Position, -1, 0, GameBoard, LeftOfPiece) ;true),
	(get_piece_with_xy_offset(Position, 0, 1, GameBoard, BottomOfPiece) ;true),
	(get_piece_with_xy_offset(Position, 0, -1, GameBoard, TopOfPiece) ;true),

	build_valid_list([RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], [], L),

	(
		(L \= []) ->
		(
			head(L, FirstPosition), tail(L, Positions),
			invoke_dragon(Position, Strength, GameBoard, Positions, FirstPosition, List),
			append(Current, List, Next)
		);
		true
	).

% verify_new_pieces(+PiecesToRemove, +CurrentWhiteCount, +CurrentBlackCount, -NewWhiteCount, -NewBlackCount)
% verifies if there are new dragons to update players piece count
verify_new_pieces([], NewWhiteCount, NewBlackCount, NewWhiteCount, NewBlackCount).
verify_new_pieces([piecePosition(_Position, dice(Color, _Value))|T], WhiteCount, BlackCount, NewWhiteCount, NewBlackCount) :-
	Color = white,
	WhiteCount1 is WhiteCount + 1,
	verify_new_pieces(T, WhiteCount1, BlackCount, NewWhiteCount, NewBlackCount).
verify_new_pieces([piecePosition(_Position, dice(Color, _Value))|T], WhiteCount, BlackCount, NewWhiteCount, NewBlackCount) :-
	Color = black,
	BlackCount1 is BlackCount + 1,
	verify_new_pieces(T, WhiteCount, BlackCount1, NewWhiteCount, NewBlackCount).

% check_dragons(+GameState, -NextGameState)
% Checks if dragon caves have been occupied, generating the new dragon piece and marking the cave as invoked, updating the game state
check_dragons(game_state(Player, npieces(WhiteCount, BlackCount), CurrentBoard), game_state(Player, npieces(NewWhiteCount, NewBlackCount), NextBoard)) :-
	verify_dragon_cave(position(1, 5), 3, CurrentBoard, [], Dragons1),
	verify_dragon_cave(position(5, 5), 5, CurrentBoard, Dragons1, Dragons2),
	verify_dragon_cave(position(9, 5), 3, CurrentBoard, Dragons2, Dragons),
	
	verify_new_pieces(Dragons, 0, 0, TempWhiteCount, TempBlackCount),
	NewWhiteCount is TempWhiteCount + WhiteCount,
	NewBlackCount is TempBlackCount + BlackCount,

	add_pieces(Dragons, CurrentBoard, NextBoard).

% apply_move(+Move, +GameState, -TempGameState)
% Applies valid move received from the user, removing the original piece and adding it to its new position
apply_move(move(MoveStartPosition, MoveEndPosition, Piece), game_state(Player, CurrentPieces, GameBoard), game_state(Player, CurrentPieces, TempBoard)):-
	remove_pieces([piecePosition(MoveStartPosition, Piece)], GameBoard, AddBoard),
	add_pieces([piecePosition(MoveEndPosition, Piece)], AddBoard, TempBoard).

% apply_move(+PiecesToRemove, +PiecesToAdd, +CurrentGameState, -NextGameState)
% Adds PiecesToAdd and removes PiecesToRemove from CurrentGameState, updating GameState to NextGameState
apply_changed_pieces(PiecesToRemove, PiecesToAdd, game_state(CurrentPlayer, CurrentNPieces, CurrentBoard), game_state(CurrentPlayer, CurrentNPieces, NextBoard)) :-
	add_pieces(PiecesToAdd, CurrentBoard, TempBoard),
	remove_pieces(PiecesToRemove, TempBoard, NextBoard).

% eats_by_strength(+Piece, +Neighboors, +TempPiecesToRemove, +PiecesToRemove, +PieceToAdd)
% Checks if there were any pieces eaten by the strength capture rule, by inserting them into PiecesToRemove
% A piece captures another by strength when is next to an opposite piece of lower, seeing its value decremented by one unit
% PieceToAdd is unified with the piece that performed the capture by strength with value decremented
eats_by_strength(_Dice, [], PiecesToRemove, PiecesToRemove, _PieceToAdd).
eats_by_strength(dice(Piece, Strength), [piecePosition(Position, Dice)|T], TempPiecesToRemove, PiecesToRemove, PieceToAdd) :-
	get_value_from_dice(Dice, TargetPiece, TargetStrength),
	(
		(get_opposite_type(Piece, TargetPiece), Strength > TargetStrength) -> 
		(
			append([piecePosition(Position, Dice)], TempPiecesToRemove, PiecesToRemove), Strength1 is Strength - 1, PieceToAdd = dice(Piece, Strength1)
		);
		eats_by_strength(dice(Piece, Strength), T, TempPiecesToRemove, PiecesToRemove, PieceToAdd)
	).

% is_eaten(+PiecePosition, +GameBoard)
% Checks if a piece is eaten applying the custodial capture rule vertically or horizontally
% A piece is eaten by the custodial capture rule when is surrounded by pieces different from itself in the same direction (Horizontal/Vertical)
is_eaten(PlayerPiecePosition, PiecePosition, GameBoard) :- is_eaten_horizontal(PlayerPiecePosition, PiecePosition, GameBoard).
is_eaten(PlayerPiecePosition, PiecePosition, GameBoard) :- is_eaten_vertical(PlayerPiecePosition, PiecePosition, GameBoard).

% is_eaten_horizontal(+PlayerPiecePosition, +PiecePosition, +GameBoard)
% Checks if PiecePosition is eaten horizontally, applying the custodial capture rule
is_eaten_horizontal(PlayerPiecePosition, piecePosition(Position, dice(Piece, _Value)), GameBoard) :-
	Piece \= empty, Piece \= mountain, Piece \= dragonCave(_State),
	get_piece_with_xy_offset(Position, 1, 0, GameBoard, piecePosition(RightOfPiecePosition, RightOfPieceDice)),
	get_piece_with_xy_offset(Position, -1, 0, GameBoard, piecePosition(LeftOfPiecePosition, LeftOfPieceDice)),
	(PlayerPiecePosition = RightOfPiecePosition ; PlayerPiecePosition = LeftOfPiecePosition),
	get_value_from_dice(RightOfPieceDice, RightOfPiece, _RightValue),
	get_value_from_dice(LeftOfPieceDice, LeftOfPiece, _LeftValue),
	(RightOfPiece \= empty, RightOfPiece \= Piece, LeftOfPiece \= empty, LeftOfPiece \= Piece).

% is_eaten_horizontal(+PlayerPiecePosition, +PiecePosition, +GameBoard)
% Checks if PiecePosition is eaten vertically, applying the custodial capture rule
is_eaten_vertical(PlayerPiecePosition, piecePosition(Position, dice(Piece, _Value)), GameBoard) :-
	Piece \= empty, Piece \= mountain, Piece \= dragonCave(_State),
	get_piece_with_xy_offset(Position, 0, 1, GameBoard, piecePosition(BottomOfPiecePosition, BottomOfPieceDice)),
	get_piece_with_xy_offset(Position, 0, -1, GameBoard, piecePosition(TopOfPiecePosition, TopOfPieceDice)),
	(PlayerPiecePosition = BottomOfPiecePosition ; PlayerPiecePosition = TopOfPiecePosition),
	get_value_from_dice(BottomOfPieceDice, BottomOfPiece, _BottomValue),
	get_value_from_dice(TopOfPieceDice, TopOfPiece, _TopValue),
	(BottomOfPiece \= empty, BottomOfPiece \= Piece, TopOfPiece \= empty, TopOfPiece \= Piece).

% custodial_capture(+GameBoard, +Pieces, +Remove, -PiecesToRemove)
% Verifies if pieces present in Pieces will be eaten apllying the custodial capture rule
% Pieces that are confirmed to be eaten will be added to PiecesToRemove
custodial_capture(_GameState, _PlayerPiecePosition, [], List, List).
custodial_capture(game_state(Player, _NPieces, GameBoard), PlayerPiecePosition, [PiecePosition|PPT], List, PiecesToRemove) :-
	(
		ground(PiecePosition), is_eaten(PlayerPiecePosition, PiecePosition, GameBoard) -> (custodial_capture(game_state(Player, _NPieces, GameBoard), PlayerPiecePosition, PPT, [PiecePosition|List], PiecesToRemove));
		(custodial_capture(game_state(Player, _NPieces, GameBoard), PlayerPiecePosition, PPT, List, PiecesToRemove))
	).

% get_pieces_diff(+Move, +GameState, -PiecesToRemove, -PiecesToAdd)
% verifies if the game board needs to be changed after the user move has been applied
% PiecesToRemove represents a list that will contain all the eaten pieces associated with current move
% PiecesToAdd represents a list that will contain all the new/changed pieces associated with current move
get_changed_pieces(move(_MoveStartPosition, MoveEndPosition, Piece), game_state(Player, NPieces, GameBoard), PiecesToRemove, PiecesToAdd) :-
	(get_piece_with_xy_offset(MoveEndPosition, 1, 0, GameBoard, RightOfPiece) ;true),
	(get_piece_with_xy_offset(MoveEndPosition, -1, 0, GameBoard, LeftOfPiece) ;true),
	(get_piece_with_xy_offset(MoveEndPosition, 0, 1, GameBoard, BottomOfPiece) ;true),
	(get_piece_with_xy_offset(MoveEndPosition, 0, -1, GameBoard, TopOfPiece) ;true),

	custodial_capture(game_state(Player, NPieces, GameBoard), MoveEndPosition, [RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], [], TempPiecesToRemove),

	build_valid_list([RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], [], List),

	%verify if eats and add to pieces to remove if not there already and add to pieces to add if eats
	(
		length(TempPiecesToRemove, L), L = 0 ->
		(
			eats_by_strength(Piece, List, TempPiecesToRemove, PiecesToRemove, PieceToAdd),
			(
				ground(PieceToAdd) ->
				PiecesToAdd = [piecePosition(MoveEndPosition, PieceToAdd)]; true
			)
		)
		; 
		PiecesToRemove = TempPiecesToRemove
	).

% verify_orthogonal(+Position1, +Position2, +GameBoard)
% Verifies if two positions given as parameters are orthogonal
verify_orthogonal(position(X1, Y1), position(X2, Y2), GameBoard) :-
	get_desired_position_input(position(TempX2, TempY2), GameBoard, empty, _Piece),
	(
		(X1 \= TempX2, Y1 \= TempY2)
			-> print('Not orthogonal :/\n'), verify_orthogonal(position(X1, Y1), position(X2, Y2), GameBoard);
			X2 is TempX2, Y2 is TempY2
	).

% get_move(-Move, +GameState)
% Asks user for the desired piece to move and the destiny position
% Verifies if the move introduced is valid
get_move(move(Position1, Position2, Piece), game_state(CurrentPlayer, _NPieces, GameBoard)) :-
	% read player's moves
	print('Piece to move (Example: A,1):\n'),
	get_desired_position_input(position(X1, Y1), GameBoard, CurrentPlayer, Piece),
	
	print('Desired place (Example: E,4):\n'),
	verify_orthogonal(position(X1, Y1), position(X2, Y2), GameBoard),

	Position1 = position(X1, Y1), Position2 = position(X2, Y2).

% update_piece_count(+GameState, +PiecesToRemove, -NextGameState)
% Updates each player current pieces count
update_player_piece_count(PiecesToRemove, game_state(white, npieces(WhiteCount, BlackCount), CurrentBoard), game_state(black, npieces(WhiteCount, NextBlackCount), CurrentBoard)) :-
	length(PiecesToRemove, RemovedAmount),
	NextBlackCount is BlackCount - RemovedAmount.
update_player_piece_count(PiecesToRemove, game_state(black, npieces(WhiteCount, BlackCount), CurrentBoard), game_state(white, npieces(NextWhiteCount, BlackCount), CurrentBoard)) :-
	length(PiecesToRemove, RemovedAmount),
	NextWhiteCount is WhiteCount - RemovedAmount.

% game_over(+GameState, -Winner)
% Interprets game state and verifies if game is over
% Game ends when one of the players has only one piece on the board
game_over(GameState, Winner) :-
	GameState =.. [_PredName, _Player, npieces(WhiteCount, BlackCount), _Board],
	((WhiteCount =:= 1 -> Winner = black ; false);
	(BlackCount =:= 1 -> Winner = white ; false)),
	display_game(GameState, -1).

% current_player(+GameState, -Player)
% Gets the current player
current_player(game_state(CurrPlayer, _NPieces, _Board), Player) :- Player = CurrPlayer.

% move(+GameState, +Move, -NewGameState)
% Performs a player move
% Applies the user move, checks if there were any eaten pieces or any invoked dragons 
move(GameState, Move, NewGameState) :-
	apply_move(Move, GameState, TempGameState),
	get_changed_pieces(Move, TempGameState, PiecesToRemove, PiecesToAdd),
	update_player_piece_count(PiecesToRemove, TempGameState, TempGameState1),
	apply_changed_pieces(PiecesToRemove, PiecesToAdd, TempGameState1, TempGameState2),
	check_dragons(TempGameState2, NewGameState).	

% player_play(+GameState, -NextGameState)
% Executes player play
% Displays the current state of the game, asks user to input the desired move to perform
% and executes
player_play(GameState, NextGameState) :-
	current_player(GameState, Player),
	display_game(GameState, Player),
	get_move(Move, GameState),
	move(GameState, Move, NextGameState).

% computer_play(+GameState, +Difficulty, -NextGameState)
% Executes computer play
% Displays the current state of the game, chooses a move to be played by the bot
% and executes the selected move
computer_play(GameState, Difficulty, NextGameState) :-
	current_player(GameState, Player),
	display_game(GameState, Player),
	choose_move(GameState, Player, Difficulty, Move),
	move(GameState, Move, NextGameState).

% game_loop_pvp(+GameState)
% Player versus Player game loop
% Execute player play, verifying if game is over
game_loop_pvp(GameState) :-
	player_play(GameState, NextGameState), !,
	(game_over(NextGameState, Winner) -> game_over_menu(Winner) ; game_loop_pvp(NextGameState)).

% game_loop_pvm(+GameState, +Difficulty)
% Player versus Machine game loop
% Executes player play or computer play, depending on the current player 
% Waits for user input, on computer play, so the game proceeds and verifies if game is over
game_loop_pvm(GameState, Difficulty) :- game_loop_pvm(GameState, Difficulty, player).
game_loop_pvm(GameState, Difficulty, player) :-
	player_play(GameState, NextGameState), !,
	(game_over(NextGameState, Winner) -> game_over_menu(Winner) ; game_loop_pvm(NextGameState, Difficulty, computer)).
game_loop_pvm(GameState, Difficulty, computer) :-
	wait_for_user_input,
	computer_play(GameState, Difficulty, NextGameState), !,
	(game_over(NextGameState, Winner) -> game_over_menu(Winner) ; game_loop_pvm(NextGameState, Difficulty, player)).

% game_loop_mvm(+GameState, +Difficulty)
% Machine versus Machine game loop
% Executes computer play, waits for user input so the game proceeds and verifies if game is over
game_loop_mvm(GameState, Difficulty) :-
	computer_play(GameState, Difficulty, NextGameState), !,
	wait_for_user_input,
	(game_over(NextGameState, Winner) -> game_over_menu(Winner) ; game_loop_mvm(NextGameState, Difficulty)).

% start_pvp_game(+Difficulty)
% Starts Player versus Player game loop with given Difficulty
start_pvp_game :-
	initial(GameState),
	game_loop_pvp(GameState).

% start_pvm_game(+Difficulty)
% Starts Player versus Machine game loop with given Difficulty
start_pvm_game(Difficulty) :-
	initial(GameState),
	game_loop_pvm(GameState, Difficulty).

% start_mvm_game(+Difficulty)
% Starts Machine versus Machine game loop with given Difficulty
start_mvm_game(Difficulty) :-
	initial(GameState),
	game_loop_mvm(GameState, Difficulty).


% play/0
% main predicate, starts the game execution, redirecting user to main menu
play :- main_menu.

