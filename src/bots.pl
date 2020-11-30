:- use_module(library(random)).

% valid_piece_moves(+GameBoard, +PiecePosition, -Moves)
% 'Moves' is unified with the valid piece moves that the piece given in 'PiecePosition' can make in the current 'GameBoard'
valid_piece_moves(GameBoard, piecePosition(position(X, Y), Piece), Moves) :-
    findall(move(position(X, Y), position(X1, Y1), Piece), (
        (X1 = X ; Y1 = Y),  % has got to be orthogonal
        nth_board_element(GameBoard, position(X1, Y1), empty) % can only move to an empty position
    ), Moves).

% valid_moves_from_pieces(+GameBoard, +Pieces, -Moves)
% 'Moves' is unified with all the possible moves that can be made for each of the 'Pieces' in the current 'GameBoard'
valid_moves_from_pieces(_GameBoard, [], []).
valid_moves_from_pieces(GameBoard, [PiecesH|PiecesT], Moves) :-
    valid_piece_moves(GameBoard, PiecesH, PieceMoves),
    valid_moves_from_pieces(GameBoard, PiecesT, MovesTemp),
    append(MovesTemp, PieceMoves, Moves).

% valid_moves(+GameState, +Player, -ListOfMoves)
% 'ListOfMoves' is unified with all the moves the 'Player' can make in the current 'GameState'
valid_moves(game_state(_Player, _NPieces, GameBoard), Player, ListOfMoves) :-
    findall(piecePosition(Position, dice(Player, N)), (    % finds all of the player's pieces
        nth_board_element(GameBoard, Position, dice(Player, N))
    ), PlayersPieces),
    valid_moves_from_pieces(GameBoard, PlayersPieces, ListOfMovesTemp),
    random_permutation(ListOfMovesTemp, ListOfMoves).


% choose_move_easy(+PossibleMoves, -Move)
% 'Move' is unified with the next easy difficulty move.
choose_move_easy(PossibleMoves, Move) :-
    random_member(Move, PossibleMoves).   % Easy difficulty is just random moves


% check_pieces_are_player_or_empty(+Pieces, +Player)
% Succeeds if all the 'Pieces' are either owned by the 'Player' or are empty
check_pieces_are_player_or_empty([], _Player).
check_pieces_are_player_or_empty([dice(Player, _Value)|T], Player) :-
    check_pieces_are_player_or_empty(T, Player).
check_pieces_are_player_or_empty([empty|T], Player) :-
    check_pieces_are_player_or_empty(T, Player).


% available_blank_orthogonal_spots(+Player, +GameBoard, +PiecePosition, -N)
% Succeeds if all the orthogonal spots near the piece in 'PiecePosition' are either occupied by 'Player' or empty, 
% and unifies N with the number of empty spots in those locations
available_blank_orthogonal_spots(Player, GameBoard, PiecePosition, N) :-
    get_pieces_orthogonal_to_position(PiecePosition, GameBoard, L), !,
    check_pieces_are_player_or_empty(L, Player),
    count(empty, L, N).

% is_next_to_dragon(+Position, -DragonPosition)
% If the 'Position' is next to a dragon, 'DragonPosition' is unified with the dragon's position.
% Else, the predicate fails
is_next_to_dragon(position(X, Y), position(DragonX, DragonY)) :-
    (Y = 5 -> ( X = 2 ; X = 4 ; X = 6 ; X = 8) ;
    ((Y = 6 ; Y = 4) -> (X = 1 ; X = 5 ; X = 9) ; fail)),
    (
        ((X = 2; X = 1), DragonX = 1, DragonY = 5) 
        ; 
        ((X = 4 ; X = 5; X = 6), DragonX = 5, DragonY = 5) 
        ; 
        ((X = 8; X = 9), DragonX = 9, DragonY = 5)
    ).

% can_make_dragon(+Player, +GameState, +Moves, -Move)
% If the 'Player' can make a dragon in the current 'GameState', using one of the possible 'Moves', 'Move' is unified with the move that does it.
% Else, the predicate fails.
can_make_dragon(_Player, _GameState, [], _Move) :- !, fail.
can_make_dragon(Player, game_state(_Player, _NPieces, GameBoard), [move(position(X1, Y1), position(X2, Y2), Piece)|_MovesT], Move) :-
    % verifies that the end position is beside a dragon and finds the destination dragon
    is_next_to_dragon(position(X2, Y2), position(DragonX, DragonY)),
    % checks that the piece to move doesn't affect the destination dragon
    ((X1 =:= DragonX-1 ; X1 =:= DragonX+1) -> Y1 \= DragonY; (X1 = DragonX -> (Y1 =\= DragonY+1, Y1 =\= DragonY-1) ; true)),    

    % To be able to make a dragon, the dragonCave must be empty
    nth_board_element(GameBoard, position(DragonX, DragonY), dragonCave(empty)),

    available_blank_orthogonal_spots(Player, GameBoard, position(DragonX, DragonY), N), % Number of free orthogonal spaces surrounding the dragon
    N = 1, !,  % if only one space is left, the dragon can be invoked
    Move = move(position(X1, Y1), position(X2, Y2), Piece), print('Can make dragon!\n').
can_make_dragon(Player, GameState, [_MovesH|MovesT], Move) :-
    can_make_dragon(Player, GameState, MovesT, Move).


    
% check_custodial_capture(+Move, +MiddlePieceOffset, +OppositePieceOffset, +GameBoard)
% Succeeds if the 'Move' results in the custodial capture of the MiddlePiece
check_custodial_capture(move(MoveStartPosition, MoveEndPosition, dice(Player, _Strength)), xyOffset(MiddleX, MiddleY), xyOffset(OppositeX, OppositeY), GameBoard) :-

    get_piece_with_xy_offset(MoveEndPosition, MiddleX, MiddleY, GameBoard, piecePosition(MiddlePiecePosition, MiddlePieceDice)),
    get_piece_with_xy_offset(MoveEndPosition, OppositeX, OppositeY, GameBoard, piecePosition(OppositePiecePosition, OppositePieceDice)),

    MiddlePiecePosition \= MoveStartPosition,      
    OppositePiecePosition \= MoveStartPosition, % So the piece isn't trying to capture custodially with itself

	get_value_from_dice(MiddlePieceDice, MiddlePiece, _MiddleStrength),
	get_value_from_dice(OppositePieceDice, OppositePiece, _OppositeStrength),
    
    get_opposite_type(Player, MiddlePiece), % middle piece has to be opposite from player

    ( % The opposite piece must be one of the following, in orther to result in a custodial capture
        OppositePiece = Player ;
        OppositePiece = mountain ;
        OppositePiece = dragonCave(empty) ;
        OppositePiece = dragonCave(invoked)
    ), !.


% can_capture_custodial(+GameState, +Moves, -OutMove)
% If the player can capture a piece by custodial capture in the current 'GameState', using one of the possible 'Moves', 
% 'OutMove' is unified with the move that does it.
% Else, the predicate fails.
can_capture_custodial(_GameState, [], _Move) :- !, fail.
can_capture_custodial(game_state(_Player, _Npieces, GameBoard), [Move|_MovesT], OutMove) :-
    (
        check_custodial_capture(Move, xyOffset(1, 0), xyOffset(2, 0), GameBoard) ;
        check_custodial_capture(Move, xyOffset(-1, 0), xyOffset(-2, 0), GameBoard) ;
        check_custodial_capture(Move, xyOffset(0, 1), xyOffset(0, 2), GameBoard) ;
        check_custodial_capture(Move, xyOffset(0, -1), xyOffset(0, -2), GameBoard)
    ), !,

    print('Can capture by custodial!\n'),
    OutMove = Move.

can_capture_custodial(game_state(_Player, _Npieces, GameBoard), [_MovesH|MovesT], Move) :-
    can_capture_custodial(game_state(_Player, _Npieces, GameBoard), MovesT, Move).
    

% piece_can_capture_by_power(+Piece, +Pieces)
% Succeeds if the 'Piece' can capture by power at least one of the 'Pieces'
piece_can_capture_by_power(_Eats, []) :- !, fail.
piece_can_capture_by_power(dice(PlayerEats, StrengthEats), [dice(PlayerEaten, StrengthEaten)|_T]) :-
    get_opposite_type(PlayerEats, PlayerEaten),    % needs to be the opposite player
    StrengthEats > StrengthEaten, !.  % needs to have more strength
piece_can_capture_by_power(Eats, [_EatenH|EatenT]) :-
    piece_can_capture_by_power(Eats, EatenT).

% can_capture_by_power(+GameState, +Moves, -Move)
% If the player can capture a piece by power in the current 'GameState', using one of the possible 'Moves', 
% 'Move' is unified with the move that does it.
% Else, the predicate fails.
can_capture_by_power(_GameState, [], _Move) :- !, fail.
can_capture_by_power(game_state(_Player, _NPieces, GameBoard), [move(StartPosition, EndPosition, Piece)|_MovesT], Move) :-
    get_pieces_orthogonal_to_position(EndPosition, GameBoard, PiecesNextToEnd),    % Gets the pieces that are orthogonal to the end move position
    piece_can_capture_by_power(Piece, PiecesNextToEnd), !,                   % checks if the piece that's moved can capture at least one of those by power
    Move = move(StartPosition, EndPosition, Piece), print('Can capture by power!\n').
can_capture_by_power(GameState, [_MovesH|MovesT], Move) :-
    can_capture_by_power(GameState, MovesT, Move).


% choose_move_medium(+GameState, +Player, +PossibleMoves, -Move)
% 'Move' is unified with the next medium difficulty move for 'Player', in the current 'GameState'.
choose_move_medium(GameState, Player, PossibleMoves, Move) :-   % The first priority is creating a dragon if possible
    print('Checking can make dragon...\n'),
    can_make_dragon(Player, GameState, PossibleMoves, Move), !.
choose_move_medium(GameState, _Player, PossibleMoves, Move) :-  % The second is eating a piece by custodial capture
    print('Checking can capture custodial...\n'),
    can_capture_custodial(GameState, PossibleMoves, Move), !.
choose_move_medium(GameState, _Player, PossibleMoves, Move) :-  % The third is eating a piece by capture by power
    print('Checking can capture by power...\n'),
    can_capture_by_power(GameState, PossibleMoves, Move), !.
choose_move_medium(_GameState, _Player, PossibleMoves, Move) :- % Lastly, the bot chooses randomly because it is not that smart (The hard bot is down below)
    random_member(Move, PossibleMoves), !.


% can_be_captured(+PiecePosition, +GameBoard)
% Succeeds if the piece in 'PiecePosition' can be captured if it arrives at the position specified in 'PiecePosition', in the current GameBoard context
% Else fails
can_be_captured(piecePosition(position(X, Y), dice(Piece, Strength)), GameBoard) :-   % Can be captured by strength
    get_pieces_with_offset_orthogonal_to_position(position(X, Y), GameBoard, NearPieces),

    member(pieceXYOffset(xyOffset(OffendingXOffset, OffendingYOffset), empty), NearPieces),  % checks that the piece has an empty space next to it
    get_opposite_type(Piece, OffenderType),

    valid_moves(game_state(_Player, _NPieces, GameBoard), OffenderType, OpponentMoves),
    PosX is X+OffendingXOffset, PosY is Y+OffendingYOffset,
    member(move(_StartPos, position(PosX, PosY), dice(_Who, OpponentStrength)), OpponentMoves),   % checks that an opponent with greater strength can move to that space
    OpponentStrength > Strength, !.  % has got to have more strength

can_be_captured(piecePosition(position(X, Y), dice(Piece, _Value)), GameBoard) :-    % Can be captured by custody
    get_pieces_with_offset_orthogonal_to_position(position(X, Y), GameBoard, NearPieces),

    member(pieceXYOffset(xyOffset(OffendingXOffset, OffendingYOffset), OffendingPiece), NearPieces), % checks that the piece has an opponent's piece next to it

    OffendingPieceX is X + OffendingXOffset, OffendingPieceY is Y + OffendingYOffset,
    
    OffendingPiece \= empty, OffendingPiece \= dice(Piece, _SomeValue),
    get_opposite_type(Piece, OffenderType),
    
    valid_moves(game_state(_Player, _NPieces, GameBoard), OffenderType, OpponentMoves),
    PosX is X -1*OffendingXOffset, PosY is Y -1*OffendingYOffset,
    member(move(position(StartX, StartY), position(PosX, PosY), _OpponentPiece), OpponentMoves),   % checks that the opponent can move another piece to the opposite side
    (StartX \= OffendingPieceX; StartY \= OffendingPieceY), !.


% verify_can_capture_next_round(+GameState, +Moves)
% Succeds if one of the 'Moves' can capture in the current GameState context
% Else fails
verify_can_capture_next_round(GameState, Moves) :-
    can_capture_custodial(GameState, Moves, _CustodialMove), !.
verify_can_capture_next_round(GameState, Moves) :-
    can_capture_by_power(GameState, Moves, _PowerMove), !.


% can_capture_next_round(+GameState, +Moves, -Move)
% If the player can capture a piece in the next round, going to an intermediate position using one of the possible 'Moves', 
% 'Move' is unified with the move that does it.
% Else, the predicate fails.
can_capture_next_round(_GameState, [], _Move) :- !, fail.
can_capture_next_round(game_state(Player, NPieces, GameBoard), [move(StartPosition, EndPosition, Piece)|_MovesT], Move) :-
    valid_piece_moves(GameBoard, piecePosition(EndPosition, Piece), Moves),
    verify_can_capture_next_round(game_state(Player, NPieces, GameBoard), Moves),
    \+ can_be_captured(piecePosition(EndPosition, Piece), GameBoard), !,  % Only moves if the piece can not be captured
    Move = move(StartPosition, EndPosition, Piece),
    print('Can eat next round!\n').

can_capture_next_round(GameState, [_MovesH|MovesT], Move) :-
    can_capture_next_round(GameState, MovesT, Move), !.


% avoid_being_captured(+GameState, +Moves, -Move)
% If the player can save one of his pieces from being captured using one of the possible 'Moves', 
% 'Move' is unified with the move that does it.
% Else, the predicate fails.
avoid_being_captured(_GameState, [], _Move) :- !, fail.
avoid_being_captured(game_state(_Player, _NPieces, GameBoard), [move(StartPosition, EndPosition, Piece)|_MovesT], Move) :-
    can_be_captured(piecePosition(StartPosition, Piece), GameBoard),  % the piece has to be in danger
    \+ can_be_captured(piecePosition(EndPosition, Piece), GameBoard), !,    % the piece has to move to a safer position
    Move = move(StartPosition, EndPosition, Piece), print('Avoided being captured!\n').
 
avoid_being_captured(GameState, [_MovesH|MovesT], Move) :-
    avoid_being_captured(GameState, MovesT, Move), !.


% move_towards_dragon(+GameState, +Moves, -Move)
% If the player can move to near a dragon in the effor to invoke one using one of the possible moves, 
% 'Move' is unified with the move that does it
% Else, the predicate fails.
move_towards_dragon(_GameState, [], _Move) :- !, fail.
move_towards_dragon(game_state(_Player, _NPieces, GameBoard), [move(StartPosition, EndPosition, Piece)|_MovesT], Move) :-
    (
        is_next_to_dragon(StartPosition, DragonPosition) -> 
        (    % if the piece is next to a dragon cave, it only leaves if the dragon was already invoked
            nth_board_element(GameBoard, DragonPosition, dragonCave(invoked))
        )
        ; true
    ),
    is_next_to_dragon(EndPosition, DragonEndPosition),   % only moves to a position that's next to an uninvoked dragon
    
    nth_board_element(GameBoard, DragonEndPosition, dragonCave(empty)),  % checks that the cave can still invoke a dragon

    \+ can_be_captured(piecePosition(EndPosition, Piece), GameBoard), !,  % checks that the piece can not be captured after moving
    Move = move(StartPosition, EndPosition, Piece).

move_towards_dragon(game_state(Player, NPieces, GameBoard), [_MovesH|MovesT], Move) :-
    move_towards_dragon(game_state(Player, NPieces, GameBoard), MovesT, Move).
   

% choose_move_hard(+GameState, +Player, +PossibleMoves, -Move)
% 'Move' is unified with the next hard difficulty move for 'Player', in the current 'GameState'.
choose_move_hard(GameState, Player, PossibleMoves, Move) :-     % The first priority is creating a dragon if possible
    print('Checking can make dragon...\n'),
    can_make_dragon(Player, GameState, PossibleMoves, Move), !.
choose_move_hard(GameState, _Player, PossibleMoves, Move) :-    % The second is eating a piece by custodial capture
    print('Checking can capture custodial...\n'),
    can_capture_custodial(GameState, PossibleMoves, Move), !.
choose_move_hard(GameState, _Player, PossibleMoves, Move) :-    % The third is eating a piece by capture by power
    print('Checking can capture by power...\n'),
    can_capture_by_power(GameState, PossibleMoves, Move), !.
choose_move_hard(GameState, _Player, PossibleMoves, Move) :-    % The fourth is moving to an intermediate position that will let us eat next round
    print('Finding a position to eat next round...\n'),
    can_capture_next_round(GameState, PossibleMoves, Move), !.  
choose_move_hard(GameState, _Player, PossibleMoves, Move) :-    % the fifth is removing a piece from danger
    print('Avoiding losing one of my pieces...\n'),
    avoid_being_captured(GameState, PossibleMoves, Move), !.
choose_move_hard(GameState, _Player, PossibleMoves, Move) :-    % the sixth is moving towards a dragon, in the effor to invoke one
    print('Moving towards a dragon...\n'),
    move_towards_dragon(GameState, PossibleMoves, Move), !.
choose_move_hard(_GameState, _Player, PossibleMoves, Move) :-   % Lastly, the bot chooses randomly
    random_member(Move, PossibleMoves).

% choose_move(+GameState, +Player, +Level, -Move)
% 'Move' is unified with the next player move, depending on the current difficulty 'Level'
choose_move(GameState, Player, Level, Move) :-
    valid_moves(GameState, Player, PossibleMoves),
    (Level = easy -> choose_move_easy(PossibleMoves, Move) ; (
        Level = medium -> choose_move_medium(GameState, Player, PossibleMoves, Move) ; (
            Level = hard -> choose_move_hard(GameState, Player, PossibleMoves, Move) ; print('Invalid difficulty \''), print(Level), print('\'!\n')
        )
    )).

