:- use_module(library(random)).

% valid_piece_moves(+GameBoard, +PiecePosition, -Moves)
valid_piece_moves(GameBoard, piecePosition(position(X, Y), Piece), Moves) :-
    findall(move(position(X, Y), position(X1, Y1), Piece), (
        (X1 = X ; Y1 = Y),  % has got to be orthogonal
        nth1(Y1, GameBoard, Line),
        nth1(X1, Line, empty)
    ), Moves).

valid_moves_from_pieces(_, [], Temp, Moves) :- Moves = Temp.
valid_moves_from_pieces(GameBoard, [PiecesH|PiecesT], Temp, Moves) :-
    valid_piece_moves(GameBoard, PiecesH, PieceMoves),
    append(Temp, PieceMoves, Temp1),
    valid_moves_from_pieces(GameBoard, PiecesT, Temp1, Moves).

% valid_moves(+GameState, +Player, -ListOfMoves)
valid_moves(game_state(_, _, GameBoard), Player, ListOfMoves) :-
    findall(piecePosition(position(X, Y), dice(Player, N)), (
        nth1(Y, GameBoard, Line),
        nth1(X, Line, dice(Player, N))
    ), PlayersPieces),
    valid_moves_from_pieces(GameBoard, PlayersPieces, [], ListOfMoves).

choose_move_easy(PossibleMoves, Move) :-
    random_member(Move, PossibleMoves).


check_pieces_are_player_or_empty([], _Player).
check_pieces_are_player_or_empty([dice(Player, _)|T], Player) :-
    check_pieces_are_player_or_empty(T, Player).
check_pieces_are_player_or_empty([empty|T], Player) :-
    check_pieces_are_player_or_empty(T, Player).

get_pieces_orthogonal_to_position(Position, GameBoard, L) :-
    (getPieceWithXYOffset(Position, 1, 0, GameBoard, piecePosition(_, RightOfPiece)) ;true),
	(getPieceWithXYOffset(Position, -1, 0, GameBoard, piecePosition(_, LeftOfPiece)) ;true),
	(getPieceWithXYOffset(Position, 0, 1, GameBoard, piecePosition(_, BottomOfPiece)) ;true),
	(getPieceWithXYOffset(Position, 0, -1, GameBoard, piecePosition(_, TopOfPiece)) ;true),
    
	build_valid_list([RightOfPiece, LeftOfPiece, BottomOfPiece, TopOfPiece], [], L).

get_pieces_with_offset_orthogonal_to_position(Position, GameBoard, L) :-
    (getPieceWithXYOffset(Position, 1, 0, GameBoard, piecePosition(_, RightOfPiece)) ;true),
	(getPieceWithXYOffset(Position, -1, 0, GameBoard, piecePosition(_, LeftOfPiece)) ;true),
	(getPieceWithXYOffset(Position, 0, 1, GameBoard, piecePosition(_, BottomOfPiece)) ;true),
	(getPieceWithXYOffset(Position, 0, -1, GameBoard, piecePosition(_, TopOfPiece)) ;true),
    
	build_valid_list([pieceXYOffset(xyOffset(1, 0), RightOfPiece), pieceXYOffset(xyOffset(-1, 0), LeftOfPiece), pieceXYOffset(xyOffset(0, 1), BottomOfPiece), pieceXYOffset(xyOffset(0, -1), TopOfPiece)], [], L).

available_blank_orthogonal_spots(Player, GameBoard, PiecePosition, N) :-
    get_pieces_orthogonal_to_position(PiecePosition, GameBoard, L), !,

    check_pieces_are_player_or_empty(L, Player),

    %print('L: '), print(L), nl, print('PiecePosition: '), print(PiecePosition), nl,
    count(empty, L, N).

can_make_dragon(_Player, _GameState, [], _Move) :- !, fail.
can_make_dragon(Player, game_state(_Player, _NPieces, GameBoard), [move(position(X1, Y1), position(X2, Y2), Piece)|_MovesT], Move) :-
    % verifies that the end position is beside a dragon
    (Y2 = 5 -> ( X2 = 2 ; X2 = 4 ; X2 = 6 ; X2 = 8) ;
    ((Y2 = 6 ; Y2 = 4) -> (X2 = 1 ; X2 = 5 ; X2 = 9) ; fail)),
    % finds the destination dragon
    (
        ((X2 = 2; X2 = 1), DragonX = 1, DragonY = 5) 
        ; 
        ((X2 = 4 ; X2 = 5; X2 = 6), DragonX = 5, DragonY = 5) 
        ; 
        ((X2 = 8; X2 = 9), DragonX = 9, DragonY = 5)
    ),
    ((X1 =:= DragonX-1 ; X1 =:= DragonX+1) -> Y1 \= DragonY; (X1 = DragonX -> (Y1 =\= DragonY+1, Y1 =\= DragonY-1) ; true)),    % checks that the piece to move doesn't affect the destination dragon

    %print('Dragon: '), print(DragonX), print(' '), print(DragonY), nl,
	nth1(DragonY, GameBoard, Row),
	nth1(DragonX, Row, DragonPiece),
    DragonPiece = dragonCave(empty),
    available_blank_orthogonal_spots(Player, GameBoard, position(DragonX, DragonY), N),
    N = 1, !,
    Move = move(position(X1, Y1), position(X2, Y2), Piece), print('Can make dragon!\n'), wait_for_user_input.
can_make_dragon(Player, GameState, [_MovesH|MovesT], Move) :-
    can_make_dragon(Player, GameState, MovesT, Move).


    

check_custodial_capture(move(MoveStartPosition, MoveEndPosition, dice(Player, _Strength)), xyOffset(MiddleX, MiddleY), xyOffset(OppositeX, OppositeY), GameBoard) :-
    %print('- check_custodial_capture\n'),
    getPieceWithXYOffset(MoveEndPosition, MiddleX, MiddleY, GameBoard, piecePosition(MiddlePiecePosition, MiddlePieceDice)),
    getPieceWithXYOffset(MoveEndPosition, OppositeX, OppositeY, GameBoard, piecePosition(OppositePiecePosition, OppositePieceDice)),

    MiddlePiecePosition \= MoveStartPosition,      
    OppositePiecePosition \= MoveStartPosition,

    %print('Middle piece position: '), print(MiddlePiecePosition), print(' '), print(MiddlePieceDice), nl,
    %print('Opposite piece position: '), print(OppositePiecePosition), print(' '), print(OppositePieceDice), nl,

	get_value_from_dice(MiddlePieceDice, MiddlePiece, _MiddleStrength),

	get_value_from_dice(OppositePieceDice, OppositePiece, _OppositeStrength),

    %print('MiddlePiece: '), print(MiddlePiece), nl,
    %print('OppositePiece: '), print(OppositePiece), nl,
    
    get_opposite_type(Player, MiddlePiece), % middle piece has to be opposite from player

    (
        OppositePiece = Player ;
        OppositePiece = mountain ;
        OppositePiece = dragonCave(empty) ;
        OppositePiece = dragonCave(invoked)
    ), !.


can_capture_custodial(_GameState, [], _Move) :- !, fail.
can_capture_custodial(game_state(_Player, _Npieces, GameBoard), [Move|_MovesT], OutMove) :-
    %print('- - can_capture_custodial\n'),
    (
        check_custodial_capture(Move, xyOffset(1, 0), xyOffset(2, 0), GameBoard) ;
        check_custodial_capture(Move, xyOffset(-1, 0), xyOffset(-2, 0), GameBoard) ;
        check_custodial_capture(Move, xyOffset(0, 1), xyOffset(0, 2), GameBoard) ;
        check_custodial_capture(Move, xyOffset(0, -1), xyOffset(0, -2), GameBoard)
    ), !,
    %print(MoveStartPosition), nl,
    %print(MoveEndPosition), nl,
    %print(Piece), nl,
    print('Can capture by custodial!\n'), wait_for_user_input,
    OutMove = Move.

can_capture_custodial(game_state(_Player, _Npieces, GameBoard), [_MovesH|MovesT], Move) :-
    can_capture_custodial(game_state(_Player, _Npieces, GameBoard), MovesT, Move).
    


piece_can_capture_by_power(_Eats, []) :- !, fail.
piece_can_capture_by_power(dice(PlayerEats, StrengthEats), [dice(PlayerEaten, StrengthEaten)|_T]) :-
    get_opposite_type(PlayerEats, PlayerEaten),
    StrengthEats > StrengthEaten, !.
piece_can_capture_by_power(Eats, [_EatenH|EatenT]) :-
    piece_can_capture_by_power(Eats, EatenT).

can_capture_by_power(_GameState, [], _Move) :- !, fail.
can_capture_by_power(game_state(_Player, _NPieces, GameBoard), [move(StartPosition, EndPosition, Piece)|_MovesT], Move) :-
    get_pieces_orthogonal_to_position(EndPosition, GameBoard, PiecesNextToEnd),
    piece_can_capture_by_power(Piece, PiecesNextToEnd), !,
    Move = move(StartPosition, EndPosition, Piece), print('Can capture by power!\n'), wait_for_user_input.
can_capture_by_power(GameState, [_MovesH|MovesT], Move) :-
    can_capture_by_power(GameState, MovesT, Move).



choose_move_medium(GameState, Player, PossibleMoves, Move) :-
    print('Checking can make dragon...\n'),
    can_make_dragon(Player, GameState, PossibleMoves, Move), !.
choose_move_medium(GameState, _Player, PossibleMoves, Move) :-
    print('Checking can capture custodial...\n'),
    can_capture_custodial(GameState, PossibleMoves, Move), !.
choose_move_medium(GameState, _Player, PossibleMoves, Move) :-
    print('Checking can capture by power...\n'),
    can_capture_by_power(GameState, PossibleMoves, Move), !.
choose_move_medium(_GameState, _Player, PossibleMoves, Move) :-
    random_member(Move, PossibleMoves), !.


count_pieces_with_type_in_direction(GameBoard, OffenderType, position(X, Y), Offset, Count) :-
    count_pieces_with_type_in_direction(GameBoard, OffenderType, position(X, Y), Offset, Count, -1).
count_pieces_with_type_in_direction(GameBoard, OffenderType, position(X, Y), xyOffset(XOffsetTemp, _YOffsetTemp), Count, Strength) :-
    XOffsetTemp = 0 -> (XOffset = 0, YOffset = 1, StartX = X, StartY = 1) ; (XOffset = 1 , YOffset = 0, StartX = 1, StartY = Y),
    count_pieces_with_type_in_direction_helper(GameBoard, OffenderType, position(StartX, StartY), xyOffset(XOffset, YOffset), Count, Strength).

count_pieces_with_type_in_direction_helper(_GameBoard, _OffenderType, position(X, Y), _Offset, Count, _Strength) :-
    (X < 1; X > 9; Y < 1; Y > 9), Count = 0,
    print('FINALIZER\n').
count_pieces_with_type_in_direction_helper(GameBoard, OffenderType, position(X, Y), Offset, Count, Strength) :-
    print('Calculating count\n'),
    nth1(Y, GameBoard, Line),
    nth1(X, Line, Piece),
    get_value_from_dice(Piece, Type, OffendingStrength),
    OffenderType = Type, OffendingStrength > Strength, !,
    count_pieces_with_type_in_direction_pos(position(X, Y), Offset, NextPosition),
    count_pieces_with_type_in_direction_helper(GameBoard, OffenderType, NextPosition, Offset, CountTemp, Strength),
    format('CountTemp: ~w\n', [CountTemp]),
    Count is CountTemp+1.
count_pieces_with_type_in_direction_helper(GameBoard, OffenderType, position(X, Y), Offset, Count, Strength) :-
    count_pieces_with_type_in_direction_pos(position(X, Y), Offset, NextPosition),
    format('Current position: ~w, nextposition: ~w, count: ~w\n', [position(X, Y), NextPosition, Count]),
    count_pieces_with_type_in_direction_helper(GameBoard, OffenderType, NextPosition, Offset, Count, Strength).

count_pieces_with_type_in_direction_pos(position(X, Y), xyOffset(0, 1), position(X, NextY)) :-
    NextY is Y+1.
count_pieces_with_type_in_direction_pos(position(X, Y), xyOffset(1, 0), position(NextX, Y)) :-
    NextX is X+1.

can_be_captured(piecePosition(Position, dice(Piece, Strength)), GameBoard) :-
    get_pieces_with_offset_orthogonal_to_position(Position, GameBoard, NearPieces),

    member(pieceXYOffset(xyOffset(OffendingXOffset, OffendingYOffset), empty), NearPieces),
    get_opposite_type(Piece, OffenderType),

    count_pieces_with_type_in_direction(GameBoard, OffenderType, Position, xyOffset(OffendingXOffset, OffendingYOffset), Count, Strength), Count >= 2, !,  % has got to have more strength
    format('Piece at ~w can be captured by power by someone\n', [Position]).

can_be_captured(piecePosition(Position, dice(Piece, _Value)), GameBoard) :-
    get_pieces_with_offset_orthogonal_to_position(Position, GameBoard, NearPieces),
    member(pieceXYOffset(xyOffset(OffendingXOffset, OffendingYOffset), OffendingPiece), NearPieces), 
    OffendingPiece \= empty, OffendingPiece \= dice(Piece, _SomeValue),
    get_opposite_type(Piece, OffenderType),
    count_pieces_with_type_in_direction(GameBoard, OffenderType, Position, xyOffset(OffendingXOffset, OffendingYOffset), Count), Count >= 1, !,
    format('Piece at ~w can be captured by custody by someone\n', [Position]).

verify_can_capture_next_round(GameState, Moves) :-
    print('Verifying custodial capture\n'),
    can_capture_custodial(GameState, Moves, _CustodialMove), !.
verify_can_capture_next_round(GameState, Moves) :-
    print('Verifying power capture\n'),
    can_capture_by_power(GameState, Moves, _PowerMove), !.

can_capture_next_round(_GameState, [], _Move) :- !, fail.
can_capture_next_round(game_state(Player, NPieces, GameBoard), [move(StartPosition, EndPosition, Piece)|_MovesT], Move) :-
    valid_piece_moves(GameBoard, piecePosition(EndPosition, Piece), Moves),
    print(Moves), nl,
    verify_can_capture_next_round(game_state(Player, NPieces, GameBoard), Moves),
    % \+ can_be_captured(piecePosition(EndPosition, Piece), GameBoard),
    Move = move(StartPosition, EndPosition, Piece),
    print('- - Can eat next round!\n'),
    print(Move), nl,
    wait_for_user_input.

can_capture_next_round(GameState, [_MovesH|MovesT], Move) :-
    can_capture_next_round(GameState, MovesT, Move), !.

avoid_being_captured(_GameState, [], _Move) :- !, fail.
avoid_being_captured(game_state(_Player, _NPieces, GameBoard), [move(StartPosition, EndPosition, Piece)|_MovesT], Move) :-
    can_be_captured(piecePosition(StartPosition, Piece), GameBoard),
    \+ can_be_captured(piecePosition(EndPosition, Piece), GameBoard), !,
    Move = move(StartPosition, EndPosition, Piece), print('avoided being captured!\n'), wait_for_user_input.

avoid_being_captured(GameState, [_MovesH|MovesT], Move) :-
    avoid_being_captured(GameState, MovesT, Move), !.
   
% A melhor jogada
% 1º se for possível fazer um dragão, fazer.
% 2º se for possível comer por captura custodial, comer.
% 3º se for possível comer por captua por poder, comer.
% 4º mover para uma posição que torne possível comer na próxima ronda. 
% 5º evitar que uma peça seja comida.
% 6º mover para próximo de um dragão.
%choose_move_hard(GameState, Player, PossibleMoves, Move) :-
%    print('Checking can make dragon...\n'),
%    can_make_dragon(Player, GameState, PossibleMoves, Move), !.
%choose_move_hard(GameState, Player, PossibleMoves, Move) :-
%    print('Checking can capture custodial...\n'),
%    can_capture_custodial(Player, GameState, PossibleMoves, Move), !.
%choose_move_hard(GameState, _Player, PossibleMoves, Move) :-
%    print('Checking can capture by power...\n'),
%    can_capture_by_power(GameState, PossibleMoves, Move), !.
%choose_move_hard(GameState, _Player, PossibleMoves, Move) :-
%    print('Finding a position to eat next round...\n'),
%    can_capture_next_round(GameState, PossibleMoves, Move), !.
choose_move_hard(GameState, _Player, PossibleMoves, Move) :-
    print('Avoiding losing one of my pieces...\n'),
    avoid_being_captured(GameState, PossibleMoves, Move), !.
%choose_move_hard(GameState, _Player, PossibleMoves, Move) :-
%    print('Moving towards a dragon...\n'),
%    can_capture_by_power(GameState, PossibleMoves, Move), !.
choose_move_hard(_GameState, _Player, PossibleMoves, Move) :-
    random_member(Move, PossibleMoves).

% choose_move(+GameState, +Player, +Level, -Move)
choose_move(GameState, Player, Level, Move) :-
    valid_moves(GameState, Player, PossibleMoves),
    (Level = easy -> choose_move_easy(PossibleMoves, Move) ; (
        Level = medium -> choose_move_medium(GameState, Player, PossibleMoves, Move) ; (
            Level = hard -> choose_move_hard(GameState, Player, PossibleMoves, Move) ; print('Invalid difficulty \''), print(Level), print('\'!\n')
        )
    )).

