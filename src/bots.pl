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


    

check_custodial_capture(Player, move(MoveStartPosition, MoveEndPosition), xyOffset(MiddleX, MiddleY), xyOffset(OppositeX, OppositeY), GameBoard) :-
    (getPieceWithXYOffset(MoveEndPosition, MiddleX, MiddleY, GameBoard, piecePosition(MiddlePiecePosition, MiddlePieceDice)) ;true),
    (getPieceWithXYOffset(MoveEndPosition, OppositeX, OppositeY, GameBoard, piecePosition(OppositePiecePosition, OppositePieceDice)) ;true),

    MiddlePiecePosition \= MoveStartPosition,
    OppositePiecePosition \= MoveStartPosition,

    ground(MiddlePieceDice),
	get_value_from_dice(MiddlePieceDice, MiddlePiece, _Strength),
    get_opposite_type(Player, PlayerToBeEaten),
    MiddlePiece = PlayerToBeEaten, 

    ground(OppositePieceDice),
	get_value_from_dice(OppositePieceDice, OppositePiece, _Strength),


    (
        OppositePiece = Player ;
        OppositePiece = mountain ;
        OppositePiece = dragonCave(empty) ;
        OppositePiece = dragonCave(invoked)
    ), !.
    %print('Opposite: '), print(OppositeFromPlayer), nl,
    %print('SecondPiece: '), print(SecondPiece), nl,
    %print('OppositePiece: '), print(OppositePiece), nl.


can_capture_custodial(_Player, _GameState, [], _Move) :- !, fail.
can_capture_custodial(Player, game_state(_Player, _Npieces, GameBoard), [move(MoveStartPosition, MoveEndPosition, Piece)|_MovesT], Move) :-
    (
        check_custodial_capture(Player, move(MoveStartPosition, MoveEndPosition), xyOffset(1, 0), xyOffset(2, 0), GameBoard) ;
        check_custodial_capture(Player, move(MoveStartPosition, MoveEndPosition), xyOffset(-1, 0), xyOffset(-2, 0), GameBoard) ;
        check_custodial_capture(Player, move(MoveStartPosition, MoveEndPosition), xyOffset(0, 1), xyOffset(0, 2), GameBoard) ;
        check_custodial_capture(Player, move(MoveStartPosition, MoveEndPosition), xyOffset(0, -1), xyOffset(0, -2), GameBoard)
    ), !,
    %print(MoveStartPosition), nl,
    %print(MoveEndPosition), nl,
    %print(Piece), nl,
    print('Can capture by custodial!\n'), wait_for_user_input,
    Move = move(MoveStartPosition, MoveEndPosition, Piece).

can_capture_custodial(Player, game_state(_Player, _Npieces, GameBoard), [_MovesH|MovesT], Move) :-
    can_capture_custodial(Player, game_state(_Player, _Npieces, GameBoard), MovesT, Move).
    


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
    can_make_dragon(Player, GameState, PossibleMoves, Move), !.
choose_move_medium(GameState, Player, PossibleMoves, Move) :-
    can_capture_custodial(Player, GameState, PossibleMoves, Move), !.
choose_move_medium(GameState, _Player, PossibleMoves, Move) :-
    can_capture_by_power(GameState, PossibleMoves, Move), !.
choose_move_medium(_GameState, _Player, PossibleMoves, Move) :-
    random_member(Move, PossibleMoves), !.

% A melhor jogada
% 1º se for possível fazer um dragão, fazer.
% 2º se for possível comer por captura custodial, comer.
% 3º se for possível comer por captua por poder, comer.
% 4º mover para uma posição que torne possível comer na próxima ronda. 
% 5º evitar que uma peça seja comida.
% 6º mover para próximo de um dragão.

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

