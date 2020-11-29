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

choose_move_medium(GameState, Player, PossibleMoves, Move) :-
    random_member(Move, PossibleMoves).

% A melhor jogada
% 1º se for possível fazer um dragão, fazer.
% 2º se for possível comer por captura custodial, comer.
% 3º se for possível comer por captua por poder, comer.
% 4º mover para uma posição que torne possível comer na próxima ronda. 
% 5º evitar que uma peça seja comida.
% 6º mover para próximo de um dragão.

choose_move_hard(GameState, Player, PossibleMoves, Move) :-
    random_member(Move, PossibleMoves).

% choose_move(+GameState, +Player, +Level, -Move)
choose_move(GameState, Player, Level, Move) :-
    valid_moves(GameState, Player, PossibleMoves),
    (Level = easy -> choose_move_easy(PossibleMoves, Move) ; (
        Level = medium -> choose_move_medium(GameState, Player, PossibleMoves, Move) ; (
            Level = hard -> choose_move_hard(GameState, Player, PossibleMoves, Move) ; print('Invalid difficulty \''), print(Level), print('\'!\n')
        )
    )).

