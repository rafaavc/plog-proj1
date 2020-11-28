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

% choose_move(+GameState, +Player, +Level, -Move)
choose_move(GameState, Player, Level, Move) :-
    valid_moves(GameState, Player, PossibleMoves),
    (Level = easy -> choose_move_easy(PossibleMoves, Move) ; (
        Level = medium -> true; /*choose_move_medium(GameState, Player, PossibleMoves, Move) ;*/ (
            Level = hard -> true; /*choose_move_hard(GameState, Player, PossibleMoves, Move) ;*/ print('Invalid difficulty \''), print(Level), print('\'!\n')
        )
    ))
.

