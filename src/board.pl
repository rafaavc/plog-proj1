%database

% board(-Board)
board([
	[mountain, dice(black, 3), dice(black, 2), dice(black, 2), dice(black, 2), dice(black, 2), dice(black, 2), dice(black, 3), mountain],
	[empty, empty, empty, empty, dice(black, 4), empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[dragonCave, empty, empty, empty, dragonCave, empty, empty, empty, dragonCave],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, dice(white, 4), empty, empty, empty, empty],
	[mountain, dice(white, 3), dice(white, 2), dice(white, 2), dice(white, 2), dice(white, 2), dice(white, 2), dice(white, 3), mountain]
]).

/*
% intermediate state
board([
	[6, 7],
	[mountain, empty, black, empty, black, empty, black, empty, mountain],
	[empty, empty, empty, empty, black, empty, empty, empty, white],
	[empty, empty, empty, empty, empty, empty, white, empty, empty],
	[empty, empty, empty, empty, black, empty, empty, empty, empty],
	[dragonCave, empty, empty, empty, dragonCave, empty, empty, empty, dragonCave],
	[white, empty, empty, black, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, black],
	[empty, empty, empty, empty, white, empty, empty, empty, empty],
	[mountain, empty, white, empty, empty, white, empty, empty, mountain]
]).
*/

/*
% final state
board([
	[1, 5],
	[mountain, empty, black, empty, empty, empty, black, empty, mountain],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, black, empty, empty, empty, empty],
	[dragonCave, empty, empty, empty, dragonCave, empty, empty, empty, dragonCave],
	[empty, empty, empty, black, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, black],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[mountain, empty, white, empty, empty, empty, empty, empty, mountain]
]).
*/

% establishes correspondence between each atom and the symbol to be displayed
% symbol(+Atom, -Symbol)
symbol(mountain, 'M').
symbol(black, 'B').
symbol(empty, ' ').
symbol(white, 'W').
symbol(dragonCave, 'D').

%displays the board's grid intermediate elements
display_board_middle_separator :-
	print('  \x251C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2500\\x2524\\n').

%displays the board's grid top line
display_board_top_separator :-
	print('    A    B    C    D    E    F    G    H    I\n'),
	print('  \x250C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2500\\x2510\ '),
	nl.	

%displays the board's grid bottom line
display_board_bottom_separator :-
	print('  \x2514\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2500\\x2518\\n').


display_board_row([], _).
display_board_row([dice(Color, Value) | T], N) :-
	(N \= -1 -> print(N); true),
	symbol(Color, ColorValue),
	print(' \x2502\ '),
	print(ColorValue),
	print(Value),
	display_board_row(T, -1).
display_board_row([Symbol | T], N) :-
	(N \= -1 -> print(N); true),
	symbol(Symbol, Value),
	print(' \x2502\ '),
	print(Value),
	print(' '),
	display_board_row(T, -1).

%display all board elements
%display_board(+GameState)
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

display_player(Player) :-
	print(Player),
	print('\'s turn.'),
	nl.

% display_players_pieces(+Pieces)
display_players_pieces(npieces(NWhitePieces, NBlackPieces)) :- 
	print('white player has '),
	print(NWhitePieces), 
	print(' pieces.'), 
	nl,
	print('black player has '),
	print(NBlackPieces),
	print(' pieces.'), 
	nl.

%displays game state
% display_game(+GameState)
display_game(game_state(Player, NPieces, GameBoard)) :-
	display_board(GameBoard, 1),
	display_players_pieces(NPieces),
	display_player(Player).




