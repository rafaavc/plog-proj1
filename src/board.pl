%database

% board(-Board)
board([
	[8, 8], % amount of pieces in-game for each player (player0 - white, player1 - black)
	[mountain, black, black, black, black, black, black, black, mountain],
	[empty, empty, empty, empty, black, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[dragonCave, empty, empty, empty, dragonCave, empty, empty, empty, dragonCave],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, white, empty, empty, empty, empty],
	[mountain, white, white, white, white, white, white, white, mountain]
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
display_board_line :-
	print('  \x251C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2524\\n').

%displays the board's grid top line
display_board_top_line :-
	print('    1   2   3   4   5   6   7   8   9\n'),
	print('  \x250C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2510\ '),
	nl.	

%displays the board's grid bottom line
display_board_bottom_line :-
	print('  \x2514\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2518\\n').


display_board_row([], _).
display_board_row([H | T], N) :-
	(N \= -1 -> print(N); true),
	symbol(H, Value),
	print(' \x2502\ '),
	print(Value),
	display_board_row(T, -1).

%display all board elements
%display_board(+GameState)
display_board([H | []], N) :-
	display_board_row(H, N),
	print(' \x2502\\n'),
	display_board_bottom_line.

display_board([H | T], N) :-
	display_board_row(H, N),
	print(' \x2502\\n'),
	display_board_line,
	N1 is N+1,
	display_board(T, N1).

display_player(Player) :-
	print('Player '),
	print(Player),
	print('\'s turn.'),
	nl.

% display_players_pieces(+Pieces)
display_players_pieces([P0, P1 | _]) :- 
	print('Player 0 has '),
	print(P0), 
	print(' pieces.'), 
	nl,
	print('Player 1 has '),
	print(P1),
	print(' pieces.'), 
	nl.

%displays game state
% display_game(+GameState, +Player)
display_game([H | T], Player) :-
	display_board_top_line,
	display_board(T, 1),
	display_players_pieces(H),
	display_player(Player).




