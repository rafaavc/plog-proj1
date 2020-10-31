%database

board([
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
board([
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
board([
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

symbol(mountain, 'M').
symbol(black, 'B').
symbol(empty, ' ').
symbol(white, 'W').
symbol(dragonCave, 'D').

%displays the board's grid intermediate elements
display_board_line :-
	print('\x251C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2524\\n').

%displays the board's grid top line
display_board_top_line :-
	print('\x250C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2510\ ').	

%displays the board's grid bottom line
display_board_bottom_line :-
	print('\x2514\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2518\ ').

%display each board row
display_board_row([]) :- 
	print('\x2502\\n'),
	display_board_line.

display_board_row([H | T]) :-
	symbol(H, Value),
	print('\x2502\ '),
	print(Value),
	print(' '),
	display_board_row(T).

%display board last row
display_board_edge_row([]) :-
	print('\x2502\\n'),
	display_board_bottom_line.

display_board_edge_row([H | T]) :-
	symbol(H, Value),
	print('\x2502\ '),
	print(Value),
	print(' '),
	display_board_edge_row(T).

%display all board elements
display_board([H | []], _) :-
	display_board_edge_row(H).

display_board([H | T], _) :-
	display_board_row(H),
	display_board(T, _).	

%displays game state
display_game([H | T], _) :-
	display_board_top_line,
	nl,
	display_board([H | T], _).
