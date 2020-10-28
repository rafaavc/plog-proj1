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


symbol(mountain, 'M').
symbol(black, 'B').
symbol(empty, ' ').
symbol(white, 'W').
symbol(dragonCave, 'D').

display_board_line :-
	print('\x251C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x253C\\x2500\\x2500\\x2500\\x2524\\n').

display_board_top_line :-
	print('\x250C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x252C\\x2500\\x2500\\x2500\\x2510\').	

display_board_bottom_line :-
	print('\x2514\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2534\\x2500\\x2500\\x2500\\x2518\').

display_board_row([]) :- 
	print('\x2502\\n'),
	display_board_line.

display_board_row([H | T]) :-
	symbol(H, Value),
	print('\x2502\ '),
	print(Value),
	print(' '),
	display_board_row(T).

display_board_edge_row([]) :-
	print('\x2502\\n'),
	display_board_bottom_line.

display_board_edge_row([H | T]) :-
	symbol(H, Value),
	print('\x2502\ '),
	print(Value),
	print(' '),
	display_board_edge_row(T).

display_board([H | []], _) :-
	display_board_edge_row(H).

display_board([H | T], _) :-
	display_board_row(H),
	display_board(T, _).	

display_game([H | T], _) :-
	display_board_top_line,
	nl,
	display_board([H | T], _).
