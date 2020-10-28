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

display_board_line([]) :- nl.

display_board_line([H | T]) :-
	symbol(H, Value),
	print(Value),
	display_board_line(T).

display_game([], _).

display_game([H | T], _) :-
	display_board_line(H),
	display_game(T, _).
	
	


