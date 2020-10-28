
:- consult('board.pl').


initial(GameState) :-
	board(GameState).

getPlayer(CurrentPlayer) :-
	CurrentPlayer is 0.

play :-
	initial(GameState),
	getPlayer(CurrentPlayer),
	display_game(GameState, CurrentPlayer).



