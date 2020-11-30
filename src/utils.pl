% count(+Elem, +List, -N)
% Unifies 'N' with the number of occurrences of 'Elem' in 'List'
count(_Elem, [], 0).
count(Elem, [Elem|T], N) :-
    !, count(Elem, T, N1),
    N is N1 + 1.
count(Elem, [_|T], N) :-
    !, count(Elem, T, N).


% build_valid_list(+List, +Helper, -Ret)
% Unifies Ret with all defined elements from List
build_valid_list([], Helper, Ret):-
	reverse(Helper, Ret).
build_valid_list([H|T], Helper, Ret):-
	(
		ground(H) -> build_valid_list(T, [H|Helper], Ret);
		build_valid_list(T, Helper, Ret)
	).


% print_horizontal_line(+Size)
% Prints an horizontal line spanning 'Size' characters on the screen
print_horizontal_line(0).
print_horizontal_line(Size) :-
    print('\x2500\'),
    NextSize is Size -1,
    print_horizontal_line(NextSize).
