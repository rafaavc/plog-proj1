count(_Elem, [], 0).
count(Elem, [Elem|T], N) :-
    !, count(Elem, T, N1),
    N is N1 + 1.
count(Elem, [_|T], N) :-
    !, count(Elem, T, N).



