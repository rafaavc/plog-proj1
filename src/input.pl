:-use_module(library(lists)).

% get_num_from_code(+Code, -Num)
% if Num is -1, Code does not mean a number
get_num_from_code(Code, Num) :-
    Aux = Code - 48,
    ((Aux < 0 ; Aux > 9) -> Num is -1; Num is Aux).

% verify_move_input(+Line, -Position)
verify_move_input(Line, position(X, Y)) :-
    length(Line, 3),

    nth0(1, Line, C1),
    C1 =:= 44,

    nth0(0, Line, C0),
    get_num_from_code(C0, TempX),
    TempX \= -1,

    nth0(2, Line, C2),
    get_num_from_code(C2, TempY), 
    TempY \= -1, 

    X is TempX, Y is TempY.  % if the verifications fail, neither X nor Y are set

% get_position_input(-Position)
get_position_input(Position) :-
    read_line(Line),
    (
        verify_move_input(Line, Position)

        -> true
        ; get_move_input(Position)
    ).
