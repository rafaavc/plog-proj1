:-use_module(library(lists)).

% get_num_from_code(+Code, -Digit, +Offset)
% if Num is -1, Code does not mean a number
get_digit_from_code(Code, Digit, Offset) :-
    Aux is Code - Offset,
    ((Aux < 0 ; Aux > 9) -> Digit is -1; Digit is Aux).

% Takes a single digit number ASCII code and converts it to a digit.
% get_digit_from_num_code(+Code, -Digit)
get_digit_from_num_code(Code, Digit) :-
    get_digit_from_code(Code, Digit, 48).

% Takes an uppercase letter ASCII code and converts it to a digit.
% Example: 65 (=:= 'A') -> 0
% get_digit_from_uppercase_letter_code(+Code, -Digit)
get_digit_from_uppercase_letter_code(Code, Digit) :-
    get_digit_from_code(Code, Digit, 64).

% verify_position_input(+Line, -Position)
verify_position_input(Line, position(X, Y)) :-
    length(Line, 3),

    nth0(1, Line, C1),
    C1 =:= 44, % comma

    nth0(0, Line, C0),
    get_digit_from_uppercase_letter_code(C0, TempX),
    TempX \= -1,

    nth0(2, Line, C2),
    get_digit_from_num_code(C2, TempY), 
    TempY \= -1, 

    X is TempX, Y is TempY.  % if the verifications fail, neither X nor Y are set

% get_position_input(-Position)
get_position_input(Position) :-
    read_line(Line),
    (
        verify_position_input(Line, Position)

        -> true
        ; print('That is not a valid position input. Example: \'A,1\'.'), nl, get_position_input(Position)
    ).
% verify_digit_input(+Min, +Max, +Input, -Out)
verify_digit_input(Min, Max, Input, Out) :-
    length(Input, 1),
    nth0(0, Input, D),
    get_digit_from_num_code(D, Out),
    Out >= Min, Out =< Max.

% get_integer_input(+Min, +Max, -Input)
get_digit_input(Min, Max, Input) :-
    read_line(Line),
    (
        verify_digit_input(Min, Max, Line, Input)

        -> true
        ; print('That is not a valid option!'), nl, get_digit_input(Min, Max, Input)
    ).
