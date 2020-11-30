% get_num_from_code(+Code, -Digit, +Offset)
% if Digit is -1, Code does not mean a number
get_digit_from_code(Code, Digit, Offset) :-
    Aux is Code - Offset,
    ((Aux < 0 ; Aux > 9) -> Digit is -1; Digit is Aux).


% get_digit_from_num_code(+Code, -Digit)
% Takes a single digit number ASCII code and converts it to a digit.
get_digit_from_num_code(Code, Digit) :-
    get_digit_from_code(Code, Digit, 48).


% get_digit_from_uppercase_letter_code(+Code, -Digit)
% Takes an uppercase letter ASCII code and converts it to a digit.
% Example: 65 (=:= 'A') -> 0
get_digit_from_uppercase_letter_code(Code, Digit) :-
    get_digit_from_code(Code, Digit, 64).


% verify_position_input(+Line, -Position)
% Verifies that a given 'Line' is valid position input (form A,1 -> LetterCommaNumber), unifying 'Position' with the position input
% Else fails
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
% Unifies 'Position' with valid position input
get_position_input(Position) :-
    read_line(Line),
    (
        verify_position_input(Line, Position)

        -> true
        ; print('That is not a valid position input. Example: \'A,1\'.'), nl, get_position_input(Position)
    ).


% get_desired_position_input(-Position, +GameBoard, +DesiredOccupant, -Piece)
% Asks user for a valid position that may correspond to the piece he wants to move or to the piece destination
get_desired_position_input(position(X1, Y1), GameBoard, DesiredOccupant, Piece):-
	get_position_input(position(XTemp, YTemp)),
	(
		(nth1(YTemp, GameBoard, YLine), (nth1(XTemp, YLine, dice(DesiredOccupant, _Value)); nth1(XTemp, YLine, DesiredOccupant)))
			-> (X1 = XTemp, Y1 = YTemp, nth1(XTemp, YLine, Piece))
			; (print('The coordinates you inserted are not \''), print(DesiredOccupant), print('\'.\n'), get_desired_position_input(position(X1, Y1), GameBoard, DesiredOccupant, Piece))
	).


% verify_digit_input(+Min, +Max, +Input, -Out)
% Succeeds if 'Input' is a digit between Min and Max (including), unifying 'Out' with the digit
verify_digit_input(Min, Max, Input, Out) :-
    length(Input, 1),
    nth0(0, Input, D),
    get_digit_from_num_code(D, Out),
    Out >= Min, Out =< Max.


% get_digit_input(+Min, +Max, -Input)
% Unifies 'Input' with a valid digit between Min and Max (including)
get_digit_input(Min, Max, Input) :-
    read_line(Line),
    (
        verify_digit_input(Min, Max, Line, Input)

        -> true
        ; print('That is not a valid option!'), nl, get_digit_input(Min, Max, Input)
    ).
