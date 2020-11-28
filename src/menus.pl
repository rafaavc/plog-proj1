router(WhereTo) :-
    (WhereTo = 1 -> start_PvP_game ; (
        WhereTo = 2 -> difficulty_menu(Difficulty), start_PvM_game(Difficulty) ; (
            WhereTo = 3 -> difficulty_menu(Difficulty), start_MvM_game(Difficulty) ; (
                WhereTo = 4 -> true ; print('Don\'t know how you got here...\n')
            )
        )
    )).

difficulty(1, easy).
difficulty(2, medium).
difficulty(3, hard).

difficulty_menu(Difficulty) :-
    print('\x250C\'),
    print_horizontal_line(29), 
    print('\x2510\\n'),
    print('\x2502\                             \x2502\\n'),
    print('\x2502\         DIFFICULTY          \x2502\\n'),
    print('\x2502\                             \x2502\\n'),
    print('\x2514\'),
    print_horizontal_line(29), 
    print('\x2518\\n'),
    print('[1] Easy\n'),
    print('[2] Medium\n'),
    print('[3] Hard\n'),
    print('Choose your option: '),
    get_digit_input(1, 3, Digit),
    difficulty(Digit, Difficulty).

print_horizontal_line(0).
print_horizontal_line(Size) :-
    print('\x2500\'),
    NextSize is Size -1,
    print_horizontal_line(NextSize).

main_menu :-
    print('\x250C\'),
    print_horizontal_line(29), 
    print('\x2510\\n'),
    print('\x2502\                             \x2502\\n'),
    print('\x2502\        THREE DRAGONS        \x2502\\n'),
    print('\x2502\  < the prolog board game >  \x2502\\n'),
    print('\x2502\                             \x2502\\n'),
    print('\x2514\'),
    print_horizontal_line(29), 
    print('\x2518\\n'),
    print('[1] Player versus Player\n'),
    print('[2] Player versus Machine\n'),
    print('[3] Machine versus Machine\n'),
    print('[4] Quit :(\n\n'),
    print('Choose your option: '),
    get_digit_input(1, 4, Input),
    router(Input).



