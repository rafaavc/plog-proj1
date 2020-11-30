
% router(+WhereTo)
% Establishes correspondence between a main menu option and the associated predicate
router(WhereTo) :-
    (WhereTo = 1 -> start_pvp_game ; (
        WhereTo = 2 -> difficulty_menu(Difficulty), start_pvm_game(Difficulty) ; (
            WhereTo = 3 -> difficulty_menu(Difficulty), start_mvm_game(Difficulty) ; (
                WhereTo = 4 -> true ; print('Don\'t know how you got here...\n')
            )
        )
    )).

% game_over_router(+WhereTo)
% Establishes correspondence between a main menu option and the associated predicate
game_over_router(WhereTo) :-
    (WhereTo = 1 -> play ; (
        WhereTo = 2 -> true ; print('Don\'t know how you got here...\n')
    )).

% wait_for_user_input
% waits for the user to input something
wait_for_user_input :-
    print('Press ENTER to continue!\n'),
	read_line(_Line).

% difficulty(+Number, -Difficulty)
% Associates a number to a difficulty
difficulty(1, easy).
difficulty(2, medium).
difficulty(3, hard).

% game_over_menu(+Winner)
% Displays the game
game_over_menu(Winner) :-
    print('\x250C\'),
    print_horizontal_line(29), 
    print('\x2510\\n'),
    print('\x2502\                             \x2502\\n'),
    print('\x2502\         GAME OVER           \x2502\\n'),
    format('\x2502\        ~w wins!          \x2502\\n', [Winner]),
    print('\x2502\                             \x2502\\n'),
    print('\x2514\'),
    print_horizontal_line(29), 
    print('\x2518\\n'),
    print('[1] Play Again\n'),
    print('[2] Quit\n'),
    print('Choose your option: '),
    get_digit_input(1, 2, Digit),
    game_over_router(Digit). 

% difficulty_menu(-Difficulty)
% Prints the difficulty menu on the screen. Unifies 'Difficulty' with the chosen difficulty
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

% main_menu
% Prints the main menu on the screen. Gets the user option and calls 'router'
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



