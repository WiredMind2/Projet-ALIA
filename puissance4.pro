% Connect Four (Puissance 4) main game file

:- consult('game.pro').
:- consult('modes.pro').
:- consult('tournament.pro').
:- ensure_loaded('ai_selector.pro').
:- consult('ai/game_utils.pro').

% iaMinimax wrapper for backward compatibility if needed
iaMinimax(Board, NewBoard, Player) :-
    (   catch(minimax_ai(Board, NewBoard, Player), _, fail)
    ->  true
    ;   random_ai(Board, NewBoard, Player)
    ).

start :-
    setup,
    main_menu.

main_menu :-
    writeln('Choose game mode:'),
    writeln('1 - Player vs Player'),
    writeln('2 - Player vs IA (you play first)'),
    writeln('3 - IA vs IA'),
    writeln('4 - Run Tournament'),
    writeln('q - Quit'),
    read(Choice),
    handle_choice(Choice).

% handle_choice(Choice)
% Handles the user's menu selection
% Choice: user's menu selection (1, 2, 3, or q)
handle_choice(1) :-
    write('Starting Player vs Player...'), nl,
    (   catch(play(1), Error, (handle_error(Error), fail))
    ->  true
    ;   writeln('Game ended unexpectedly.'), main_menu
    ).

handle_choice(2) :-
    write('Starting Player vs IA...'), nl,
    get_ai_for_player(2, AI),
    (   catch(play_pvai(AI, 1), Error, (handle_error(Error), fail))
    ->  true
    ;   writeln('Game ended unexpectedly.'), main_menu
    ).

handle_choice(3) :-
    write('Starting IA vs IA...'), nl,
    get_ai_for_player(1, AI1),
    get_ai_for_player(2, AI2),
    (   catch(play_aivai(AI1, AI2, 1), Error, (handle_error(Error), fail))
    ->  true
    ;   writeln('Game ended unexpectedly.'), main_menu
    ).

handle_choice(4) :-
    write('Starting Tournament...'), nl,
    (   catch(run_tournament('tournament_config.pl'), Error, (handle_error(Error), fail))
    ->  main_menu
    ;   writeln('Tournament ended unexpectedly.'), main_menu
    ).

handle_choice(q) :-
    writeln('Goodbye.').
handle_choice(_) :-                             % Invalid choice
    writeln('Invalid choice, try again.'),
    main_menu.

handle_error(Error) :-
    format('Error occurred: ~w~n', [Error]),
    writeln('Returning to main menu...'),
    main_menu.
