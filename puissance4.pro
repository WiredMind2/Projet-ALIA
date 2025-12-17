:- consult('game.pro').
:- consult('modes.pro').
:- consult('tournament.pro').

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

handle_choice(1) :-
    write('\nStarting Player vs Player...'), nl,
    play('x').
handle_choice(2) :-
    ai_menu.

handle_choice(3) :-
    write('Starting IA vs IA...'), nl,
    play_aivai('x').

handle_choice(4) :-
    write('Starting Tournament...'), nl,
    run_tournament('tournament_config.pl').

handle_choice(q) :-
    writeln('Goodbye.').

handle_choice(_) :-
    writeln('Invalid choice, try again.'),
    main_menu.

ai_menu :-
    writeln('\nChoose IA type:'),
    writeln('1 - Random'),
    writeln('2 - Better Random (takes winning move and avoids losing moves)'),
    writeln('3 - Minimax'),
    writeln('4 - Minimax with Alpha-Beta Pruning'),
    writeln('q - Back to main menu'),
    read(Choice),
    handleAiTypeChoice(Choice).

handleAiTypeChoice(1) :-
    writeln('You selected Random IA.'),
    start_pvai_game('random', _, _).

handleAiTypeChoice(2) :-
    writeln('You selected Better Random IA.'),
    start_pvai_game('better_random', _, _).

handleAiTypeChoice(3) :-
    writeln('Choose Minimax depth (between 1 and 7):'),
    read(Depth),
    (   integer(Depth), Depth >= 1, Depth =< 7 )
    ->  write('You selected Minimax IA with depth '), write(Depth), writeln('.'),
        start_pvai_game('minimax', Depth, false)
    ;   writeln('Invalid depth, try again.'), ai_menu.

handleAiTypeChoice(4) :-
    writeln('Choose Minimax depth (between 1 and 7):'),
    read(Depth),
    (   integer(Depth), Depth >= 1, Depth =< 7 )
    ->  write('You selected Minimax with Alpha-Beta Pruning IA with depth '), write(Depth), writeln('.'),
        start_pvai_game('minimax', Depth, true)
    ;   writeln('Invalid depth, try again.'), ai_menu.

handleAiTypeChoice(q) :-
    main_menu.

handleAiTypeChoice(_) :-
    writeln('Invalid choice, try again.'),
    ai_menu.

start_pvai_game(IAType, Depth, UseAlphaBeta) :-
    write('Starting Player vs IA (you go first)...'), nl,
    play_pvai('x', IAType, Depth, UseAlphaBeta).

handle_error(Error) :-
    format('Error occurred: ~w~n', [Error]),
    writeln('Returning to main menu...'),
    main_menu.