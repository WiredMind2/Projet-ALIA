:- consult('game.pro').
:- consult('modes.pro').

start :-
    setup,
    main_menu.

main_menu :-
    writeln('Choose game mode:'),
    writeln('1 - Player vs Player'),
    writeln('2 - Player vs IA (you play first)'),
    writeln('3 - IA vs IA'),
    writeln('q - Quit'),
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :-
    write('Starting Player vs Player...'), nl,
    play('x').
handle_choice(2) :-
    write('Starting Player vs IA (you are x)...'), nl,
    play_pvai('x').
handle_choice(3) :-
    write('Starting IA vs IA...'), nl,
    play_aivai('x').
handle_choice(q) :-
    writeln('Goodbye.').
handle_choice(_) :-
    writeln('Invalid choice, try again.'),
    main_menu.