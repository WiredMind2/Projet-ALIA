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
    (   catch(play('x'), Error, (handle_error(Error), fail))
    ->  true
    ;   writeln('Game ended unexpectedly.'), main_menu
    ).
handle_choice(2) :-
    write('Starting Player vs IA (you are x)...'), nl,
    (   catch(play_pvai('x'), Error, (handle_error(Error), fail))
    ->  true
    ;   writeln('Game ended unexpectedly.'), main_menu
    ).
handle_choice(3) :-
    write('Starting IA vs IA...'), nl,
    (   catch(play_aivai('x'), Error, (handle_error(Error), fail))
    ->  true
    ;   writeln('Game ended unexpectedly.'), main_menu
    ).
handle_choice(q) :-
    writeln('Goodbye.').
handle_choice(_) :-
    writeln('Invalid choice, try again.'),
    main_menu.

handle_error(Error) :-
    format('Error occurred: ~w~n', [Error]),
    writeln('Returning to main menu...'),
    main_menu.