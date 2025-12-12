:- dynamic board/1.
:- dynamic last_index/1.

:- consult('print.pro').
:- consult('matrix.pro').
:- consult('win.pro').
:- consult('ai/selector.pro').
:- consult('ai/game_utils.pro').
:- consult('ai/evaluation.pro').
:- consult('ai/minimax/minimax.pro').
:- consult('ai/random/random.pro').
:- consult('ai/random/almost_random.pro').

% Backward compatibility wrapper for iaMinimax in main game
iaMinimax(Board, NewBoard, Player) :-
    (   catch(minimax_ai(Board, NewBoard, Player), _, fail)
    ->  true
    ;   % Fallback to random if minimax fails
        random_ai(Board, NewBoard, Player)
    ).

play(Player):-  
    write('New turn for: '),
    writeln(Player),
    board(Board),
    print_board(Board),
    playHumanMove(Board, NewBoard, Player),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) ),
        !
    ;
        changePlayer(Player,NextPlayer),
        play(NextPlayer)
    ).

% Game utilities are now in ai/game_utils.pro
% Functions removed: playMove, applyBoard, applyLastIndex, changePlayer, validMove, setup
% These are now available through ai/game_utils.pro consultation

start :-
    setup,
    main_menu.

main_menu :-
    writeln('Choose game mode:'),
    writeln('1 - Player vs Player'),
    writeln('2 - Player vs IA'),
    writeln('3 - IA vs IA'),
    writeln('q - Quit'),
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :-
    write('Starting Player vs Player...'), nl,
    play('x').
handle_choice(2) :-
    write('Starting Player vs IA...'), nl,
    get_ai_for_player('x', AI),
    play_pvai(AI, 'x').
handle_choice(3) :-
    write('Starting IA vs IA...'), nl,
    get_ai_for_player('x', AI1),
    get_ai_for_player('x', AI2),
    play_aivai(AI1, AI2, 'x').
handle_choice(q) :-
    writeln('Goodbye.').
handle_choice(_) :-
    writeln('Invalid choice, try again.'),
    main_menu.

play_pvai(AI, Player) :-  % Player 'x' = human, 'o' = IA
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 'x' ->
        playHumanMove(Board, NewBoard, Player)
    ;
        ( call(AI, Board, NewBoard, Player) -> true ; (writeln('AI failed, using random move'), iaRandom(Board, NewBoard, Player)) )
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) )
    ;
        changePlayer(Player,NextPlayer),
        play_pvai(AI, NextPlayer)
    ).

play_aivai(AI1, AI2, Player) :-  % Player 'x' uses AI1, 'o' uses AI2
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 'x' ->
        ( call(AI1, Board, NewBoard, Player) -> true ; (writeln('AI1 failed, using random move'), iaRandom(Board, NewBoard, Player)) )
    ;
        ( call(AI2, Board, NewBoard, Player) -> true ; (writeln('AI2 failed, using random move'), iaRandom(Board, NewBoard, Player)) )
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) )
    ;
        changePlayer(Player,NextPlayer),
        play_aivai(AI1, AI2, NextPlayer)
    ).

play_pvai_minimax(Player) :-  % Player 'x' = human, 'o' = IA Minimax
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 'x' ->
        playHumanMove(Board, NewBoard, Player)
    ;
        iaMinimax(Board, NewBoard, Player)
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) )
    ;
        changePlayer(Player,NextPlayer),
        play_pvai_minimax(NextPlayer)
    ).

playHumanMove(Board,NewBoard,Player) :-
    read(Col),
    (   integer(Col), Col >= 1, Col =< 7
    ->  ColIndex is Col - 1,
        (   validMove(ColIndex)
        ->  playMove(Board, ColIndex, TmpBoard, Player),
            NewBoard = TmpBoard,
            write('Dropping in column '), write(Col), nl,
            last_index(UpdatedIdx),
            write('Updated Indices: '), writeln(UpdatedIdx),
            true
        ;   writeln('Column is full, pick another.'),
            playHumanMove(Board,NewBoard,Player)
        )
    ;   writeln('Invalid input, enter a number between 1 and 7.'),
        playHumanMove(Board,NewBoard,Player)
    ).