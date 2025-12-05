:- dynamic board/1.
:- dynamic last_index/1.

:- consult('print.pro').
:- consult('matrix.pro').
:- consult('win.pro').

iaRandom(Board, NewBoard, Player) :-
    findall(Column, validMove(Column), ValidMoves),
    random_member(ChosenColumn, ValidMoves),
    playMove(Board, ChosenColumn, NewBoard, Player).

play(Player):-  
    write('New turn for: '),
    writeln(Player),
    board(Board),
    print_board(Board),
    playHumanMove(Board, NewBoard, Player),
    applyIt(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) ),
        !
    ;
        changePlayer(Player,NextPlayer),
        play(NextPlayer)
    ).

playMove(Board,Col,NewBoard,Player):-
    last_index(LastIndex),
    nth0(Col,LastIndex,Row),
    Row < 6,                         
    replaceMatrix(Board,Row,Col, Player,NewBoard),
    NewRow is Row + 1,
    replace(LastIndex,Col,NewRow,NewLastIndex),
    applyLastIndex(LastIndex,NewLastIndex).


applyBoard(_OldBoard,NewBoard):-
    retractall(board(_)),
    assert(board(NewBoard)).

applyLastIndex(_OldLastIndex,NewLastIndex):-
    retractall(last_index(_)),
    assert(last_index(NewLastIndex)).

changePlayer('x', 'o').
changePlayer('o', 'x').

validMove(Col) :-
    between(0,6,Col),
    last_index(Indices),
    nth0(Col, Indices, Row),
    Row < 6.

applyIt(OldBoard, NewBoard):- 
    retract(board(OldBoard)), 
    assert(board(NewBoard)).

setup :- 
    retractall(board(_)),
    retractall(last_index(_)),
    generate_matrix(7,6,Board),
    length_list(7, Indices),
    maplist(=(0), Indices),
    assert(last_index(Indices)),
    assert(board(Board)).

init :-
    setup,
    play('x').

start :-
    setup,
    main_menu.

main_menu :-
    writeln('Choose game mode:'),
    writeln('1 - Player vs Player'),
    writeln('2 - Player vs IA (you play first)'),
    writeln('3 - IA vs Player (IA plays first)'),
    writeln('4 - IA vs IA'),
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
    write('Starting IA vs Player (IA is x)...'), nl,
    play_iavp('x').
handle_choice(4) :-
    write('Starting IA vs IA...'), nl,
    play_aivai('x').
handle_choice(q) :-
    writeln('Goodbye.').
handle_choice(_) :-
    writeln('Invalid choice, try again.'),
    main_menu.

play_pvai(Player) :-  % Player 'x' = human, 'o' = IA
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 'x' ->
        playHumanMove(Board, NewBoard, Player)
    ;
        iaRandom(Board, NewBoard, Player)
    ),
    applyIt(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) )
    ;
        changePlayer(Player,NextPlayer),
        play_pvai(NextPlayer)
    ).

play_aivai(Player) :-  % Player 'x' = IA, 'o' = IA
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    iaRandom(Board, NewBoard, Player),
    applyIt(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) )
    ;
        changePlayer(Player,NextPlayer),
        play_aivai(NextPlayer)
    ).

play_iavp(Player) :-  % Player 'x' = IA, 'o' = human
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 'x' ->
        iaRandom(Board, NewBoard, Player)
    ;
        playHumanMove(Board, NewBoard, Player)
    ),
    applyIt(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) )
    ;
        changePlayer(Player,NextPlayer),
        play_iavp(NextPlayer)
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