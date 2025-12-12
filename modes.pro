:- consult('game.pro').
:- consult('print.pro').
:- consult('ia.pro').

play(Player):-  
    write('New turn for: '),
    print_player(Player),
    nl,
    board(Board),
    print_board(Board),
    playHumanMove(Board, NewBoard, Player),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        print_board(NewBoard),
        ( Result = 'draw' -> writeln('It''s a draw!') ; write('Player '), print_player(Result), writeln(' wins!') ),
        !
    ;
        changePlayer(Player,NextPlayer),
        play(NextPlayer)
    ).

play_pvai(Player) :-  % Player 'x' = human, 'o' = IA
    write('New turn for: '), print_player(Player), nl,
    board(Board),
    print_board(Board),
    ( Player = 'x' ->
        playHumanMove(Board, NewBoard, Player)
    ;
        iaPresqueRandom(Board, NewBoard, Player)
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        print_board(NewBoard),
        ( Result = 'draw' -> writeln('It''s a draw!') ; write('Player '), print_player(Result), writeln(' wins!') )
    ;
        changePlayer(Player,NextPlayer),
        play_pvai(NextPlayer)
    ).

play_aivai(Player) :-  % Player 'x' = IA, 'o' = IA
    write('New turn for: '), print_player(Player), nl,
    board(Board),
    print_board(Board),
    iaRandom(Board, NewBoard, Player),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        print_board(NewBoard),
        ( Result = 'draw' -> writeln('It''s a draw!') ; write('Player '), print_player(Result), writeln(' wins!') )
    ;
        changePlayer(Player,NextPlayer),
        play_aivai(NextPlayer)
    ).

playHumanMove(Board,NewBoard,Player) :-
    read(Col),
    (   integer(Col), Col >= 1, Col =< 7
    ->  ColIndex is Col - 1,
        (   validMove(ColIndex)
        ->  last_index(LastIndex),
            playMove(Board, ColIndex, TmpBoard, Player, LastIndex, NewLastIndex),
            NewBoard = TmpBoard,
            applyLastIndex(LastIndex, NewLastIndex),
            write('Dropping in column '), write(Col), nl,
            write('Updated Indices: '), writeln(NewLastIndex),
            true
        ;   writeln('Column is full, pick another.'),
            playHumanMove(Board,NewBoard,Player)
        )
    ;   writeln('Invalid input, enter a number between 1 and 7.'),
        playHumanMove(Board,NewBoard,Player)
    ).
