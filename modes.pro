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

% play_pvai(AI, Player)
% AI: The AI predicate to use for the computer player (Player 2)
play_pvai(AI, Player) :- 
    write('New turn for: '), print_player(Player), nl,
    board(Board),
    print_board(Board),
    ( Player = 1 ->
        playHumanMove(Board, NewBoard, Player)
    ;
        call(AI, Board, NewBoard, Player)
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        print_board(NewBoard),
        ( Result = 'draw' -> writeln('It''s a draw!') ; write('Player '), print_player(Result), writeln(' wins!') )
    ;
        changePlayer(Player,NextPlayer),
        play_pvai(AI, NextPlayer)
    ).

% play_aivai(AI1, AI2, Player)
play_aivai(AI1, AI2, Player) :- 
    write('New turn for: '), print_player(Player), nl,
    board(Board),
    print_board(Board),
    ( Player = 1 ->
        call(AI1, Board, NewBoard, Player)
    ;
        call(AI2, Board, NewBoard, Player)
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        print_board(NewBoard),
        ( Result = 'draw' -> writeln('It''s a draw!') ; write('Player '), print_player(Result), writeln(' wins!') )
    ;
        changePlayer(Player,NextPlayer),
        play_aivai(AI1, AI2, NextPlayer)
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
