:- dynamic board/1.
:- dynamic last_index/1.

:- consult('print.pro').
:- consult('matrix.pro').

iaRandom(Player, Board, NewBoard) :-
    findall(Column, validMove(Column), ValidMoves),
    random_member(ChosenColumn, ValidMoves),
    playMove(Board, ChosenColumn, NewBoard, Player).

play(Player):-  
    write('New turn for: '),
    writeln(Player),
    board(Board),
    print_board(Board),
    iaRandom(Player, Board, NewBoard),
    applyIt(Board, NewBoard),
    changePlayer(Player,NextPlayer),
    play(NextPlayer).

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

validMove(Col) :-
    between(0,6,Col),
    last_index(Indices),
    nth0(Col, Indices, Row),
    Row < 6.

applyIt(OldBoard, NewBoard):- 
    retract(board(OldBoard)), 
    assert(board(NewBoard)).

init :- 
    retractall(board(_)),
    retractall(last_index(_)),
    generate_matrix(7,6,Board),
    length_list(7, Indices),
    maplist(=(0), Indices),
    assert(last_index(Indices)),
    assert(board(Board)),
    play('x').





%---- Player Move ----
changePlayer('x', 'o').
changePlayer('o', 'x').

playHumanMove(Board,NewBoard,Player) :-
    read(Col),
    (   integer(Col), Col >= 1, Col =< 7
    ->  ColIndex is Col - 1,
        (   validMove(ColIndex)
        ->  playMove(Board, ColIndex, TmpBoard, Player),
            NewBoard = TmpBoard,
            write('Dropping in column '), write(Col), nl,
            last_index(UpdatedIdx),
            write('Updated Indices: '), writeln(UpdatedIdx)
        ;   writeln('Column is full, pick another.'),
            playHumanMove(Board,NewBoard,Player)
        )
    ;   writeln('Invalid input, enter a number between 1 and 7.'),
        playHumanMove(Board,NewBoard,Player)
    ).