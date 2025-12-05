:- dynamic board/1.
:- dynamic last_index/1.

:- consult('print.pro').

play(Player):-  
    write('New turn for: '),
    writeln(Player),
    board(Board),
    print_board(Board),
    replaceMatrix(Board, 0, 0, Player, NewBoard), % Placeholder for actual move logic
    applyIt(Board, NewBoard),
    print_board(NewBoard).

length_list(N, List) :- length(List, N).

generate_matrix(Cols, Rows, Matrix) :-
    length_list(Rows, Matrix),
    maplist(length_list(Cols), Matrix),
    maplist(maplist(=(.)), Matrix).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

replaceMatrix(Matrix, RowIndex, ColIndex, Player, NewMatrix) :-
    nth0(RowIndex, Matrix, OldRow),
    replace(OldRow, ColIndex, Player, NewRow),
    replace(Matrix, RowIndex, NewRow, NewMatrix).

playMove(Board,Col,NewBoard,Player):-
    last_index(LastIndex),
    nth0(Col,LastIndex,Row),
    replaceMatrix(Board,Row,Col, Player,NewBoard),
    NewRow is Row+1,
    print(NewRow),
    nl,
    replace(LastIndex,Col,NewRow,NewLastIndex),
    nth0(Col,NewLastIndex,Row),
    print(Row),
    nl,
    applyLastIndex(LastIndex,NewLastIndex),
    last_index(LastIndex),
    nth0(Col,LastIndex,Row),
    print(Row),
    nl.


applyBoard(Board,NewBoard):-
    retract(board(Board)),
    assert(board(NewBoard)).

applyLastIndex(LastIndex,NewLastIndex):-
    retract(last_index(LastIndex)),
    assert(last_index(NewLastIndex)).

init :- 
    retractall(board(_)),
    retractall(last_index(_)),
    generate_matrix(7,6,Board),
    length_list(7, Indices),
    maplist(=(0), Indices),
    assert(last_index(Indices)),
    assert(board(Board)),
    play('x').

changePlayer('A', 'B').
changePlayer('B', 'A').

applyIt(OldBoard, NewBoard):- 
    retract(board(OldBoard)), 
    assert(board(NewBoard)).
