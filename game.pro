:- dynamic board/1.
:- dynamic last_index/1.

:- consult('matrix.pro').
:- consult('win.pro').

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

setup :- 
    retractall(board(_)),
    retractall(last_index(_)),
    generate_matrix(7,6,Board),
    length_list(7, Indices),
    maplist(=(0), Indices),
    assert(last_index(Indices)),
    assert(board(Board)).
