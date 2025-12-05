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

get_item_2d(Matrix, Row, Col, Value) :- nth0(Row, Matrix, TheRow), nth0(Col, TheRow, Value).

horizontal_win(Board, M) :-
    between(0,5,Row),
    between(0,3,StartCol),
    get_item_2d(Board, Row, StartCol, M),
    C2 is StartCol + 1, get_item_2d(Board, Row, C2, M),
    C3 is StartCol + 2, get_item_2d(Board, Row, C3, M),
    C4 is StartCol + 3, get_item_2d(Board, Row, C4, M).

vertical_win(Board, M) :-
    between(0,6,Col),
    between(0,2,StartRow),
    get_item_2d(Board, StartRow, Col, M),
    R2 is StartRow + 1, get_item_2d(Board, R2, Col, M),
    R3 is StartRow + 2, get_item_2d(Board, R3, Col, M),
    R4 is StartRow + 3, get_item_2d(Board, R4, Col, M).

diagonal1_win(Board, M) :-  % / diagonal
    between(0,2,StartRow),
    between(3,6,StartCol),
    get_item_2d(Board, StartRow, StartCol, M),
    R2 is StartRow + 1, C2 is StartCol - 1, get_item_2d(Board, R2, C2, M),
    R3 is StartRow + 2, C3 is StartCol - 2, get_item_2d(Board, R3, C3, M),
    R4 is StartRow + 3, C4 is StartCol - 3, get_item_2d(Board, R4, C4, M).

diagonal2_win(Board, M) :-  % \ diagonal
    between(0,2,StartRow),
    between(0,3,StartCol),
    get_item_2d(Board, StartRow, StartCol, M),
    R2 is StartRow + 1, C2 is StartCol + 1, get_item_2d(Board, R2, C2, M),
    R3 is StartRow + 2, C3 is StartCol + 2, get_item_2d(Board, R3, C3, M),
    R4 is StartRow + 3, C4 is StartCol + 3, get_item_2d(Board, R4, C4, M).

win(Board, M) :-
    (horizontal_win(Board, M) ;
     vertical_win(Board, M) ;
     diagonal1_win(Board, M) ;
     diagonal2_win(Board, M)),
    M \= '.'.

game_over(Board, Result) :-
    (win(Board, M) -> Result = M ;
     (\+ (member(Row, Board), member('.', Row)) -> Result = 'draw' ;
      Result = 'no')).

available_moves(Board, Moves) :-
    findall(Col, (between(0,6,Col), get_item_2d(Board, 0, Col, '.')), Moves).