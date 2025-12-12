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