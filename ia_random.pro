iaRandom(Board, NewBoard, Player) :-
    findall(Column, validMove(Column), ValidMoves),
    ValidMoves \= [],
    length(ValidMoves, Len),
    random(0, Len, Index),
    nth0(Index, ValidMoves, ChosenColumn),
    playMove(Board, ChosenColumn, NewBoard, Player).