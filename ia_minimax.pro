% Minimax AI implementation
iaMinimax(Board, NewBoard, Player) :-
    minimax(Board, 1, Player, BestCol, _),
    playMove(Board, BestCol, NewBoard, Player).