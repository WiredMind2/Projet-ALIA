:- consult('game.pro').

iaRandom(Board, NewBoard, Player) :-
    findall(Column, validMove(Column), ValidMoves),
    ValidMoves \= [],
    random_member(ChosenColumn, ValidMoves),
    playMove(Board, ChosenColumn, NewBoard, Player).

iaPresqueRandom(Player, Board, NewBoard) :-
    /** Plays randomly unless able to win or opponent about to win **/
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [],
    (   
        /** Winning move **/
        member(Col, ValidMoves),
        playMove(Board, Col, TmpBoard, Player),
        game_over(TmpBoard, Result),
        Result == Player
    ->  playMove(Board, Col, NewBoard, Player)
    ;   
        /** Block opponent's win **/
        changePlayer(Player, Opponent),
        member(Col, ValidMoves),
        playMove(Board, Col, TmpBoard, Opponent),
        game_over(TmpBoard, Result),
        Result == Opponent
    ->  playMove(Board, Col, NewBoard, Player)
    ;   
        /** Play random otherwise **/
        random_member(ChosenColumn, ValidMoves),
        playMove(Board, ChosenColumn, NewBoard, Player)
    ).
