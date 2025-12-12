:- use_module(library(random)).
:- consult('game.pro').

iaRandom(Board, NewBoard, Player) :-
    findall(Column, validMove(Column), ValidMoves),
    ValidMoves \= [],
    random_member(ChosenColumn, ValidMoves),
    last_index(LastIndex),
    playMove(Board, ChosenColumn, NewBoard, Player, LastIndex, NewLastIndex),
    applyLastIndex(LastIndex, NewLastIndex).

iaPresqueRandom(Board, NewBoard, Player) :-
    /** Plays randomly unless able to win or opponent about to win **/
    last_index(LastIndex),
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [],
    (   
        /** Winning move **/
        member(Col, ValidMoves),
        playMove(Board, Col, TmpBoard, Player, LastIndex, _),
        game_over(TmpBoard, Result),
        Result == Player
    ->  playMove(Board, Col, NewBoard, Player, LastIndex, NewLastIndex),
        applyLastIndex(LastIndex, NewLastIndex)
    ;   
        /** Block opponent's win **/
        changePlayer(Player, Opponent),
        member(Col, ValidMoves),
        playMove(Board, Col, TmpBoard, Opponent, LastIndex, _),
        game_over(TmpBoard, Result),
        Result == Opponent
    ->  playMove(Board, Col, NewBoard, Player, LastIndex, NewLastIndex),
        applyLastIndex(LastIndex, NewLastIndex)
    ;   
        /** Play random otherwise **/
        random_member(ChosenColumn, ValidMoves),
        playMove(Board, ChosenColumn, NewBoard, Player, LastIndex, NewLastIndex),
        applyLastIndex(LastIndex, NewLastIndex)
    ).
