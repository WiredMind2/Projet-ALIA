iaPresqueRandom(Board, NewBoard, Player) :-
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
        findall(Col, validMove(Col), ValidMoves2),
        length(ValidMoves2, Len),
        random(0, Len, Index),
        nth0(Index, ValidMoves2, ChosenColumn),
        playMove(Board, ChosenColumn, NewBoard, Player)
    ).