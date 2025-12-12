% Smart random AI implementation
% This AI plays randomly unless it can win or needs to block an opponent's win

:- consult('../game_utils.pro').

% Smart random AI - checks for winning/blocking moves before playing randomly
almost_random_ai(Board, NewBoard, Player) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [],
    (   
        % Try to find a winning move
        member(Col, ValidMoves),
        playMove(Board, Col, TmpBoard, Player),
        game_over(TmpBoard, Result),
        Result == Player
    ->  % Winning move found
        playMove(Board, Col, NewBoard, Player)
    ;   
        % Check if we need to block opponent's win
        changePlayer(Player, Opponent),
        member(Col, ValidMoves),
        playMove(Board, Col, TmpBoard, Opponent),
        game_over(TmpBoard, Result),
        Result == Opponent
    ->  % Block opponent's winning move
        playMove(Board, Col, NewBoard, Player)
    ;   
        % No immediate threats or opportunities, play randomly
        findall(Col, validMove(Col), ValidMoves2),
        length(ValidMoves2, Len),
        random(0, Len, Index),
        nth0(Index, ValidMoves2, ChosenColumn),
        playMove(Board, ChosenColumn, NewBoard, Player)
    ).