% Smart random AI implementation
% This AI plays randomly unless it can win immediately or needs to block an opponent's immediate win

:- ensure_loaded('ai/game_utils.pro').

% almost_random_ai(Board, NewBoard, Player)
% Smart random AI - checks for winning/blocking moves before playing randomly
% This AI follows a priority system: 1) Win if possible, 2) Block opponent's win, 3) Play randomly
% Board: current game board
% NewBoard: resulting board after the AI move
% Player: player number (1 or 2) making the move
almost_random_ai(Board, NewBoard, Player) :-
    findall(Col, validMove(Col), ValidMoves),     % Find all valid columns
    ValidMoves \= [],                             % Ensure there are valid moves available
    (   
        % PRIORITY 1: Try to find a winning move
        % Check if any valid move results in immediate victory
        member(Col, ValidMoves),                  % Try each valid column
        playMove(Board, Col, TmpBoard, Player),   % Simulate the move
        game_over(TmpBoard, Result),              % Check if this move wins
        Result == Player                          % Victory condition met
    ->  
        % Winning move found - take it immediately
        playMove(Board, Col, NewBoard, Player)
    ;   
        % PRIORITY 2: Check if we need to block opponent's win
        % Look for moves that would prevent opponent from winning next turn
        changePlayer(Player, Opponent),           % Get opponent's player number
        member(Col, ValidMoves),                  % Try each valid column
        playMove(Board, Col, TmpBoard, Opponent), % Simulate opponent making this move
        game_over(TmpBoard, Result),              % Check if this gives opponent victory
        Result == Opponent                        % Opponent would win with this move
    ->  
        % Block opponent's winning move - play here instead
        playMove(Board, Col, NewBoard, Player)
    ;   
        % PRIORITY 3: No immediate threats or opportunities, play randomly
        findall(Col, validMove(Col), ValidMoves2), % Refresh valid moves list
        length(ValidMoves2, Len),                 % Count available moves
        random(0, Len, Index),                   % Generate random index
        nth0(Index, ValidMoves2, ChosenColumn),  % Select random column
        playMove(Board, ChosenColumn, NewBoard, Player) % Apply the random move
    ).