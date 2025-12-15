% Minimax AI with Alpha-Beta Pruning
:- use_module(library(random)).
:- consult('game.pro').

% Evaluation board - higher values for center positions
evaluation_board([
    [3, 4, 5, 7, 5, 4, 3],
    [4, 6, 8, 10, 8, 6, 4],
    [5, 7, 11, 13, 11, 7, 5],
    [5, 7, 11, 13, 11, 7, 5],
    [4, 6, 8, 10, 8, 6, 4],
    [3, 4, 5, 7, 5, 4, 3]
]).

% Evaluate board position for a player
evaluate_board(Board, Player, Score) :-
    changePlayer(Player, Opponent),
    game_over(Board, Result),
    ( Result = Player ->
        % Player wins
        Score = 100000
    ; Result = Opponent ->
        % Opponent wins
        Score = -100000
    ; Result = 'draw' ->
        % Draw
        Score = 0
    ;
        % Non-terminal: use position-based heuristic
        evaluation_board(EvalBoard),
        calculate_position_score(Board, EvalBoard, Player, PlayerScore),
        calculate_position_score(Board, EvalBoard, Opponent, OpponentScore),
        Score is PlayerScore - OpponentScore
    ).

% Calculate position score for a player
calculate_position_score(Board, EvalBoard, Player, Score) :-
    findall(Value,
        (nth0(Row, Board, BoardRow),
         nth0(Col, BoardRow, Cell),
         Cell = Player,
         nth0(Row, EvalBoard, EvalRow),
         nth0(Col, EvalRow, Value)),
        Values),
    sum_list(Values, Score).

% Check if board is terminal (game over or draw)
is_terminal(Board) :-
    game_over(Board, Result),
    Result \= 'no'.

% Main minimax entry point for AI
% iaMinimax(+Board, -NewBoard, +Player, +Depth, +UseAlphaBeta)
iaMinimax(Board, NewBoard, Player, Depth, UseAlphaBeta) :-
    last_index(LastIndex),
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [],
    ( UseAlphaBeta = true ->
        minimax_ab(Board, LastIndex, Depth, -100000, 100000, Player, Player, true, ValidMoves, BestCol, _Score)
    ;
        minimax_no_ab(Board, LastIndex, Depth, Player, Player, true, ValidMoves, BestCol, _Score)
    ),
    playMove(Board, BestCol, NewBoard, Player, LastIndex, NewLastIndex),
    applyLastIndex(LastIndex, NewLastIndex).

% Minimax WITH Alpha-Beta Pruning
% AIPlayer is the original AI (stays constant), CurrentPlayer is who's moving now
minimax_ab(Board, LastIndex, Depth, Alpha, Beta, AIPlayer, CurrentPlayer, IsMaximizing, ValidMoves, BestCol, BestScore) :-
    ( Depth = 0 ; is_terminal(Board) ),
    !,
    evaluate_board(Board, AIPlayer, BestScore),
    BestCol = none.

minimax_ab(Board, LastIndex, Depth, Alpha, Beta, AIPlayer, CurrentPlayer, IsMaximizing, ValidMoves, BestCol, BestScore) :-
    Depth > 0,
    \+ is_terminal(Board),
    NewDepth is Depth - 1,
    changePlayer(CurrentPlayer, Opponent),
    ( IsMaximizing = true ->
        % Maximizing player
        random_member(DefaultCol, ValidMoves),
        maximize_moves(ValidMoves, Board, LastIndex, NewDepth, Alpha, Beta, AIPlayer, CurrentPlayer, Opponent, DefaultCol, -100000, BestCol, BestScore)
    ;
        % Minimizing player
        random_member(DefaultCol, ValidMoves),
        minimize_moves(ValidMoves, Board, LastIndex, NewDepth, Alpha, Beta, AIPlayer, CurrentPlayer, Opponent, DefaultCol, 100000, BestCol, BestScore)
    ).
% Maximizing player: iterate through moves
maximize_moves([], _Board, _LastIndex, _Depth, _Alpha, _Beta, _AIPlayer, _CurrentPlayer, _Opponent, BestCol, BestScore, BestCol, BestScore).
maximize_moves([Col|Rest], Board, LastIndex, Depth, Alpha, Beta, AIPlayer, CurrentPlayer, Opponent, CurrentBestCol, CurrentBestScore, FinalCol, FinalScore) :-
    playMove(Board, Col, NewBoard, CurrentPlayer, LastIndex, NewLastIndex),
    findall(C, (nth0(C, NewLastIndex, Row), Row < 6), NewValidMoves),
    minimax_ab(NewBoard, NewLastIndex, Depth, Alpha, Beta, AIPlayer, Opponent, false, NewValidMoves, _NextCol, Score),
    ( Score > CurrentBestScore ->
        NewBestCol = Col,
        NewBestScore = Score
    ;
        NewBestCol = CurrentBestCol,
        NewBestScore = CurrentBestScore
    ),
    NewAlpha is max(Alpha, NewBestScore),
    % Alpha-Beta pruning
    ( NewAlpha >= Beta ->
        FinalCol = NewBestCol,
        FinalScore = NewBestScore
    ;
        maximize_moves(Rest, Board, LastIndex, Depth, NewAlpha, Beta, AIPlayer, CurrentPlayer, Opponent, NewBestCol, NewBestScore, FinalCol, FinalScore)
    ).

% Minimizing player: iterate through moves
minimize_moves([], _Board, _LastIndex, _Depth, _Alpha, _Beta, _AIPlayer, _CurrentPlayer, _Opponent, BestCol, BestScore, BestCol, BestScore).
minimize_moves([Col|Rest], Board, LastIndex, Depth, Alpha, Beta, AIPlayer, CurrentPlayer, Opponent, CurrentBestCol, CurrentBestScore, FinalCol, FinalScore) :-
    playMove(Board, Col, NewBoard, CurrentPlayer, LastIndex, NewLastIndex),
    findall(C, (nth0(C, NewLastIndex, Row), Row < 6), NewValidMoves),
    minimax_ab(NewBoard, NewLastIndex, Depth, Alpha, Beta, AIPlayer, Opponent, true, NewValidMoves, _NextCol, Score),
    ( Score < CurrentBestScore ->
        NewBestCol = Col,
        NewBestScore = Score
    ;
        NewBestCol = CurrentBestCol,
        NewBestScore = CurrentBestScore
    ),
    NewBeta is min(Beta, NewBestScore),
    % Alpha-Beta pruning
    ( Alpha >= NewBeta ->
        FinalCol = NewBestCol,
        FinalScore = NewBestScore
    ;
        minimize_moves(Rest, Board, LastIndex, Depth, Alpha, NewBeta, AIPlayer, CurrentPlayer, Opponent, NewBestCol, NewBestScore, FinalCol, FinalScore)
    ).

% Minimax WITHOUT Alpha-Beta Pruning
minimax_no_ab(Board, LastIndex, Depth, AIPlayer, CurrentPlayer, IsMaximizing, ValidMoves, BestCol, BestScore) :-
    ( Depth = 0 ; is_terminal(Board) ),
    !,
    evaluate_board(Board, AIPlayer, BestScore),
    BestCol = none.

minimax_no_ab(Board, LastIndex, Depth, AIPlayer, CurrentPlayer, IsMaximizing, ValidMoves, BestCol, BestScore) :-
    Depth > 0,
    \+ is_terminal(Board),
    NewDepth is Depth - 1,
    changePlayer(CurrentPlayer, Opponent),
    ( IsMaximizing = true ->
        % Maximizing player
        random_member(DefaultCol, ValidMoves),
        maximize_moves_no_ab(ValidMoves, Board, LastIndex, NewDepth, AIPlayer, CurrentPlayer, Opponent, DefaultCol, -100000, BestCol, BestScore)
    ;
        % Minimizing player
        random_member(DefaultCol, ValidMoves),
        minimize_moves_no_ab(ValidMoves, Board, LastIndex, NewDepth, AIPlayer, CurrentPlayer, Opponent, DefaultCol, 100000, BestCol, BestScore)
    ).

% Maximizing without pruning
maximize_moves_no_ab([], _Board, _LastIndex, _Depth, _AIPlayer, _CurrentPlayer, _Opponent, BestCol, BestScore, BestCol, BestScore).
maximize_moves_no_ab([Col|Rest], Board, LastIndex, Depth, AIPlayer, CurrentPlayer, Opponent, CurrentBestCol, CurrentBestScore, FinalCol, FinalScore) :-
    playMove(Board, Col, NewBoard, CurrentPlayer, LastIndex, NewLastIndex),
    findall(C, (nth0(C, NewLastIndex, Row), Row < 6), NewValidMoves),
    minimax_no_ab(NewBoard, NewLastIndex, Depth, AIPlayer, Opponent, false, NewValidMoves, _NextCol, Score),
    ( Score > CurrentBestScore ->
        NewBestCol = Col,
        NewBestScore = Score
    ;
        NewBestCol = CurrentBestCol,
        NewBestScore = CurrentBestScore
    ),
    maximize_moves_no_ab(Rest, Board, LastIndex, Depth, AIPlayer, CurrentPlayer, Opponent, NewBestCol, NewBestScore, FinalCol, FinalScore).

% Minimizing without pruning
minimize_moves_no_ab([], _Board, _LastIndex, _Depth, _AIPlayer, _CurrentPlayer, _Opponent, BestCol, BestScore, BestCol, BestScore).
minimize_moves_no_ab([Col|Rest], Board, LastIndex, Depth, AIPlayer, CurrentPlayer, Opponent, CurrentBestCol, CurrentBestScore, FinalCol, FinalScore) :-
    playMove(Board, Col, NewBoard, CurrentPlayer, LastIndex, NewLastIndex),
    findall(C, (nth0(C, NewLastIndex, Row), Row < 6), NewValidMoves),
    minimax_no_ab(NewBoard, NewLastIndex, Depth, AIPlayer, Opponent, true, NewValidMoves, _NextCol, Score),
    ( Score < CurrentBestScore ->
        NewBestCol = Col,
        NewBestScore = Score
    ;
        NewBestCol = CurrentBestCol,
        NewBestScore = CurrentBestScore
    ),
    minimize_moves_no_ab(Rest, Board, LastIndex, Depth, AIPlayer, CurrentPlayer, Opponent, NewBestCol, NewBestScore, FinalCol, FinalScore).
