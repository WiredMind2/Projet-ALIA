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
        calculate_position_score(Board, EvalBoard, Player, PlayerPosScore),
        calculate_position_score(Board, EvalBoard, Opponent, OpponentPosScore),
        
        % Here we can add threat detection for better evaluation at smaller depths
        % But technically this can be computationally expensive
        % And is unecessary at higher depths where minimax already sees winning moves

        % count_threats(Board, Player, PlayerThreats),
        % count_threats(Board, Opponent, OpponentThreats),
        
        % ThreatScore is (PlayerThreats * 5000) - (OpponentThreats * 10000),
        PosScore is PlayerPosScore - OpponentPosScore,
        
        % Score is ThreatScore + PosScore
        Score is PosScore
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

% Count threats: 3 pieces in a row with one empty space
count_threats(Board, Player, ThreatCount) :-
    findall(1, is_threat(Board, Player), Threats),
    length(Threats, ThreatCount).

% Check if there's a threat in any direction
is_threat(Board, Player) :-
    (horizontal_threat(Board, Player) ;
     vertical_threat(Board, Player) ;
     diagonal1_threat(Board, Player) ;
     diagonal2_threat(Board, Player)).

% Horizontal threat: XXX. or .XXX or XX.X or X.XX
horizontal_threat(Board, Player) :-
    between(0, 5, Row),
    between(0, 3, StartCol),
    C1 is StartCol, C2 is StartCol + 1, C3 is StartCol + 2, C4 is StartCol + 3,
    get_item_2d(Board, Row, C1, V1),
    get_item_2d(Board, Row, C2, V2),
    get_item_2d(Board, Row, C3, V3),
    get_item_2d(Board, Row, C4, V4),
    count_player_and_empty([V1, V2, V3, V4], Player, 3, 1).

% Vertical threat: 3 pieces with one empty above
vertical_threat(Board, Player) :-
    between(0, 6, Col),
    between(0, 2, StartRow),
    R1 is StartRow, R2 is StartRow + 1, R3 is StartRow + 2, R4 is StartRow + 3,
    get_item_2d(Board, R1, Col, V1),
    get_item_2d(Board, R2, Col, V2),
    get_item_2d(Board, R3, Col, V3),
    get_item_2d(Board, R4, Col, V4),
    count_player_and_empty([V1, V2, V3, V4], Player, 3, 1).

% Diagonal1 threat (/ direction)
diagonal1_threat(Board, Player) :-
    between(0, 2, StartRow),
    between(3, 6, StartCol),
    R1 is StartRow, C1 is StartCol,
    R2 is StartRow + 1, C2 is StartCol - 1,
    R3 is StartRow + 2, C3 is StartCol - 2,
    R4 is StartRow + 3, C4 is StartCol - 3,
    get_item_2d(Board, R1, C1, V1),
    get_item_2d(Board, R2, C2, V2),
    get_item_2d(Board, R3, C3, V3),
    get_item_2d(Board, R4, C4, V4),
    count_player_and_empty([V1, V2, V3, V4], Player, 3, 1).

% Diagonal2 threat (\ direction)
diagonal2_threat(Board, Player) :-
    between(0, 2, StartRow),
    between(0, 3, StartCol),
    R1 is StartRow, C1 is StartCol,
    R2 is StartRow + 1, C2 is StartCol + 1,
    R3 is StartRow + 2, C3 is StartCol + 2,
    R4 is StartRow + 3, C4 is StartCol + 3,
    get_item_2d(Board, R1, C1, V1),
    get_item_2d(Board, R2, C2, V2),
    get_item_2d(Board, R3, C3, V3),
    get_item_2d(Board, R4, C4, V4),
    count_player_and_empty([V1, V2, V3, V4], Player, 3, 1).

% Count how many cells match Player and how many are empty
count_player_and_empty(List, Player, ExpectedPlayer, ExpectedEmpty) :-
    findall(1, (member(V, List), V = Player), PlayerCells),
    findall(1, (member(V, List), V = '.'), EmptyCells),
    length(PlayerCells, ExpectedPlayer),
    length(EmptyCells, ExpectedEmpty).

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
