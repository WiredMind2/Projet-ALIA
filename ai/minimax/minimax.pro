% Complete Minimax AI implementation
% This module contains the full minimax algorithm with alpha-beta pruning

:- consult('../game_utils.pro').
:- consult('../evaluation.pro').

% Main entry point for Minimax AI with timeout handling
minimax_ai(Board, NewBoard, Player) :-
    catch(
        call_with_time_limit(1, (
            minimax(Board, 1, Player, BestCol, _),
            playMove(Board, BestCol, NewBoard, Player)
        )),
        time_limit_exceeded,
        % Fallback to random move if minimax times out
        random_ai:random_ai(Board, NewBoard, Player)
    ).

% Base case: depth limit reached
minimax(Board, Depth, Player, BestCol, Score) :-
    Depth =< 0,
    !,  % Cut to prevent backtracking
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: game is over
minimax(Board, _Depth, Player, BestCol, Score) :-
    game_over(Board, Result), 
    Result \= 'no',
    !,  % Cut to prevent backtracking
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: no valid moves (draw situation)
minimax(Board, _Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves = [],
    !,  % Cut to prevent backtracking
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Recursive case: evaluate all possible moves
minimax(Board, Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [],
    !,  % Cut to prevent backtracking once we have valid moves
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    findall(Score-Col, (
        member(Col, ValidMoves), 
        simulateMove(Board, Col, NewBoard, Player), 
        minimax(NewBoard, NewDepth, Opponent, _, OppScore), 
        Score is -OppScore
    ), ScoresCols),
    ScoresCols \= [],  % Make sure we found some moves
    sort(ScoresCols, Sorted), 
    last(Sorted, Score-BestCol).

% Fallback case: if no scores found, return neutral evaluation
minimax(Board, _Depth, Player, BestCol, Score) :-
    evaluate(Board, Player, Score),
    BestCol = -1.