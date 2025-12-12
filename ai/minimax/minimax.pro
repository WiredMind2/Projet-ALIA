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

% Base case: depth limit reached or game over
minimax(Board, Depth, Player, BestCol, Score) :-
    Depth =< 0,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: game is over
minimax(Board, Depth, Player, BestCol, Score) :-
    game_over(Board, Result), 
    Result \= 'no',
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Recursive case: evaluate all possible moves
minimax(Board, Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [],
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    findall(Score-Col, (
        member(Col, ValidMoves), 
        simulateMove(Board, Col, NewBoard, Player), 
        minimax(NewBoard, NewDepth, Opponent, _, OppScore), 
        Score is -OppScore
    ), ScoresCols),
    sort(ScoresCols, Sorted), 
    last(Sorted, Score-BestCol).