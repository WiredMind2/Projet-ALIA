% Pure random AI implementation
% This module implements a completely random AI that selects valid moves uniformly at random

:- ensure_loaded('ai/game_utils.pro').

% random_ai(Board, NewBoard, Player)
% Random AI - selects a completely random valid move
% This AI has no strategy and simply chooses any valid move with equal probability
% Board: current game board
% NewBoard: resulting board after the AI move
% Player: player number (1 or 2) making the move
random_ai(Board, NewBoard, Player) :-
    findall(Column, validMove(Column), ValidMoves), % Find all valid columns
    ValidMoves \= [],                               % Ensure there are valid moves available
    length(ValidMoves, Len),                        % Count number of valid moves
    random(0, Len, Index),                         % Generate random index
    nth0(Index, ValidMoves, ChosenColumn),         % Select random column from list
    playMove(Board, ChosenColumn, NewBoard, Player). % Apply the move