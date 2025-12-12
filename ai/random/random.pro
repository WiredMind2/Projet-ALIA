% Pure random AI implementation
% This module implements a completely random AI that selects valid moves uniformly

:- consult('../game_utils.pro').

% Random AI - selects a random valid move
random_ai(Board, NewBoard, Player) :-
    findall(Column, validMove(Column), ValidMoves),
    ValidMoves \= [],
    length(ValidMoves, Len),
    random(0, Len, Index),
    nth0(Index, ValidMoves, ChosenColumn),
    playMove(Board, ChosenColumn, NewBoard, Player).