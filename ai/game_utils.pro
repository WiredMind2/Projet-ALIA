% Shared game utilities for AI implementations
% This module contains common game functions used by all AI types

:- consult('../matrix.pro').
:- consult('../win.pro').

% Apply a move to the board
playMove(Board, Col, NewBoard, Player) :-
    last_index(LastIndex),
    nth0(Col, LastIndex, Row),
    Row < 6,                         
    replaceMatrix(Board, Row, Col, Player, NewBoard),
    NewRow is Row + 1,
    replace(LastIndex, Col, NewRow, NewLastIndex),
    applyLastIndex(LastIndex, NewLastIndex).

% Simulate a move without modifying the game state (used by minimax)
simulateMove(Board, Col, NewBoard, Player) :-
    last_index(LastIndex),
    nth0(Col, LastIndex, Row),
    Row < 6,
    replaceMatrix(Board, Row, Col, Player, NewBoard).

% Check if a column is a valid move
validMove(Col) :-
    between(0, 6, Col),
    last_index(Indices),
    nth0(Col, Indices, Row),
    Row < 6.

% Switch between players
changePlayer('x', 'o').
changePlayer('o', 'x').

% Update the current board state
applyBoard(_OldBoard, NewBoard) :-
    retractall(board(_)),
    assert(board(NewBoard)).

% Update the last index state after a move
applyLastIndex(_OldLastIndex, NewLastIndex) :-
    retractall(last_index(_)),
    assert(last_index(NewLastIndex)).

% Setup initial game state
setup :- 
    retractall(board(_)),
    retractall(last_index(_)),
    generate_matrix(7, 6, Board),
    length_list(7, Indices),
    maplist(=(0), Indices),
    assert(last_index(Indices)),
    assert(board(Board)).

% Check if the game is over and return the result
game_over(Board, Result) :-
    (win(Board, M) -> Result = M ;
     (board_full(Board) -> Result = 'draw' ;
      Result = 'no')).

% Check if board is full (no empty cells)
board_full(Board) :-
    \+ (member(Row, Board), member('.', Row)).