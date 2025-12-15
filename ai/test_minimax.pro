% Test file for minimax with alpha-beta pruning
% This file demonstrates how to use the minimax algorithm

:- consult('minimax.pro').
:- consult('../matrix.pro').
:- consult('../win.pro').

% Test the minimax algorithm with a simple board state
test_minimax :-
    % Setup the game state
    setup,

    % Initialize alpha-beta parameters
    % Player 1: depth 3, heuristic 1
    % Player 2: depth 3, heuristic 2
    init_alpha_beta(3, 1, 3, 2),

    % Get the initial board
    board(Board),

    % Test with Player 1's turn
    write('Testing minimax with Player 1...'), nl,
    coup_alpha_beta(Board, 1, Column),
    write('Player 1 should play in column: '), write(Column), nl,

    % Test with Player 2's turn
    write('Testing minimax with Player 2...'), nl,
    coup_alpha_beta(Board, 2, Column2),
    write('Player 2 should play in column: '), write(Column2), nl.

% Test with a partially filled board
test_minimax_partial :-
    % Setup the game state
    setup,

    % Get the initial board
    board(Board1),

    % Simulate a few moves: Player 1 in column 3, Player 2 in column 3
    simulateMove(Board1, 3, Board2, 1),
    simulateMove(Board2, 3, Board3, 2),

    % Update the board state
    applyBoard(Board1, Board3),

    % Initialize parameters
    init_alpha_beta(2, 1, 2, 2),

    % Test the algorithm
    write('Testing minimax with partial board...'), nl,
    write('Board state (some moves already made):'), nl,
    print_board(Board3),

    coup_alpha_beta(Board3, 1, Column),
    write('Player 1 should play in column: '), write(Column), nl.

% Helper to print board for debugging
print_board([]).
print_board([Column|Rest]) :-
    write(Column), nl,
    print_board(Rest).

% Run all tests
test_all :-
    test_minimax,
    nl, write('---'), nl,
    test_minimax_partial.