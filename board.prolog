% File: board.prolog
% Purpose: This module handles the board representation, initialization, and display for the Connect 4 game.
% It provides predicates for creating an empty board, initializing the dynamic board fact, and printing the board with row and column labels.
% Adapted from morpion.prolog: morpion.prolog uses a flat 9-element list for a 3x3 Tic-Tac-Toe board. This version uses a 6x7 matrix (list of lists) to accommodate the larger Connect 4 board and gravity mechanics, where pieces fall to the bottom.

:- dynamic board/1.

% empty_matrix(Rows, Cols, Matrix)
% Generates an empty matrix with the specified number of rows and columns, filled with 'e' (empty).
% Rows: integer, number of rows (6 for Connect 4)
% Cols: integer, number of columns (7 for Connect 4)
% Matrix: list of lists, the resulting empty matrix
% Adapted from morpion.prolog: morpion initializes a flat list; this creates a 2D structure.
empty_matrix(Rows, Cols, Matrix) :-
    length(Matrix, Rows),
    maplist(empty_row(Cols), Matrix).

% empty_row(Cols, Row)
% Creates a single row with Cols number of 'e' elements.
% Cols: integer, number of columns
% Row: list, the resulting row filled with 'e'
empty_row(Cols, Row) :-
    length(Row, Cols),
    maplist(=('e'), Row).

% init_board
% Initializes the dynamic board fact with a new empty 6x7 matrix.
% Retracts any existing board and asserts the new one.
% Adapted from morpion.prolog: morpion's initialize predicate sets a flat list; this sets a 2D matrix.
init_board :-
    retractall(board(_)),
    empty_matrix(6, 7, Board),
    assert(board(Board)).

% print_board
% Prints the current board to the console with column numbers (1-7) and row numbers (1-6).
% Retrieves the board matrix and prints rows with labels.
print_board :-
    board(Matrix),
    write('  1 2 3 4 5 6 7'), nl,
    print_rows(Matrix, 1).

% print_rows(Matrix, Num)
% Helper predicate to print each row with its row number.
% Matrix: list of lists, the board
% Num: integer, current row number starting from 1
print_rows([], _).
print_rows([Row|Rest], Num) :-
    write(Num), write(' '),
    print_row(Row),
    nl,
    Num1 is Num + 1,
    print_rows(Rest, Num1).

% print_row(Row)
% Prints a single row, writing each element separated by spaces.
% Row: list, a row of the board
print_row([]).
print_row([H|T]) :-
    write(H), write(' '),
    print_row(T).