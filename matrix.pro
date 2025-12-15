% Matrix utility functions for Connect Four game
% This module provides basic matrix operations needed for game board manipulation

% length_list(N, List)
% Succeeds if List has length N (wrapper for built-in length/2)
length_list(N, List) :- length(List, N).

% generate_matrix(Cols, Rows, Matrix)
% Creates a matrix (list of lists) with specified dimensions, filled with zeros
% Cols: number of columns (width)
% Rows: number of rows (height)  
% Matrix: output matrix where each cell contains 0 (empty)
generate_matrix(Cols, Rows, Matrix) :-
    length_list(Cols, Matrix),                    % Create list with Cols elements
    maplist(length_list(Rows), Matrix),           % Each element is a list with Rows elements
    maplist(maplist(=(0)), Matrix).              % Fill all elements with 0 (empty)

% replace(List, Index, NewValue, NewList)
% Replaces element at Index in List with NewValue
% Special case: replacing the first element
replace([_|T], 0, X, [X|T]).
% General case: recursively find and replace element
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

% replaceMatrix(Matrix, ColIndex, RowIndex, Player, NewMatrix)
% Replaces a cell in a 2D matrix at specified column and row indices
% Matrix: original 2D matrix
% ColIndex: column index (0-based)
% RowIndex: row index (0-based)
% Player: value to place (1 or 2 for players, 0 for empty)
% NewMatrix: resulting matrix with the replacement
replaceMatrix(Matrix, ColIndex, RowIndex, Player, NewMatrix) :-
    nth0(ColIndex, Matrix, OldCol),               % Get the column at ColIndex
    replace(OldCol, RowIndex, Player, NewCol),    % Replace element in that column
    replace(Matrix, ColIndex, NewCol, NewMatrix). % Replace the column in the matrix

% sublist(Sub, List)
% Checks if Sub is a contiguous sublist of List
% Sub: candidate sublist
% List: list to search in
sublist(Sub, List) :- append(_, Suffix, List), append(Sub, _, Suffix).