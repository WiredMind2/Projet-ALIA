% Board printing functions for Connect Four game
% This module handles the visual display of the game board

% Print player with their color
print_player(1) :- 
    ansi_format([fg(red)], 'o', []).
print_player(2) :- 
    ansi_format([fg(yellow)], 'o', []).
print_player(x) :- 
    ansi_format([fg(red)], 'o', []).
print_player(o) :- 
    ansi_format([fg(yellow)], 'o', []).

% print_cell(Value)
% Prints a single cell with appropriate character representation
% Value: 0 for empty, 1 for player 1 ('x'), 2 for player 2 ('o')
print_cell(0) :-
    write('.').                                    % Empty cell
print_cell(.) :-
    write('.').
print_cell(1) :- 
    ansi_format([fg(red)], 'o', []).               % Player 1 cell
print_cell(2) :- 
    ansi_format([fg(yellow)], 'o', []).            % Player 2 cell
print_cell(x) :- 
    ansi_format([fg(red)], 'o', []).
print_cell(o) :- 
    ansi_format([fg(yellow)], 'o', []).



% print_row(Cells)
% Prints a single row of cells with space separation
% Cells: list of cell values to print
print_row([]).
print_row([Cell|Rest]) :-
    print_cell(Cell), write(' '),                 % Print cell and space
    print_row(Rest).                              % Recursively print rest

% get_row(Board, RowIndex, RowList)
% Extracts a specific row from the 2D board matrix
% Board: 2D matrix representing the game board
% RowIndex: index of row to extract (0-based)
% RowList: output list containing the row elements
get_row([], _, []).
get_row([Col|RestCols], RowIndex, [Cell|RestCells]) :-
    nth0(RowIndex, Col, Cell),                    % Get element at RowIndex from column
    get_row(RestCols, RowIndex, RestCells).       % Recursively get rest of row

% print_rows(Board, RowIndex)
% Recursively prints all rows from top to bottom
% Board: 2D matrix representing the game board
% RowIndex: current row to print (starts from top, 5, and goes down to 0)
print_rows(_, -1) :- !.                           % Base case: no more rows to print
print_rows(Board, RowIndex) :-
    get_row(Board, RowIndex, Row),                % Extract the current row
    print_row(Row),                               % Print the row
    nl,                                           % New line after each row
    NextRow is RowIndex - 1,                      % Move to next row down
    print_rows(Board, NextRow).                   % Recursively print remaining rows

% print_column_numbers(N)
% Prints column numbers (0-6) at the bottom of the board
% N: current column number to print
print_column_numbers(8) :- nl, !.                 % Extra newline after column numbers
print_column_numbers(N) :-
    write(N), write(' '),                         % Print column number and space
    Next is N + 1,                                % Move to next column
    print_column_numbers(Next).                   % Recursively print remaining numbers

% print_board(Board)
% Main function to print the complete game board
% Board: 2D matrix representing the game board
print_board(Board) :-
    print_rows(Board, 5),                         % Print all rows (5 down to 0)
    print_column_numbers(0).                      % Print column numbers (0-7)