% Win detection functions for Connect Four game
% This module detects when a player has won by checking horizontal, vertical, and diagonal lines

% win(Board, Winner)
% Main win detection predicate that checks all possible win conditions
% Board: current game board (2D matrix)
% Winner: returns 1 or 2 if a player has won, 0 if no winner yet
win(Board, Winner) :-
    horizontal_win(Board, Winner) ;               % Check horizontal lines (left to right)
    vertical_win(Board, Winner) ;                 % Check vertical lines (top to bottom)
    diagonal_win(Board, Winner).                  % Check diagonal lines (both directions)

% horizontal_win(Board, Winner)
% Checks for horizontal wins by examining each row
% Board: current game board
% Winner: returns player number if horizontal win found
horizontal_win(Board, Winner) :-
    (Winner = 1 ; Winner = 2),                   % Winner must be player 1 or 2
    between(0, 5, Row),                          % Check each of the 6 rows (0-5)
    % Extract all cells in this row from all columns
    findall(W, (between(0, 6, Col), 
                nth0(Col, Board, C), 
                nth0(Row, C, W)), RowList),
    sublist([Winner, Winner, Winner, Winner], RowList). % Look for 4 consecutive pieces

% vertical_win(Board, Winner)
% Checks for vertical wins by examining each column
% Board: current game board
% Winner: returns player number if vertical win found
vertical_win(Board, Winner) :-
    (Winner = 1 ; Winner = 2),                   % Winner must be player 1 or 2
    member(Col, Board),                          % Get each column
    sublist([Winner, Winner, Winner, Winner], Col). % Look for 4 consecutive pieces in column

% diagonal_win(Board, Winner)
% Checks for diagonal wins in both diagonal directions
% Board: current game board
% Winner: returns player number if diagonal win found
diagonal_win(Board, Winner) :-
    (Winner = 1 ; Winner = 2),                   % Winner must be player 1 or 2
    (diagonal1_win(Board, Winner) ;              % Check diagonal from top-left to bottom-right
     diagonal2_win(Board, Winner)).              % Check diagonal from top-right to bottom-left

% diagonal1_win(Board, Winner)
% Checks for diagonal wins from top-left to bottom-right (\ direction)
% Board: current game board
% Winner: player number to check for
diagonal1_win(Board, Winner) :-
    between(0, 3, StartRow),                     % Starting rows 0-3 (can't start lower or won't have 4 rows)
    between(0, 3, StartCol),                     % Starting columns 0-3 (can't start right or won't have 4 columns)
    check_diagonal(StartRow, StartCol, 1, 1, Board, Winner). % Check diagonal with step (1,1)

% diagonal2_win(Board, Winner)
% Checks for diagonal wins from top-right to bottom-left (/ direction)
% Board: current game board
% Winner: player number to check for
diagonal2_win(Board, Winner) :-
    between(0, 3, StartRow),                     % Starting rows 0-3 (can't start lower or won't have 4 rows)
    between(3, 6, StartCol),                     % Starting columns 3-6 (can't start left or won't have 4 columns)
    check_diagonal(StartRow, StartCol, 1, -1, Board, Winner). % Check diagonal with step (1,-1)

% check_diagonal(Row, Col, DRow, DCol, Board, Winner)
% Generic diagonal checking function that verifies 4 consecutive pieces
% Row, Col: starting position to check from
% DRow, DCol: direction steps to move (e.g., 1,1 for down-right, 1,-1 for down-left)
% Board: current game board
% Winner: player number to check for
check_diagonal(Row, Col, DRow, DCol, Board, Winner) :-
    nth0(Col, Board, C), nth0(Row, C, Winner),           % Check starting position
    Row1 is Row + DRow, Col1 is Col + DCol,              % Move to next position
    nth0(Col1, Board, C1), nth0(Row1, C1, Winner),       % Check second position
    Row2 is Row1 + DRow, Col2 is Col1 + DCol,            % Move to third position
    nth0(Col2, Board, C2), nth0(Row2, C2, Winner),       % Check third position
    Row3 is Row2 + DRow, Col3 is Col2 + DCol,            % Move to fourth position
    nth0(Col3, Board, C3), nth0(Row3, C3, Winner).       % Check fourth position