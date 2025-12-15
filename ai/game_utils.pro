% Shared game utilities for AI implementations
% This module contains common game functions used by all AI types

:- consult('../matrix.pro').
:- consult('../win.pro').

% playMove(Board, Col, NewBoard, Player)
% Applies a move to the board (Stateful - for main game loop)
% This function modifies the global game state and should only be used in the main game loop
% Board: current game board
% Col: column index where to place the piece (0-6)
% NewBoard: resulting board after the move
% Player: player number (1 or 2) making the move
playMove(Board, Col, NewBoard, Player) :-
    last_index(LastIndex),                        % Get current state of column heights
    nth0(Col, LastIndex, Row),                    % Get next available row in this column
    Row < 6,                                      % Check if column is not full
    replaceMatrix(Board, Col, Row, Player, NewBoard), % Apply the move
    NewRow is Row + 1,                            % Update height for this column
    replace(LastIndex, Col, NewRow, NewLastIndex), % Update the last index state
    applyLastIndex(LastIndex, NewLastIndex).      % Save updated state

% simulateMove(Board, Col, NewBoard, Player)
% Simulates a move without modifying the game state (Stateless - for AI)
% Used by AI algorithms to test moves without affecting the actual game
% Board: current game board
% Col: column index where to place the piece (0-6)
% NewBoard: resulting board after the simulated move
% Player: player number (1 or 2) making the move
simulateMove(Board, Col, NewBoard, Player) :-
    get_next_open_row(Board, Col, Row),           % Find next available row in column
    Row \= -1,                                    % Check if column is playable
    replaceMatrix(Board, Col, Row, Player, NewBoard). % Apply the simulated move

% validMove(Board, Col)
% Checks if a column is a valid move (Stateless - for AI)
% Board: current game board
% Col: column index to check (0-6)
% Returns true if the column has space for another piece
validMove(Board, Col) :-
    between(0, 6, Col),                          % Check column index is valid
    get_next_open_row(Board, Col, Row),          % Find next available row
    Row \= -1.                                   % Column has space if row != -1

% validMove(Col)
% Legacy validMove for main game loop (uses global state)
% This version uses global state instead of taking board as parameter
% Col: column index to check (0-6)
% Returns true if the column has space for another piece
validMove(Col) :-
    between(0, 6, Col),                          % Check column index is valid
    last_index(Indices),                         % Get current column heights from state
    nth0(Col, Indices, Row),                     % Get height of this column
    Row < 6.                                     % Column not full if height < 6

% get_next_open_row(Board, Col, Row)
% Helper to find the next open row in a column
% Board: current game board
% Col: column index to examine
% Row: returns the row index of the first empty cell, or -1 if column is full
get_next_open_row(Board, Col, Row) :-
    nth0(Col, Board, ColumnData),                % Get column data from board
    find_first_zero(ColumnData, 0, Row).         % Find first empty cell (value 0)

% find_first_zero(List, Index, Result)
% Recursively finds the index of the first zero in a list
% List: list to search
% Index: current position being checked
% Result: index of first zero, or -1 if no zeros found
find_first_zero([], _, -1).                      % Base case: empty list
find_first_zero([0|_], Index, Index) :- !.       % Found zero at current index
find_first_zero([_|Rest], Index, Row) :-         % Current element not zero
    NextIndex is Index + 1,                      % Move to next position
    find_first_zero(Rest, NextIndex, Row).       % Recursively search rest

% changePlayer(Current, Next)
% Switches between players
% Current: current player number (1 or 2)
% Next: returns the other player number
changePlayer(1, 2).                              % Player 1 -> Player 2
changePlayer(2, 1).                              % Player 2 -> Player 1

% applyBoard(OldBoard, NewBoard)
% Updates the current board state in the global database
% OldBoard: current board (ignored, just for pattern matching)
% NewBoard: new board state to save
applyBoard(_OldBoard, NewBoard) :-
    retractall(board(_)),                        % Remove old board state
    assert(board(NewBoard)).                     % Save new board state

% applyLastIndex(OldLastIndex, NewLastIndex)
% Updates the last index state after a move
% OldLastIndex: current last index state (ignored)
% NewLastIndex: new last index state to save
applyLastIndex(_OldLastIndex, NewLastIndex) :-
    retractall(last_index(_)),                   % Remove old last index state
    assert(last_index(NewLastIndex)).            % Save new last index state

% setup
% Sets up the initial game state
% Creates empty board and initializes column tracking
setup :- 
    retractall(board(_)),                        % Clear any existing board state
    retractall(last_index(_)),                   % Clear any existing index state
    generate_matrix(7, 6, Board),                % Create 7x6 empty board
    length_list(7, Indices),                     % Create list for column heights
    maplist(=(0), Indices),                      % Initialize all heights to 0
    assert(last_index(Indices)),                 % Save initial column heights
    assert(board(Board)).                        % Save initial empty board

% game_over(Board, Result)
% Checks if the game is over and returns the result
% Board: current game board
% Result: returns 'draw' if board is full, player number (1/2) if someone won, 'no' if game continues
game_over(Board, Result) :-
    (win(Board, M) -> Result = M ;               % Someone won
     (board_full(Board) -> Result = 'draw' ;     % Board full, it's a draw
      Result = 'no')).                           % Game continues

% board_full(Board)
% Checks if the board is completely full (no empty cells)
% Board: current game board
% Returns true if no cell contains 0 (empty)
board_full(Board) :-
    \+ (member(Col, Board), member(0, Col)).     % No column contains any empty cells