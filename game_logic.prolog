% File: game_logic.prolog
% Purpose: This module implements the core game logic for Connect 4, including applying moves with gravity, checking for wins in all directions, determining game over conditions, and utility predicates for board manipulation.
% It handles the rules specific to Connect 4, such as pieces falling to the bottom and winning with 4 consecutive pieces.
% Adapted from morpion.prolog: morpion.prolog implements Tic-Tac-Toe with direct placement on a flat 9-element list and 3-in-a-row win checks. This version adapts to Connect 4's gravity mechanics (pieces drop to the lowest empty row), 4-in-a-row wins, and a 6x7 2D matrix board.

% drop_piece(Board, Column, Mark, NewBoard)
% Applies a move by dropping the given mark into the lowest empty ('e') row of the specified column.
% Board: list of lists, current board state
% Column: integer (1-7), the column to drop into
% Mark: atom ('x' or 'o'), the player's mark
% NewBoard: list of lists, the board after the move
% Fails if the column is full.
% Adapted from morpion.prolog: morpion's move predicate places directly at a square; this implements gravity by finding the lowest empty row.
drop_piece(Board, Col, Mark, NewBoard) :-
    find_lowest_empty_row(Board, Col, Row),
    set_item_2d(Board, Row, Col, Mark, NewBoard).

% find_lowest_empty_row(Board, Col, Row)
% Finds the lowest (highest row number, since rows are 1-6 from top to bottom) empty row in the given column.
% Board: list of lists, current board
% Col: integer (1-7), column to check
% Row: integer (1-6), the row where the piece will land
% Adapted from morpion.prolog: morpion doesn't have gravity; this is new for Connect 4.
find_lowest_empty_row(Board, Col, Row) :-
    findall(R, (between(1, 6, R), get_item_2d(Board, R, Col, 'e')), Rs),
    max_list(Rs, Row).

% column_full(Board, Col)
% Succeeds if the specified column has no empty spaces (i.e., all rows in the column are occupied).
% Board: list of lists, current board
% Col: integer (1-7), column to check
% Adapted from morpion.prolog: morpion checks for full board differently; this checks per column for Connect 4.
column_full(Board, Col) :-
    \+ (between(1, 6, Row), get_item_2d(Board, Row, Col, 'e')).

% Helper predicates for 2D matrix manipulation
% These are adapted from morpion's list processing predicates to work with 2D structures.

% get_item_2d(Matrix, Row, Col, Value)
% Retrieves the value at the specified row and column (1-based indexing).
% Matrix: list of lists, the 2D board
% Row: integer (1-6), row index
% Col: integer (1-7), column index
% Value: atom, the value at that position ('x', 'o', or 'e')
% Adapted from morpion.prolog: morpion's square predicate maps flat list positions; this accesses 2D matrix.
get_item_2d(Matrix, Row, Col, Value) :-
    nth1(Row, Matrix, TheRow),
    nth1(Col, TheRow, Value).

% set_item_2d(Matrix, Row, Col, Value, NewMatrix)
% Sets the value at the specified row and column, returning the new matrix.
% Matrix: list of lists, original board
% Row: integer (1-6), row index
% Col: integer (1-7), column index
% Value: atom, the new value
% NewMatrix: list of lists, updated board
% Adapted from morpion.prolog: morpion's set_item works on flat list; this updates 2D matrix.
set_item_2d(Matrix, Row, Col, Value, NewMatrix) :-
    nth1(Row, Matrix, TheRow),
    set_item(TheRow, Col, Value, NewRow),
    set_item(Matrix, Row, NewRow, NewMatrix).

% set_item(List, Position, Value, NewList)
% Replaces the item at the given position (1-based) in the list with the new value.
% List: list, the original list
% Position: integer, 1-based index
% Value: any, the new value
% NewList: list, the updated list
% Adapted from morpion.prolog: directly from morpion's set_item, used here for rows.
set_item(L, N, V, L2) :-
    set_item2(L, N, V, 1, L2).

set_item2([], _N, _V, _A, []) :- !.
set_item2([_H|T1], N, V, A, [V|T2]) :-
    A = N,
    A1 is A + 1,
    set_item2(T1, -1, V, A1, T2).
set_item2([H|T1], N, V, A, [H|T2]) :-
    A1 is A + 1,
    set_item2(T1, N, V, A1, T2).

% has_four_consecutive(List, Mark)
% Succeeds if the list contains 4 consecutive occurrences of the given mark.
% List: list, a row, column, or diagonal
% Mark: atom ('x' or 'o'), the mark to check for
% Adapted from morpion.prolog: morpion checks for 3 in a row; this checks for 4 for Connect 4.
has_four_consecutive(L, M) :- append(_, [M,M,M,M|_], L).

% get_column(Board, Col, ColumnList)
% Gets the list of values in the specified column, from row 1 to 6.
% Board: list of lists, the board
% Col: integer (1-7), column index
% ColumnList: list, the column values
% Adapted from morpion.prolog: morpion doesn't have columns; this is new for 2D win checks.
get_column(Board, Col, ColumnList) :-
    findall(V, (between(1,6,Row), get_item_2d(Board, Row, Col, V)), ColumnList).

% get_diagonal1(Board, Sum, DiagonalList)
% Gets the list of values in the / diagonal where row + col = Sum.
% Board: list of lists, the board
% Sum: integer, the sum row + col
% DiagonalList: list, the diagonal values
% Adapted from morpion.prolog: morpion has no diagonals; this is new for Connect 4 win checks.
get_diagonal1(Board, S, DiagonalList) :-
    findall(V, (between(1,6,Row), Col is S - Row, between(1,7,Col), get_item_2d(Board, Row, Col, V)), DiagonalList).

% get_diagonal2(Board, Diff, DiagonalList)
% Gets the list of values in the \ diagonal where row - col = Diff.
% Board: list of lists, the board
% Diff: integer, the difference row - col
% DiagonalList: list, the diagonal values
% Adapted from morpion.prolog: morpion has no diagonals; this is new for Connect 4.
get_diagonal2(Board, D, DiagonalList) :-
    findall(V, (between(1,6,Row), Col is Row - D, between(1,7,Col), get_item_2d(Board, Row, Col, V)), DiagonalList).

% horizontal_win(Board, Mark)
% Succeeds if the mark has 4 in a row horizontally in any row.
% Board: list of lists, the board
% Mark: atom ('x' or 'o'), the mark to check
horizontal_win(Board, M) :-
    between(1,6,Row),
    between(1,4,StartCol),
    get_item_2d(Board, Row, StartCol, M),
    C2 is StartCol + 1, get_item_2d(Board, Row, C2, M),
    C3 is StartCol + 2, get_item_2d(Board, Row, C3, M),
    C4 is StartCol + 3, get_item_2d(Board, Row, C4, M).

% vertical_win(Board, Mark)
% Succeeds if the mark has 4 in a row vertically in any column.
% Board: list of lists, the board
% Mark: atom ('x' or 'o'), the mark to check
vertical_win(Board, M) :-
    between(1,7,Col),
    between(1,3,StartRow),
    get_item_2d(Board, StartRow, Col, M),
    R2 is StartRow + 1, get_item_2d(Board, R2, Col, M),
    R3 is StartRow + 2, get_item_2d(Board, R3, Col, M),
    R4 is StartRow + 3, get_item_2d(Board, R4, Col, M).

% diagonal1_win(Board, Mark)
% Succeeds if the mark has 4 in a row diagonally (/) in any diagonal.
% Board: list of lists, the board
% Mark: atom ('x' or 'o'), the mark to check
diagonal1_win(Board, M) :-
    between(1,3,StartRow),
    between(4,7,StartCol),
    get_item_2d(Board, StartRow, StartCol, M),
    R2 is StartRow + 1, C2 is StartCol - 1, get_item_2d(Board, R2, C2, M),
    R3 is StartRow + 2, C3 is StartCol - 2, get_item_2d(Board, R3, C3, M),
    R4 is StartRow + 3, C4 is StartCol - 3, get_item_2d(Board, R4, C4, M).

% diagonal2_win(Board, Mark)
% Succeeds if the mark has 4 in a row diagonally (\) in any diagonal.
% Board: list of lists, the board
% Mark: atom ('x' or 'o'), the mark to check
diagonal2_win(Board, M) :-
    between(1,3,StartRow),
    between(1,4,StartCol),
    get_item_2d(Board, StartRow, StartCol, M),
    R2 is StartRow + 1, C2 is StartCol + 1, get_item_2d(Board, R2, C2, M),
    R3 is StartRow + 2, C3 is StartCol + 2, get_item_2d(Board, R3, C3, M),
    R4 is StartRow + 3, C4 is StartCol + 3, get_item_2d(Board, R4, C4, M).

% win(Board, Mark)
% Succeeds if the mark has 4 consecutive pieces in any direction (horizontal, vertical, or diagonal).
% Board: list of lists, the board
% Mark: atom ('x' or 'o'), the mark to check
win(Board, M) :-
    (horizontal_win(Board, M) ;
     vertical_win(Board, M) ;
     diagonal1_win(Board, M) ;
     diagonal2_win(Board, M)),
    M \= 'e'.

% game_over(Board, Result)
% Determines the game result: the winning mark ('x' or 'o'), 'draw' if board is full, or 'no' if ongoing.
% Board: list of lists, the board
% Result: atom, the result
% Adapted from morpion.prolog: morpion's game_over checks win or full board; similar but adapted for 2D and draw condition.
game_over(Board, Result) :-
    write('Win check: '), (win(Board, M) -> write('yes, M='), write(M) ; write('no')), nl,
    (win(Board, M) -> Result = M ;
     (\+ (member(Row, Board), member('e', Row)) -> Result = 'draw' ;
      Result = 'no')).

% available_moves(Board, Moves)
% Returns the list of playable columns (1-7) that are not full.
% Board: list of lists, the board
% Moves: list of integers, available columns
% Adapted from morpion.prolog: morpion's moves finds empty squares; this finds non-full columns.
available_moves(Board, Moves) :-
    findall(Col, (between(1,7,Col), get_item_2d(Board, 1, Col, 'e')), Moves).