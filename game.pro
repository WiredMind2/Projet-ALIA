:- dynamic board/1.
:- dynamic last_index/1.

:- consult('matrix.pro').
:- consult('win.pro').

% playMove/6 (Stateless regarding last_index)
playMove(Board,Col,NewBoard,Player,LastIndex,NewLastIndex):-
    nth0(Col,LastIndex,Row),
    Row < 6,                         
    replaceMatrix(Board,Row,Col, Player,NewBoard),
    NewRow is Row + 1,
    replace(LastIndex,Col,NewRow,NewLastIndex).

% playMove/4 (Stateful)
playMove(Board, Col, NewBoard, Player) :-
    last_index(LastIndex),
    nth0(Col, LastIndex, Row),
    Row < 6,
    replaceMatrix(Board, Col, Row, Player, NewBoard),
    NewRow is Row + 1,
    replace(LastIndex, Col, NewRow, NewLastIndex),
    applyLastIndex(LastIndex, NewLastIndex).

% simulateMove/4 (Stateless)
simulateMove(Board, Col, NewBoard, Player) :-
    get_next_open_row(Board, Col, Row),
    Row \= -1,
    replaceMatrix(Board, Col, Row, Player, NewBoard).

applyBoard(_OldBoard,NewBoard):-
    retractall(board(_)),
    assert(board(NewBoard)).

applyLastIndex(_OldLastIndex,NewLastIndex):-
    retractall(last_index(_)),
    assert(last_index(NewLastIndex)).

changePlayer('x', 'o').
changePlayer('o', 'x').
changePlayer(1, 2).
changePlayer(2, 1).

% validMove/1 (Stateful)
validMove(Col) :-
    between(0,6,Col),
    last_index(Indices),
    nth0(Col, Indices, Row),
    Row < 6.

% validMove/2 (Stateless)
validMove(Board, Col) :-
    between(0, 6, Col),
    get_next_open_row(Board, Col, Row),
    Row \= -1.

% Helper predicates
get_next_open_row(Board, Col, Row) :-
    nth0(Col, Board, ColumnData),
    find_first_zero(ColumnData, 0, Row).

find_first_zero([], _, -1).
find_first_zero([0|_], Index, Index) :- !.
find_first_zero([_|Rest], Index, Row) :-
    NextIndex is Index + 1,
    find_first_zero(Rest, NextIndex, Row).

setup :- 
    retractall(board(_)),
    retractall(last_index(_)),
    generate_matrix(7,6,Board),
    length_list(7, Indices),
    maplist(=(0), Indices),
    assert(last_index(Indices)),
    assert(board(Board)).

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
