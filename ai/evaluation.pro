% Heuristic evaluation functions for AI implementations
% This module contains all the scoring and evaluation logic

:- consult('../matrix.pro').
:- consult('../win.pro').

% Main evaluation function for minimax
evaluate(Board, Player, Score) :-
    game_over(Board, Result),
    changePlayer(Player, Opponent),
    ( Result == Player -> Score = 10000
    ; Result == Opponent -> Score = -10000
    ; Result == 'draw' -> Score = 0
    ; Score is 0
    ).

% Check for open 3-in-a-row patterns
is_open3([P,P,P,0], P).
is_open3([0,P,P,P], P).

% Check for open 2-in-a-row patterns  
is_open2([P,P,0,0], P).
is_open2([0,0,P,P], P).

% Get cell value from board
get_cell(Board, R, C, V) :- nth0(R, Board, Row), nth0(C, Row, V).

% Extract horizontal window of 4 cells
window_row(Board, R, C, List) :- between(0,5,R), between(0,3,C), 
    findall(V, (between(0,3,I), C2 is C+I, get_cell(Board,R,C2,V)), List).

% Extract vertical window of 4 cells
window_col(Board, R, C, List) :- between(0,2,R), between(0,6,C), 
    findall(V, (between(0,3,I), R2 is R+I, get_cell(Board,R2,C,V)), List).

% Extract diagonal1 (/) window of 4 cells
window_diag1(Board, R, C, List) :- between(0,2,R), between(0,3,C), 
    findall(V, (between(0,3,I), R2 is R+I, C2 is C+I, get_cell(Board,R2,C2,V)), List).

% Extract diagonal2 (\) window of 4 cells
window_diag2(Board, R, C, List) :- between(0,2,R), between(3,6,C), 
    findall(V, (between(0,3,I), R2 is R+I, C2 is C-I, get_cell(Board,R2,C2,V)), List).

% Generic window extractor (any direction)
window(Board, List) :- 
    window_row(Board,_,_,List) ; 
    window_col(Board,_,_,List) ; 
    window_diag1(Board,_,_,List) ; 
    window_diag2(Board,_,_,List).

% Count open 3-in-a-row patterns for a player
count_open3(Board, Player, Count) :- 
    findall(1, (window(Board, List), is_open3(List, Player)), L), 
    length(L, Count).

% Count open 2-in-a-row patterns for a player
count_open2(Board, Player, Count) :- 
    findall(1, (window(Board, List), is_open2(List, Player)), L), 
    length(L, Count).

% Calculate center position score
center_score(Board, Player, Score) :- 
    findall(S, (between(0,5,R), between(0,6,C), get_cell(Board,R,C,Player), 
                S is 4 - abs(C-3)), L), 
    sum_list(L, Score).

% Calculate playable spaces score
playable_score(Board, Score) :- 
    count_empty(Board, Count), 
    Perc is Count / 42.0 * 100, 
    (Perc >= 60 -> Score = 1000 
    ; Perc >= 50 -> Score = 5 
    ; Perc >= 40 -> Score = 4 
    ; Score = 0).

% Count empty cells in the board
count_empty(Board, Count) :-
    findall(1, (member(Row,Board), member('.',Row)), L),
    length(L, Count).