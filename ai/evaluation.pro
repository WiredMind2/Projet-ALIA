% Heuristic evaluation functions for AI implementations
% This module contains all the scoring and evaluation logic

:- consult('../matrix.pro').
:- consult('../win.pro').

% Main evaluation function for minimax
% Calcule score joueur - score adversaire avec TOUTES les heuristiques
evaluate(Board, Player, Score) :-
    game_over(Board, Result),
    changePlayer(Player, Opponent),
    ( Result == Player -> 
        Score = 10000
    ; Result == Opponent -> 
        Score = -10000
    ; Result == 'draw' -> 
        Score = 0
    ; % Partie non terminée : calculer toutes les heuristiques
        % Heuristiques pour le joueur
        center_score(Board, Player, MyCenter),
        space_between_opponent_tokens(Board, Player, MySpace),
        column_percentage_score(Board, Player, MyColScore),
        MyScore is MyCenter + MySpace + MyColScore,
        
        % Heuristiques pour l'adversaire (comptées négativement)
        center_score(Board, Opponent, OppCenter),
        space_between_opponent_tokens(Board, Opponent, OppSpace),
        column_percentage_score(Board, Opponent, OppColScore),
        OppScore is OppCenter + OppSpace + OppColScore,
        
        % Score différentiel
        Score is MyScore - OppScore
    ).

% Get cell value from board
get_cell(Board, R, C, V) :- nth0(R, Board, Row), nth0(C, Row, V).

% Calculate center position score (0 to 4 per token)
center_score(Board, Player, Score) :- 
    findall(S, (
        between(0,5,R), 
        between(0,6,C), 
        get_cell(Board,R,C,Player),
        Distance is abs(C - 3),
        S is 4 - Distance
    ), L), 
    sum_list(L, Score).

% Heuristique 3 : Pourcentage de cases jouées entre 2 jetons adverses (min 4 cases)
space_between_opponent_tokens(Board, Player, Score) :-
    changePlayer(Player, Opponent),
    findall(WindowScore, (
        window_with_opponent_edges(Board, Window, Opponent),
        calculate_space_score(Window, Player, WindowScore)
    ), Scores),
    sum_list(Scores, Score).

% Trouve les fenêtres de 4 avec des jetons adverses aux bords
window_with_opponent_edges(Board, Window, Opponent) :-
    (   window_row(Board, _, _, Window)
    ;   window_col(Board, _, _, Window)
    ;   window_diag1(Board, _, _, Window)
    ;   window_diag2(Board, _, _, Window)
    ),
    Window = [First, _, _, Last],
    First == Opponent,
    Last == Opponent.

% Calcule le score basé sur le % de jetons du joueur dans la fenêtre
calculate_space_score(Window, Player, Score) :-
    Window = [_, Cell1, Cell2, _],
    count_player_tokens([Cell1, Cell2], Player, PlayerCount),
    count_empty_cells([Cell1, Cell2], EmptyCount),
    Total is PlayerCount + EmptyCount,
    (   Total > 0 ->
        Percentage is (PlayerCount / Total) * 100,
        percentage_to_score(Percentage, Score)
    ;   Score = 0
    ).

% Convertit le pourcentage en score
percentage_to_score(Perc, Score) :-
    (   Perc >= 60 -> Score = 1000
    ;   Perc >= 50 -> Score = 5
    ;   Perc >= 40 -> Score = 4
    ;   Perc >= 30 -> Score = 3
    ;   Perc >= 20 -> Score = 2
    ;   Score = 0
    ).

% Compte les jetons du joueur dans une liste
count_player_tokens(List, Player, Count) :-
    findall(1, member(Player, List), L),
    length(L, Count).

% Compte les cases vides dans une liste
count_empty_cells(List, Count) :-
    findall(1, member('.', List), L),
    length(L, Count).

% Extract horizontal window of 4 cells
window_row(Board, R, C, List) :- 
    between(0,5,R), between(0,3,C), 
    findall(V, (between(0,3,I), C2 is C+I, get_cell(Board,R,C2,V)), List).

% Extract vertical window of 4 cells
window_col(Board, R, C, List) :- 
    between(0,2,R), between(0,6,C), 
    findall(V, (between(0,3,I), R2 is R+I, get_cell(Board,R2,C,V)), List).

% Extract diagonal1 (/) window of 4 cells
window_diag1(Board, R, C, List) :- 
    between(0,2,R), between(0,3,C), 
    findall(V, (between(0,3,I), R2 is R+I, C2 is C+I, get_cell(Board,R2,C2,V)), List).

% Extract diagonal2 (\) window of 4 cells
window_diag2(Board, R, C, List) :- 
    between(0,2,R), between(3,6,C), 
    findall(V, (between(0,3,I), R2 is R+I, C2 is C-I, get_cell(Board,R2,C2,V)), List).

% Count empty cells in the board
count_empty(Board, Count) :-
    findall(1, (member(Row,Board), member('.',Row)), L),
    length(L, Count).

% Heuristique bonus : Pourcentage de jetons dans chaque colonne (sans bornes)
column_percentage_score(Board, Player, TotalScore) :-
    findall(ColScore, (
        between(0, 6, Col),
        get_column(Board, Col, Column),
        calculate_column_score(Column, Player, ColScore)
    ), Scores),
    sum_list(Scores, TotalScore).

% Extraire une colonne du plateau
get_column(Board, Col, Column) :-
    findall(Cell, (
        between(0, 5, Row),
        get_cell(Board, Row, Col, Cell)
    ), Column).

% Calculer le score pour une colonne
calculate_column_score(Column, Player, Score) :-
    count_player_tokens(Column, Player, PlayerCount),
    count_empty_cells(Column, EmptyCount),
    Total is PlayerCount + EmptyCount,
    (   Total > 0 ->
        Percentage is (PlayerCount / Total) * 100,
        percentage_to_score(Percentage, Score)
    ;   Score = 0
    ).