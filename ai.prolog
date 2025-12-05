% File: ai.prolog
% Purpose: This module provides the artificial intelligence for the Connect 4 game using the minimax algorithm with depth-limited search and heuristic evaluation.
% It includes utility functions for board evaluation, minimax decision making, and random move selection among equally good options.
% Adapted from morpion.prolog: morpion.prolog implements minimax for Tic-Tac-Toe with full tree search and simple utility (1 for win, -1 for loss, 0 otherwise). This version adds depth limiting (to 6 for performance), a center column heuristic for non-terminal boards, and random selection among optimal moves to avoid predictability.

% AI module for Connect 4 using minimax algorithm

:- use_module(library(pairs)).

% Facts defining maximizing and minimizing players, and opponent relationships.
% maximizing(Mark): succeeds if Mark is the maximizing player ('x')
% minimizing(Mark): succeeds if Mark is the minimizing player ('o')
% opponent_mark(Mark, Opponent): returns the opponent mark
% Adapted from morpion.prolog: similar facts for players.
maximizing('x').
minimizing('o').
opponent_mark('x', 'o').
opponent_mark('o', 'x').

% utility(Board, Value)
% Evaluates the board position: 1 if 'x' wins, -1 if 'o' wins, otherwise uses center heuristic.
% Board: list of lists, current board
% Value: number, the utility value
% Adapted from morpion.prolog: morpion's utility is 1 for x win, -1 for o win, 0 otherwise. This adds heuristic for non-terminal positions.
utility(Board, Value) :-
    win(Board, 'x') -> Value = 1
    ; win(Board, 'o') -> Value = -1
    ; center_heuristic(Board, H), Value = H.

% center_heuristic(Board, Heuristic)
% Computes a heuristic value based on piece count in center columns (3,4,5), weighted by position.
% Board: list of lists, the board
% Heuristic: float, the heuristic value (between -1 and 1)
% Adapted from morpion.prolog: morpion has no heuristic; this is new for Connect 4 to evaluate non-winning positions.
center_heuristic(B, H) :-
    count_center(B, 'x', Xc),
    count_center(B, 'o', Oc),
    H is 0.1 * (Xc - Oc).

% count_center(Board, Mark, Count)
% Counts the weighted pieces of Mark in center columns: col4 *3, col3 *2, col5 *2.
% Board: list of lists, the board
% Mark: atom ('x' or 'o'), the mark to count
% Count: integer, the weighted count
count_center(B, M, C) :-
    get_column(B, 4, Col4),
    count_in_list(Col4, M, C4),
    get_column(B, 3, Col3),
    count_in_list(Col3, M, C3),
    get_column(B, 5, Col5),
    count_in_list(Col5, M, C5),
    C is C4 * 3 + C3 * 2 + C5 * 2.

% count_in_list(List, Mark, Count)
% Counts occurrences of Mark in the list.
% List: list, the column
% Mark: atom, the mark
% Count: integer, number of occurrences
count_in_list(L, M, C) :-
    include(=(M), L, Ms),
    length(Ms, C).

% minimax(Depth, Board, Mark, BestColumn)
% Selects the best column for Mark using minimax algorithm with depth limit of 6.
% Depth: integer, current depth (starts at 6)
% Board: list of lists, current board
% Mark: atom ('x' or 'o'), the player's mark
% BestColumn: integer (1-7), the chosen column
% Adapted from morpion.prolog: morpion's minimax searches full tree; this limits depth to 6 for Connect 4's larger state space.
minimax(Depth, Board, Mark, BestColumn) :-
    available_moves(Board, Moves),
    (maximizing(Mark) ->
        findall(S-U, (member(S, Moves), drop_piece(Board, S, Mark, B2), opponent_mark(Mark, Opp), minimax_value(Depth-1, B2, Opp, U)), Pairs),
        pairs_values(Pairs, Us),
        max_list(Us, MaxU),
        findall(S, member(S-MaxU, Pairs), Ss),
        random_member(BestColumn, Ss)
    ;
        findall(S-U, (member(S, Moves), drop_piece(Board, S, Mark, B2), opponent_mark(Mark, Opp), minimax_value(Depth-1, B2, Opp, U)), Pairs),
        pairs_values(Pairs, Us),
        min_list(Us, MinU),
        findall(S, member(S-MinU, Pairs), Ss),
        random_member(BestColumn, Ss)
    ).

% minimax_value(D, B, M, U)
% Computes the minimax value for board B with player M to move, at depth D.
% D: integer, remaining depth
% B: list of lists, board
% M: atom, current player
% U: number, the minimax value
% Adapted from morpion.prolog: similar to morpion's minimax, but with depth limit and heuristic.
minimax_value(D, B, M, U) :-
    (D =< 0 ; \+ available_moves(B, _)) ->
        utility(B, U)
    ;
    available_moves(B, L),
    findall(U1, (member(S, L), drop_piece(B, S, M, B2), opponent_mark(M, M2), minimax_value(D-1, B2, M2, U1)), Us),
    (maximizing(M) -> max_list(Us, U) ; min_list(Us, U)).

% random_member(X, L)
% Selects a random element from list L.
% X: the selected element
% L: list, non-empty
% Adapted from morpion.prolog: morpion has random_int_1n; this uses random/2 for list selection.
random_member(X, L) :-
    length(L, Len),
    Len > 0,
    random(0, Len, I),
    nth0(I, L, X).