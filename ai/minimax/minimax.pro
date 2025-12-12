% Complete Minimax AI implementation with beam search (top 5 moves)
% This module contains the full minimax algorithm

:- consult('../game_utils.pro').
:- consult('../evaluation.pro').

% Main entry point for Minimax AI
minimax_ai(Board, NewBoard, Player) :-
    % 1. Vérifier si on peut gagner immédiatement
    (   find_winning_move(Board, Player, WinCol)
    ->  playMove(Board, WinCol, NewBoard, Player)
    % 2. Vérifier si on doit bloquer l'adversaire
    ;   changePlayer(Player, Opponent),
        find_winning_move(Board, Opponent, BlockCol)
    ->  playMove(Board, BlockCol, NewBoard, Player)
    % 3. Sinon, utiliser minimax
    ;   minimax(Board, 4, Player, BestCol, _),
        (   BestCol >= 0, BestCol =< 6
        ->  playMove(Board, BestCol, NewBoard, Player)
        ;   random_ai:random_ai(Board, NewBoard, Player)
        )
    ).

% Trouve un coup gagnant pour le joueur (si il existe)
find_winning_move(Board, Player, WinningCol) :-
    validMove(Col),
    simulateMove(Board, Col, NewBoard, Player),
    game_over(NewBoard, Result),
    Result == Player,
    WinningCol = Col,
    !. % Premier coup gagnant trouvé

% Base case: depth limit reached
minimax(Board, Depth, Player, BestCol, Score) :-
    Depth =< 0,
    !,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: game is over
minimax(Board, _Depth, Player, BestCol, Score) :-
    game_over(Board, Result), 
    Result \= 'no',
    !,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: no valid moves
minimax(Board, _Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves = [],
    !,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Recursive case: evaluate moves and keep only top 5
minimax(Board, Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [],
    !,
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    
    % Évaluer tous les coups avec leurs scores immédiats
    findall(ImmScore-Col, (
        member(Col, ValidMoves),
        simulateMove(Board, Col, TmpBoard, Player),
        evaluate(TmpBoard, Player, ImmScore)
    ), ScoredMoves),
    
    % Trier et garder les 5 meilleurs
    sort(ScoredMoves, Sorted),
    reverse(Sorted, Descending),
    take_first_n(5, Descending, TopMoves),
    
    % Explorer récursivement uniquement les 5 meilleurs
    findall(FinalScore-Col, (
        member(_-Col, TopMoves),
        simulateMove(Board, Col, NewBoard, Player),
        minimax(NewBoard, NewDepth, Opponent, _, OppScore),
        FinalScore is -OppScore
    ), ScoresCols),
    
    sort(ScoresCols, FinalSorted),
    last(FinalSorted, Score-BestCol).

% Prendre les N premiers éléments d'une liste
take_first_n(0, _, []) :- !.
take_first_n(_, [], []) :- !.
take_first_n(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    take_first_n(N1, T, R).

% Simulate a move without modifying the global state
simulateMove(Board, Col, NewBoard, Player) :-
    last_index(LastIndex),
    nth0(Col, LastIndex, Row),
    Row < 6,
    replaceMatrix(Board, Row, Col, Player, NewBoard).

% Base case: depth limit reached
minimax(Board, Depth, Player, BestCol, Score) :-
    Depth =< 0,
    !,  % Cut to prevent backtracking
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: game is over
minimax(Board, _Depth, Player, BestCol, Score) :-
    game_over(Board, Result), 
    Result \= 'no',
    !,  % Cut to prevent backtracking
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: no valid moves (draw situation)
minimax(Board, _Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves = [],
    !,  % Cut to prevent backtracking
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Recursive case: evaluate all possible moves
minimax(Board, Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [],
    !,  % Cut to prevent backtracking once we have valid moves
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    findall(Score-Col, (
        member(Col, ValidMoves), 
        simulateMove(Board, Col, NewBoard, Player), 
        minimax(NewBoard, NewDepth, Opponent, _, OppScore), 
        Score is -OppScore
    ), ScoresCols),
    ScoresCols \= [],  % Make sure we found some moves
    sort(ScoresCols, Sorted), 
    last(Sorted, Score-BestCol).

% Fallback case: if no scores found, return neutral evaluation
minimax(Board, _Depth, Player, BestCol, Score) :-
    evaluate(Board, Player, Score),
    BestCol = -1.