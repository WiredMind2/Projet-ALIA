% Minimax algorithm with Alpha-Beta pruning for Connect Four
% Ported from JavaScript implementation in js/alphaBeta.js
% This implements a sophisticated AI using minimax algorithm with alpha-beta pruning optimization

:- consult('game_utils.pro').
:- consult('evaluation.pro').
:- consult('../win.pro').
:- consult('../matrix.pro').

% Global variables for alpha-beta algorithm
% These dynamic predicates store the algorithm state and configuration
:- dynamic colonne_max/1.         % Stores the best column found by minimax
:- dynamic joueur_courant/1.      % Current player being evaluated
:- dynamic joueur1_profondeur/1.  % Search depth limit for player 1
:- dynamic joueur2_profondeur/1.  % Search depth limit for player 2
:- dynamic joueur1_heuristique/1. % Heuristic function for player 1 (1 or 2)
:- dynamic joueur2_heuristique/1. % Heuristic function for player 2 (1 or 2)

% init_alpha_beta(J1Depth, J1Heuristic, J2Depth, J2Heuristic)
% Initialize alpha-beta algorithm parameters
% This predicate sets up the dynamic predicates used to control the search depth and evaluation method for each player
% J1Depth: Depth limit for Player 1 (how many moves ahead to search)
% J1Heuristic: Heuristic function index for Player 1 (1=position-based, 2=alignment-based)
% J2Depth: Depth limit for Player 2 (how many moves ahead to search)
% J2Heuristic: Heuristic function index for Player 2 (1=position-based, 2=alignment-based)
init_alpha_beta(J1Depth, J1Heuristic, J2Depth, J2Heuristic) :-
    retractall(colonne_max(_)),                  % Clear previous best column
    retractall(joueur1_profondeur(_)),           % Clear previous player 1 depth
    retractall(joueur2_profondeur(_)),           % Clear previous player 2 depth
    retractall(joueur1_heuristique(_)),          % Clear previous player 1 heuristic
    retractall(joueur2_heuristique(_)),          % Clear previous player 2 heuristic
    assert(joueur1_profondeur(J1Depth)),         % Set player 1 search depth
    assert(joueur2_profondeur(J2Depth)),         % Set player 2 search depth
    assert(joueur1_heuristique(J1Heuristic)),    % Set player 1 heuristic function
    assert(joueur2_heuristique(J2Heuristic)).    % Set player 2 heuristic function

% coup_alpha_beta(Board, CurrentPlayer, Column)
% Main entry point for the Alpha Beta algorithm
% Returns the column determined by the Alpha Beta algorithm as the best move
% Board: current game board
% CurrentPlayer: player number (1 or 2) to find best move for
% Column: returns the recommended column index (0-6)
coup_alpha_beta(Board, CurrentPlayer, Column) :-
    assert(joueur_courant(CurrentPlayer)),       % Store current player in global state
    valeur_max_ab(Board, 0, -10000, 10000, CurrentPlayer, _Value, _NewAlpha), % Run minimax
    colonne_max(Column).                         % Retrieve the best column found

% valeur_max_ab(Board, Depth, Alpha, Beta, Player, Value, NewAlpha)
% Recursive max function with alpha-beta pruning
% Implements the "max" part of minimax algorithm (trying to maximize score)
% Board: current game board state
% Depth: current search depth (0 at root, increases with each move)
% Alpha: current alpha value (best score max can guarantee so far)
% Beta: current beta value (best score min can guarantee so far)
% Player: player whose turn it is (maximizing player)
% Value: returns the evaluation score for this board position
% NewAlpha: returns updated alpha value after this evaluation

% Base case 1: Check for victory (terminal state)
% If someone has won, return appropriate score based on who won and depth
valeur_max_ab(Board, Depth, Alpha, _Beta, Player, Value, NewAlpha) :-
    win(Board, Winner),                          % Check if game is over
    Winner \= 0, !,                              % Someone has won
    (Winner == Player ->                         % Current player won
        Value is 1000 - Depth,                   % Prefer faster wins (less depth)
        NewAlpha = Alpha
    ;                                           % Opponent won
        Value is -1000 + Depth,                  % Prefer slower losses (more depth)
        NewAlpha = Alpha
    ).

% Base case 2: Check depth limits and draw condition
% If board is full or we've reached depth limit, use heuristic evaluation
valeur_max_ab(Board, _Depth, Alpha, _Beta, Player, Value, NewAlpha) :-
    board_full(Board), !,                        % Board is full - it's a draw
    % Use appropriate heuristic for this player
    (Player == 1 -> joueur1_heuristique(H) ; joueur2_heuristique(H)),
    (H == 1 -> heuristique1(Player, Board, HeurValue) ; heuristique2(Player, Board, HeurValue)),
    Value = HeurValue,                           % Return heuristic evaluation
    NewAlpha = Alpha.

% Base case 3: Check depth limits for player 1
% If we've reached the search depth limit for player 1, evaluate using heuristic
valeur_max_ab(Board, Depth, Alpha, _Beta, 1, Value, NewAlpha) :-
    joueur1_profondeur(Depth), !,                % Reached depth limit for player 1
    joueur1_heuristique(H),                      % Get heuristic function for player 1
    (H == 1 -> heuristique1(1, Board, HeurValue) ; heuristique2(1, Board, HeurValue)),
    Value = HeurValue,                           % Return heuristic evaluation
    NewAlpha = Alpha.

% Base case 4: Check depth limits for player 2
% If we've reached the search depth limit for player 2, evaluate using heuristic
valeur_max_ab(Board, Depth, Alpha, _Beta, 2, Value, NewAlpha) :-
    joueur2_profondeur(Depth), !,                % Reached depth limit for player 2
    joueur2_heuristique(H),                      % Get heuristic function for player 2
    (H == 1 -> heuristique1(2, Board, HeurValue) ; heuristique2(2, Board, HeurValue)),
    Value = HeurValue,                           % Return heuristic evaluation
    NewAlpha = Alpha.

% Main recursive case for max player
% Continue searching deeper into the game tree
valeur_max_ab(Board, Depth, Alpha, Beta, Player, Value, NewAlpha) :-
    NextDepth is Depth + 1,                      % Increment search depth
    changePlayer(Player, Opponent),              % Get opponent player number
    
    % Try all columns and get max value
    % Start with CurrentBestVal = Alpha, CurrentBestCol = -1
    valeur_max_recursive(Board, NextDepth, Alpha, Beta, Player, Opponent, 0, Alpha, -1, BestValue, BestColumn),
    
    % Store the best column for final decision
    retractall(colonne_max(_)),                  % Clear previous best column
    assert(colonne_max(BestColumn)),             % Store new best column
    
    Value = BestValue,                           % Return best evaluation found
    NewAlpha = BestValue.                        % Update alpha with best value

% valeur_max_recursive(Board, Depth, Alpha, Beta, Player, Opponent, CurrentCol, CurrentBestVal, CurrentBestCol, FinalBestVal, FinalBestCol)
% Recursively tries all columns to find the best move for the max player
% Board: current game board
% Depth: current search depth
% Alpha: current alpha value
% Beta: current beta value
% Player: current player (maximizing player)
% Opponent: opponent player number
% CurrentCol: column currently being evaluated (0-6)
% CurrentBestVal: best value found so far
% CurrentBestCol: column that gave the best value so far
% FinalBestVal: final best value after all columns evaluated
% FinalBestCol: final best column after all columns evaluated

% Base case: all columns evaluated
valeur_max_recursive(_, _, _, _, _, _, 7, BestVal, BestCol, BestVal, BestCol).

% Main case: column is playable, evaluate it
valeur_max_recursive(Board, Depth, Alpha, Beta, Player, Opponent, CurrentCol, CurrentBestVal, CurrentBestCol, FinalBestVal, FinalBestCol) :-
    validMove(Board, CurrentCol), !,             % Check if column is playable
    
    % Simulate the move for current player
    simulateMove(Board, CurrentCol, NewBoard, Player),
    
    % Recursive call to min (opponent's turn)
    % Pass CurrentBestVal as Alpha to min (this is the updated alpha)
    % IMPORTANT: Pass Player (MaxPlayer), not Opponent, to maintain perspective
    valeur_min_ab(NewBoard, Depth, CurrentBestVal, Beta, Player, MinValue, _),
    
    % Update best if this value is better
    ( (MinValue > CurrentBestVal ; CurrentBestCol == -1) ->
        NewBestVal = MinValue,                   % This is better
        NewBestCol = CurrentCol
    ; 
        NewBestVal = CurrentBestVal,             % Keep previous best
        NewBestCol = CurrentBestCol
    ),
    
    % Alpha-beta pruning check
    (NewBestVal >= Beta ->                       % Alpha-beta pruning condition
        % Prune the rest - this branch won't improve the result
        FinalBestVal = NewBestVal,
        FinalBestCol = NewBestCol
    ;
        % Continue to next column
        NextCol is CurrentCol + 1,
        valeur_max_recursive(Board, Depth, Alpha, Beta, Player, Opponent, NextCol, NewBestVal, NewBestCol, FinalBestVal, FinalBestCol)
    ).

% Skip case: column not playable, try next column
valeur_max_recursive(Board, Depth, Alpha, Beta, Player, Opponent, CurrentCol, CurrentBestVal, CurrentBestCol, FinalBestVal, FinalBestCol) :-
    % Column not playable, skip to next
    NextCol is CurrentCol + 1,
    valeur_max_recursive(Board, Depth, Alpha, Beta, Player, Opponent, NextCol, CurrentBestVal, CurrentBestCol, FinalBestVal, FinalBestCol).

% valeur_min_ab(Board, Depth, Alpha, Beta, Player, Value, NewBeta)
% Recursive min function with alpha-beta pruning
% Implements the "min" part of minimax algorithm (trying to minimize score)
% Board: current game board state
% Depth: current search depth
% Alpha: current alpha value (best score max can guarantee so far)
% Beta: current beta value (best score min can guarantee so far)
% Player: player whose turn it is (minimizing player)
% Value: returns the evaluation score for this board position
% NewBeta: returns updated beta value after this evaluation

% Base case 1: Check for victory (terminal state)
% If someone has won, return appropriate score based on who won and depth
valeur_min_ab(Board, Depth, _Alpha, Beta, Player, Value, NewBeta) :-
    win(Board, Winner),                          % Check if game is over
    Winner \= 0, !,                              % Someone has won
    (Winner == Player ->                         % Current player won
        Value is 1000 - Depth,                   % Prefer faster wins (less depth)
        NewBeta = Beta
    ;                                           % Opponent won
        Value is -1000 + Depth,                  % Prefer slower losses (more depth)
        NewBeta = Beta
    ).

% Base case 2: Check depth limits and draw condition
% If board is full or we've reached depth limit, use heuristic evaluation
valeur_min_ab(Board, _Depth, _Alpha, Beta, Player, Value, NewBeta) :-
    board_full(Board), !,                        % Board is full - it's a draw
    % Use appropriate heuristic for this player
    (Player == 1 -> joueur1_heuristique(H) ; joueur2_heuristique(H)),
    (H == 1 -> heuristique1(Player, Board, HeurValue) ; heuristique2(Player, Board, HeurValue)),
    Value = HeurValue,                           % Return heuristic evaluation
    NewBeta = Beta.

% Base case 3: Check depth limits for player 1
% If we've reached the search depth limit for player 1, evaluate using heuristic
valeur_min_ab(Board, Depth, _Alpha, Beta, 1, Value, NewBeta) :-
    joueur1_profondeur(Depth), !,                % Reached depth limit for player 1
    joueur1_heuristique(H),                      % Get heuristic function for player 1
    (H == 1 -> heuristique1(1, Board, HeurValue) ; heuristique2(1, Board, HeurValue)),
    Value = HeurValue,                           % Return heuristic evaluation
    NewBeta = Beta.

% Base case 4: Check depth limits for player 2
% If we've reached the search depth limit for player 2, evaluate using heuristic
valeur_min_ab(Board, Depth, _Alpha, Beta, 2, Value, NewBeta) :-
    joueur2_profondeur(Depth), !,                % Reached depth limit for player 2
    joueur2_heuristique(H),                      % Get heuristic function for player 2
    (H == 1 -> heuristique1(2, Board, HeurValue) ; heuristique2(2, Board, HeurValue)),
    Value = HeurValue,                           % Return heuristic evaluation
    NewBeta = Beta.

% Main recursive case for min player
% Continue searching deeper into the game tree
valeur_min_ab(Board, Depth, Alpha, Beta, Player, Value, NewBeta) :-
    NextDepth is Depth + 1,                      % Increment search depth
    changePlayer(Player, Opponent),              % Get opponent player number
    
    % Try all columns and get min value
    % Start with CurrentBestVal = Beta
    valeur_min_recursive(Board, NextDepth, Alpha, Beta, Player, Opponent, 0, Beta, BestValue),
    
    Value = BestValue,                           % Return best evaluation found
    NewBeta = BestValue.                         % Update beta with best value

% valeur_min_recursive(Board, Depth, Alpha, Beta, Player, Opponent, CurrentCol, CurrentBestVal, FinalBestVal)
% Recursively tries all columns to find the best move for the min player
% Board: current game board
% Depth: current search depth
% Alpha: current alpha value
% Beta: current beta value
% Player: current player (minimizing player)
% Opponent: opponent player number
% CurrentCol: column currently being evaluated (0-6)
% CurrentBestVal: best value found so far (lowest for min player)
% FinalBestVal: final best value after all columns evaluated

% Base case: all columns evaluated
valeur_min_recursive(_, _, _, _, _, _, 7, BestVal, BestVal).

% Main case: column is playable, evaluate it
valeur_min_recursive(Board, Depth, Alpha, Beta, Player, Opponent, CurrentCol, CurrentBestVal, FinalBestVal) :-
    validMove(Board, CurrentCol), !,             % Check if column is playable
    
    % Simulate the move for opponent
    simulateMove(Board, CurrentCol, NewBoard, Opponent),
    
    % Recursive call to max (player's turn)
    % IMPORTANT: Pass Player (MaxPlayer), not Opponent, to maintain perspective
    valeur_max_ab(NewBoard, Depth, Alpha, CurrentBestVal, Player, MaxValue, _),
    
    % Update best if this value is better (lower for min)
    (MaxValue < CurrentBestVal ->
        NewBestVal = MaxValue                    % This is better for min player
    ; 
        NewBestVal = CurrentBestVal              % Keep previous best
    ),
    
    % Alpha-beta pruning check
    (NewBestVal =< Alpha ->                      % Alpha-beta pruning condition
        % Prune the rest - this branch won't improve the result
        FinalBestVal = NewBestVal
    ;
        % Continue to next column
        NextCol is CurrentCol + 1,
        valeur_min_recursive(Board, Depth, Alpha, Beta, Player, Opponent, NextCol, NewBestVal, FinalBestVal)
    ).

% Skip case: column not playable, try next column
valeur_min_recursive(Board, Depth, Alpha, Beta, Player, Opponent, CurrentCol, CurrentBestVal, FinalBestVal) :-
    % Column not playable, skip to next
    NextCol is CurrentCol + 1,
    valeur_min_recursive(Board, Depth, Alpha, Beta, Player, Opponent, NextCol, CurrentBestVal, FinalBestVal).