% AI selector for Connect Four game
% This module allows players to choose which AI implementation to use

% Ensure AI implementations are loaded
:- ensure_loaded('ai/random/random.pro').
:- ensure_loaded('ai/random/almost_random.pro').
:- ensure_loaded('ai/minimax.pro').

% get_ai_for_player(Player, AI)
% Interactive AI selection for a specific player
% Displays menu and allows player to choose AI type
% Player: player number (1 or 2) who needs an AI
% AI: returns the selected AI predicate name
get_ai_for_player(Player, AI) :-
    format('Select AI for player ~w:~n', [Player]), % Display selection prompt
    writeln('1 - Random AI'),                       % Option 1: Pure random
    writeln('2 - Almost Random AI (smart random)'), % Option 2: Smart random with blocking
    writeln('3 - Minimax AI'),                     % Option 3: Advanced minimax algorithm
    read(Choice),                                  % Read user input
    handle_ai_choice(Choice, AI).                  % Process the choice

% handle_ai_choice(Choice, AI)
% Maps user choice to corresponding AI implementation
% Choice: user's numeric selection (1-3)
% AI: returns the appropriate AI predicate
handle_ai_choice(1, random_ai) :- !.               % Choose random AI
handle_ai_choice(2, almost_random_ai) :- !.        % Choose smart random AI  
handle_ai_choice(3, minimax_ai) :- !.              % Choose minimax AI
handle_ai_choice(_, AI) :-                         % Invalid choice fallback
    writeln('Invalid choice, defaulting to random AI.'),
    AI = random_ai.

% minimax_ai(Board, NewBoard, Player)
% Minimax AI wrapper that matches the expected AI interface
% Initializes minimax algorithm and executes a move
% Board: current game board
% NewBoard: resulting board after the AI move
% Player: player number (1 or 2) making the move
minimax_ai(Board, NewBoard, Player) :-
    % Initialize minimax if not already done
    % Default settings: depth 5, heuristic 1 for both players
    DEPTH is 5,                                   % Search depth (number of moves ahead)
    HEURISTIC is 1,                               % Heuristic function (1=position-based, 2=alignment-based)
    % Initialize algorithm only if not already initialized
    (current_predicate(joueur1_profondeur/1), joueur1_profondeur(_) -> true 
     ; init_alpha_beta(DEPTH, HEURISTIC, DEPTH, HEURISTIC)),
    % Execute minimax algorithm to find best column
    coup_alpha_beta(Board, Player, Column),       % Get recommended column from minimax
    % Apply the chosen move
    playMove(Board, Column, NewBoard, Player).

% iaRandom(Board, NewBoard, Player)
% Alias for backward compatibility
% Provides consistent naming with other AI functions
% Board: current game board
% NewBoard: resulting board after the AI move
% Player: player number (1 or 2) making the move
iaRandom(Board, NewBoard, Player) :- random_ai(Board, NewBoard, Player).