% AI selector module
% Provides get_ai_for_player/2 to select AI type for a player

:- consult('random/random.pro').
:- consult('random/almost_random.pro').
:- consult('minimax.pro').

% get_ai_for_player(Player, AI)
% Prompts user to select AI type and returns the corresponding predicate
get_ai_for_player(Player, AI) :-
    format('Select AI for player ~w:~n', [Player]),
    writeln('1 - Random AI'),
    writeln('2 - Almost Random AI (smart random)'),
    writeln('3 - Minimax AI'),
    read(Choice),
    handle_ai_choice(Choice, AI).

handle_ai_choice(1, random_ai) :- !.
handle_ai_choice(2, almost_random_ai) :- !.
handle_ai_choice(3, minimax_ai) :- !.
handle_ai_choice(_, AI) :-
    writeln('Invalid choice, defaulting to random AI.'),
    AI = random_ai.

% minimax_ai wrapper to match the interface
minimax_ai(Board, NewBoard, Player) :-
    % Convert player 'x'/'o' to 1/2
    (Player = 'x' -> PlayerNum = 1 ; PlayerNum = 2),
    % Initialize minimax if not already done (depth 3, heuristic 1 for both)
    (joueur1_profondeur(_) -> true ; init_alpha_beta(3, 1, 3, 1)),
    coup_alpha_beta(Board, PlayerNum, Column),
    playMove(Board, Column, NewBoard, Player).