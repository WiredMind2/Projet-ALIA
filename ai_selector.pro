:- consult('ia_random.pro').
:- consult('ia_presque_random.pro').
:- consult('ia_minimax.pro').

% Modify this predicate to select the AI for each player
get_ai_for_player('x', iaMinimax).
get_ai_for_player('o', iaRandom).