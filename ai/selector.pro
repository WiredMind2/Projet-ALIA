% AI Selection Logic
% This module provides functions to select the appropriate AI for each player

% Get AI for a specific player
% Modify these predicates to change which AI each player uses
get_ai_for_player('x', minimax_ai).
get_ai_for_player('o', random_ai).

% Alternative configurations for testing
get_ai_for_minimax_vs_random(minimax_ai, random_ai).
get_ai_for_random_vs_minimax(random_ai, minimax_ai).
get_ai_for_minimax_vs_almost_random(minimax_ai, almost_random_ai).
get_ai_for_almost_random_vs_minimax(almost_random_ai, minimax_ai).
get_ai_for_random_vs_almost_random(random_ai, almost_random_ai).
get_ai_for_almost_random_vs_random(almost_random_ai, random_ai).