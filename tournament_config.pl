% Tournament Configuration File
% Format: ai_config(Id, Type, Parameters)
% Type: random, presque_random, minimax
% Parameters: depth(N), alphabeta(true/false)

% Random AI 
ai_config(random1, random, []).

% Presque Random AI
ai_config(presque_random1, presque_random, []).

% Minimax WITHOUT Alpha-Beta Pruning
ai_config(mm_d2_noab, minimax, [depth(2), alphabeta(false)]).
ai_config(mm_d3_noab, minimax, [depth(3), alphabeta(false)]).

% Minimax WITH Alpha-Beta Pruning
ai_config(mm_d3_ab, minimax, [depth(3), alphabeta(true)]).
ai_config(mm_d4_ab, minimax, [depth(4), alphabeta(true)]).
ai_config(mm_d5_ab, minimax, [depth(5), alphabeta(true)]).

