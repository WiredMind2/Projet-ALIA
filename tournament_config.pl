% Tournament Configuration File
% Format: ai_config(Id, Type, Parameters)
% Type can be: random, presque_random, minimax
% Parameters is a list of key-value pairs: depth(N), alphabeta(true/false)

% Random AI (baseline)
ai_config(random1, random, []).

% Presque Random AI (blocks and takes wins)
ai_config(presque_random1, presque_random, []).

% Minimax WITHOUT Alpha-Beta Pruning (slower, for comparison)
ai_config(minimax_d2_noab, minimax, [depth(2), alphabeta(false)]).
ai_config(minimax_d3_noab, minimax, [depth(3), alphabeta(false)]).

% Minimax WITH Alpha-Beta Pruning (faster, recommended)
ai_config(minimax_d3_ab, minimax, [depth(3), alphabeta(true)]).
ai_config(minimax_d4_ab, minimax, [depth(4), alphabeta(true)]).
ai_config(minimax_d5_ab, minimax, [depth(5), alphabeta(true)]).

% Advanced configurations (if depth omitted, defaults to 4; if alphabeta omitted, defaults to true)
% ai_config(minimax_default, minimax, []).
% ai_config(minimax_d6_ab, minimax, [depth(6), alphabeta(true)]).
