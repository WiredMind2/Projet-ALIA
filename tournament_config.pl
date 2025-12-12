% Tournament Configuration File
% Format: ai_config(Id, Type, Parameters)
% Type can be: random, minimax, alphabeta, etc.
% Parameters is a list of key-value pairs

% Random AI (baseline)
ai_config(random1, random, []).

% Minimax with different depths
ai_config(minimax_d3, minimax, [depth(3)]).
ai_config(minimax_d4, minimax, [depth(4)]).
ai_config(minimax_d5, minimax, [depth(5)]).

% Example: Minimax with different heuristics (when implemented)
% ai_config(minimax_basic, minimax, [depth(4), heuristic(basic)]).
% ai_config(minimax_advanced, minimax, [depth(4), heuristic(advanced)]).
% ai_config(minimax_aggressive, minimax, [depth(4), heuristic(aggressive)]).

% Example: Alpha-beta pruning (when implemented)
% ai_config(alphabeta_d5, alphabeta, [depth(5)]).
% ai_config(alphabeta_d6, alphabeta, [depth(6)]).
