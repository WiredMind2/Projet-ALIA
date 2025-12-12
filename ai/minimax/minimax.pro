% Complete Minimax AI implementation with beam search (top 5 moves) and logging
% This module contains the full minimax algorithm with enhanced logging and beam search optimization

:- consult('../game_utils.pro').
:- consult('../evaluation.pro').

% Tell Prolog that minimax/5 clauses are not contiguous
:- discontiguous minimax/5.

% =============================================================================
% LOGGING CONFIGURATION AND STATE
% =============================================================================

% Enable/disable logging
:- dynamic minimax_logging_enabled/1.
:- dynamic minimax_debug_mode/1.
:- dynamic minimax_log_level/1.
:- dynamic minimax_performance_tracking/1.

% Default logging settings
minimax_logging_enabled(true).
minimax_debug_mode(false).
minimax_log_level(info).  % Options: debug, info, warning, error
minimax_performance_tracking(true).

% Logging counters and state
:- dynamic minimax_calls/1.
:- dynamic minimax_positions_evaluated/1.
:- dynamic minimax_start_time/1.
:- dynamic minimax_last_move_time/1.

% Initialize counters
minimax_calls(0).
minimax_positions_evaluated(0).

% =============================================================================
% LOGGING HELPER PREDICATES
% =============================================================================

% Format current timestamp for logging
format_timestamp(Timestamp) :-
    get_time(Time),
    format_time(atom(Timestamp), '%Y-%m-%d %H:%M:%S.%3f', Time).

% Log message with configurable level
log_message(Level, Format, Args) :-
    minimax_logging_enabled(true),
    minimax_log_level(CurrentLevel),
    log_level_priority(Level, Priority),
    log_level_priority(CurrentLevel, CurrentPriority),
    Priority >= CurrentPriority,
    !,
    format_timestamp(Timestamp),
    sformat(Message, Format, Args),
    format('[~w] [MINIMAX-~w] ~w~n', [Timestamp, Level, Message]).
log_message(_, _, _).

% Define log level priorities
log_level_priority(debug, 1).
log_level_priority(info, 2).
log_level_priority(warning, 3).
log_level_priority(error, 4).

% Debug logging wrapper
debug_log(Format, Args) :-
    log_message(debug, Format, Args).
info_log(Format, Args) :-
    log_message(info, Format, Args).
warning_log(Format, Args) :-
    log_message(warning, Format, Args).
error_log(Format, Args) :-
    log_message(error, Format, Args).

% Performance timing utilities
start_performance_timing :-
    minimax_performance_tracking(true),
    !,
    get_time(StartTime),
    asserta(minimax_start_time(StartTime)).
start_performance_timing.

end_performance_timing :-
    minimax_performance_tracking(true),
    minimax_start_time(StartTime),
    !,
    get_time(EndTime),
    Elapsed is EndTime - StartTime,
    retract(minimax_start_time(StartTime)),
    sformat(Message, 'Performance timing: ~6f seconds', [Elapsed]),
    info_log(Message, []).

% Update position counter
increment_position_counter :-
    retract(minimax_positions_evaluated(Count)),
    NewCount is Count + 1,
    asserta(minimax_positions_evaluated(NewCount)),
    debug_log('Position evaluated (total: ~w)', [NewCount]).
increment_position_counter.

% Reset counters
reset_counters :-
    retractall(minimax_calls(_)),
    retractall(minimax_positions_evaluated(_)),
    asserta(minimax_calls(0)),
    asserta(minimax_positions_evaluated(0)),
    info_log('Counters reset', []).

% =============================================================================
% BOARD VISUALIZATION AND ANALYSIS
% =============================================================================

% Visualize board state with additional information
visualize_board_state(Board, Player, Depth, BestCol, Score) :-
    minimax_debug_mode(true),
    !,
    debug_log('=== BOARD STATE ANALYSIS ===', []),
    debug_log('Current Player: ~w', [Player]),
    debug_log('Search Depth: ~w', [Depth]),
    debug_log('Best Move: Column ~w', [BestCol]),
    debug_log('Score: ~w', [Score]),
    debug_log('Board State:', []),
    print_board_with_coords(Board),
    debug_log('=== END ANALYSIS ===', []).
visualize_board_state(_, _, _, _, _).

% Print board with coordinates for debugging
print_board_with_coords(Board) :-
    write('   0 1 2 3 4 5 6'), nl,
    Board = [Row0,Row1,Row2,Row3,Row4,Row5],
    format('5 ~w ~w ~w ~w ~w ~w ~w~n', [Row0]),
    format('4 ~w ~w ~w ~w ~w ~w ~w~n', [Row1]),
    format('3 ~w ~w ~w ~w ~w ~w ~w~n', [Row2]),
    format('2 ~w ~w ~w ~w ~w ~w ~w~n', [Row3]),
    format('1 ~w ~w ~w ~w ~w ~w ~w~n', [Row4]),
    format('0 ~w ~w ~w ~w ~w ~w ~w~n', [Row5]).

% Analyze move quality
analyze_move_quality(_Board, Move, _NewBoard, _Player, Score) :-
    minimax_debug_mode(true),
    !,
    debug_log('--- MOVE ANALYSIS ---', []),
    debug_log('Move: Column ~w', [Move]),
    debug_log('Score: ~w', [Score]),
    % Check if this is a winning move
    (Score > 9000 -> debug_log('*** WINNING MOVE DETECTED ***', []); true),
    % Check if this blocks opponent win
    (Score > 5000, Score =< 9000 -> debug_log('*** BLOCKING MOVE ***', []); true),
    % Check if this is a good positional move
    (Score > 100, Score =< 5000 -> debug_log('*** GOOD POSITIONAL MOVE ***', []); true),
    % Check if this is a defensive move
    (Score < -100 -> debug_log('*** DEFENSIVE MOVE ***', []); true),
    debug_log('--- END ANALYSIS ---', []).
analyze_move_quality(_, _, _, _, _).

% =============================================================================
% CONFIGURABLE DEPTH SETTINGS
% =============================================================================

% Configurable depth settings for minimax AI
% Default depth provides good balance of speed and quality
get_minimax_depth(7).  % Default search depth (changed from 4)

% Alternative depth settings for different game phases
get_opening_depth(3).     % Faster moves in opening phase
get_middlegame_depth(4).  % Standard depth for midgame
get_endgame_depth(6).     % Deeper search in endgame when fewer pieces remain

% =============================================================================
% ALPHA-BETA PRUNING COUNTERS
% =============================================================================

% Track alpha-beta pruning statistics
:- dynamic alpha_beta_prunes/1.
:- dynamic alpha_beta_window_updates/1.

alpha_beta_prunes(0).
alpha_beta_window_updates(0).

% Increment pruning counter
increment_prune_counter :-
    retract(alpha_beta_prunes(Count)),
    NewCount is Count + 1,
    asserta(alpha_beta_prunes(NewCount)),
    debug_log('Alpha-beta prune (total: ~w)', [NewCount]).
increment_prune_counter.

% Increment window update counter
increment_window_counter :-
    retract(alpha_beta_window_updates(Count)),
    NewCount is Count + 1,
    asserta(alpha_beta_window_updates(NewCount)),
    debug_log('Window update (total: ~w)', [NewCount]).
increment_window_counter.

% Reset alpha-beta counters
reset_alpha_beta_counters :-
    retractall(alpha_beta_prunes(_)),
    retractall(alpha_beta_window_updates(_)),
    asserta(alpha_beta_prunes(0)),
    asserta(alpha_beta_window_updates(0)).

% =============================================================================
% ALPHA-BETA PRUNING IMPLEMENTATION (Negamax variant)
% =============================================================================

% Main alpha-beta predicate with negamax approach
% alpha_beta(Depth, Position, Alpha, Beta, BestMove, BestValue)
alpha_beta(0, Board, _Alpha, _Beta, Player, Move, Value) :-
    !,
    increment_position_counter,
    evaluate(Board, Player, Value),
    Move = -1,
    debug_log('Leaf node: Value = ~w', [Value]).

alpha_beta(_Depth, Board, _Alpha, _Beta, Player, Move, Value) :-
    game_over(Board, Result),
    Result \= 'no',
    !,
    increment_position_counter,
    evaluate(Board, Player, Value),
    Move = -1,
    debug_log('Terminal node: Result = ~w, Value = ~w', [Result, Value]).

alpha_beta(Depth, Board, Alpha, Beta, Player, BestMove, BestValue) :-
    findall(Col, validMove(Col), Moves),
    Moves \= [],
    !,
    debug_log('Alpha-beta at depth ~w: Alpha=~w, Beta=~w, Moves=~w', 
              [Depth, Alpha, Beta, Moves]),
    
    % Evaluate moves with beam search optimization (top 5)
    findall(ImmScore-Col, (
        member(Col, Moves),
        simulateMove(Board, Col, TmpBoard, Player),
        evaluate(TmpBoard, Player, ImmScore)
    ), ScoredMoves),
    
    sort(ScoredMoves, Sorted),
    reverse(Sorted, Descending),
    take_first_n(5, Descending, TopMoves),
    findall(TopCol, member(_-TopCol, TopMoves), TopCols),
    
    debug_log('Beam search: kept top ~w moves: ~w', [length(TopCols), TopCols]),
    
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    Depth1 is Depth - 1,
    changePlayer(Player, Opponent),
    
    evaluate_and_choose(TopCols, Board, Depth1, Alpha1, Beta1, Player, Opponent, nil, (BestMove, BestValue)),
    
    debug_log('Alpha-beta result at depth ~w: Move=~w, Value=~w', 
              [Depth, BestMove, BestValue]).

% No valid moves - return evaluation
alpha_beta(_Depth, Board, _Alpha, _Beta, Player, Move, Value) :-
    increment_position_counter,
    evaluate(Board, Player, Value),
    Move = -1,
    debug_log('No valid moves: Value = ~w', [Value]).

% =============================================================================
% EVALUATE AND CHOOSE - Process moves and select best
% =============================================================================

% Process the first move in the list
evaluate_and_choose([Move | Moves], Board, Depth, Alpha, Beta, Player, Opponent, _CurrentBest, BestResult) :-
    !,
    simulateMove(Board, Move, Position1, Player),
    debug_log('Evaluating move ~w at depth ~w', [Move, Depth]),
    
    alpha_beta(Depth, Position1, Alpha, Beta, Opponent, _MoveX, Value),
    Value1 is -Value,
    
    debug_log('Move ~w: Value = ~w (negated = ~w)', [Move, Value, Value1]),
    
    cutoff(Move, Value1, Depth, Alpha, Beta, Moves, Board, Player, Opponent, BestResult).

% No more moves to evaluate - return current best (alpha value)
evaluate_and_choose([], _Board, _Depth, Alpha, _Beta, _Player, _Opponent, CurrentBest, Result) :-
    !,
    (CurrentBest = nil ->
        Result = (-1, Alpha),
        debug_log('No moves evaluated: returning Alpha = ~w', [Alpha])
    ;
        Result = CurrentBest,
        debug_log('All moves evaluated: Best = ~w', [CurrentBest])
    ).

% =============================================================================
% CUTOFF - Handle alpha-beta pruning cases
% =============================================================================

% Case 1: Beta cutoff - Value exceeds Beta (prune remaining moves)
cutoff(Move, Value, _Depth, _Alpha, Beta, _Moves, _Board, _Player, _Opponent, (Move, Value)) :-
    Value >= Beta,
    !,
    increment_prune_counter,
    debug_log('BETA CUTOFF: Move=~w, Value=~w >= Beta=~w (pruned)', [Move, Value, Beta]).

% Case 2: Value improves Alpha - Update window and continue
cutoff(Move, Value, Depth, Alpha, Beta, Moves, Board, Player, Opponent, BestResult) :-
    Value > Alpha,
    Value < Beta,
    !,
    increment_window_counter,
    debug_log('WINDOW UPDATE: Move=~w, Value=~w in (Alpha=~w, Beta=~w)', [Move, Value, Alpha, Beta]),
    evaluate_and_choose(Moves, Board, Depth, Value, Beta, Player, Opponent, (Move, Value), BestResult).

% Case 3: Value doesn't improve Alpha - Continue with current Alpha
cutoff(_Move, Value, Depth, Alpha, Beta, Moves, Board, Player, Opponent, BestResult) :-
    Value =< Alpha,
    !,
    debug_log('NO IMPROVEMENT: Value=~w =< Alpha=~w', [Value, Alpha]),
    evaluate_and_choose(Moves, Board, Depth, Alpha, Beta, Player, Opponent, nil, BestResult).

% =============================================================================
% ENHANCED MINIMAX WITH LOGGING AND ALPHA-BETA PRUNING
% =============================================================================

% Main entry point for Minimax AI with alpha-beta pruning
minimax_ai(Board, NewBoard, Player) :-
    get_minimax_depth(Depth),
    info_log('Starting alpha-beta AI with depth ~w', [Depth]),
    start_performance_timing,
    reset_counters,
    reset_alpha_beta_counters,
    
    % 1. Vérifier si on peut gagner immédiatement
    (   find_winning_move(Board, Player, WinCol)
    ->  playMove(Board, WinCol, NewBoard, Player),
        info_log('Found immediate winning move in column ~w', [WinCol])
    % 2. Vérifier si on doit bloquer l'adversaire
    ;   changePlayer(Player, Opponent),
        find_winning_move(Board, Opponent, BlockCol)
    ->  playMove(Board, BlockCol, NewBoard, Player),
        info_log('Found blocking move in column ~w', [BlockCol])
    % 3. Sinon, utiliser alpha-beta pruning
    ;   Alpha is -100000,
        Beta is 100000,
        alpha_beta(Depth, Board, Alpha, Beta, Player, BestCol, BestValue),
        (   BestCol >= 0, BestCol =< 6
        ->  playMove(Board, BestCol, NewBoard, Player),
            info_log('AI made strategic move in column ~w with value ~w', [BestCol, BestValue])
        ;   random_ai:random_ai(Board, NewBoard, Player),
            info_log('AI made random move (no good moves found)', [])
        )
    ),
    
    end_performance_timing,
    % Log final statistics including pruning info
    minimax_calls(CallCount),
    minimax_positions_evaluated(PositionCount),
    alpha_beta_prunes(PruneCount),
    alpha_beta_window_updates(WindowCount),
    info_log('AI completed: ~w calls, ~w positions, ~w prunes, ~w window updates', 
             [CallCount, PositionCount, PruneCount, WindowCount]).

% Trouve un coup gagnant pour le joueur (si il existe)
find_winning_move(Board, Player, WinningCol) :-
    validMove(Col),
    simulateMove(Board, Col, NewBoard, Player),
    game_over(NewBoard, Result),
    Result == Player,
    WinningCol = Col,
    !. % Premier coup gagnant trouvé

% Prendre les N premiers éléments d'une liste
take_first_n(0, _, []) :- !.
take_first_n(_, [], []) :- !.
take_first_n(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    take_first_n(N1, T, R).

% =============================================================================
% OLD MINIMAX WITHOUT PRUNING (for comparison/testing)
% =============================================================================

% Base case: depth limit reached
minimax_no_pruning(Board, Depth, Player, BestCol, Score) :-
    Depth =< 0,
    !,
    increment_position_counter,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: game is over
minimax_no_pruning(Board, _Depth, Player, BestCol, Score) :-
    game_over(Board, Result), 
    Result \= 'no',
    !,
    increment_position_counter,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: no valid moves
minimax_no_pruning(Board, _Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves = [],
    !,
    increment_position_counter,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Recursive case: evaluate moves and keep only top 5 (beam search optimization)
minimax_no_pruning(Board, Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [],
    !,
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    
    % Log current search state
    debug_log('Searching depth ~w, Player ~w, ~w valid moves available', 
              [Depth, Player, length(ValidMoves)]),
    
    % Évaluer tous les coups avec leurs scores immédiats
    findall(ImmScore-Col, (
        member(Col, ValidMoves),
        simulateMove(Board, Col, TmpBoard, Player),
        evaluate(TmpBoard, Player, ImmScore)
    ), ScoredMoves),
    
    % Trier et garder les 5 meilleurs (beam search)
    sort(ScoredMoves, Sorted),
    reverse(Sorted, Descending),
    take_first_n(5, Descending, TopMoves),
    
    debug_log('Beam search: kept top ~w moves from ~w candidates', 
              [length(TopMoves), length(ValidMoves)]),
    
    % Explorer récursivement uniquement les 5 meilleurs
    findall(FinalScore-Col, (
        member(_-Col, TopMoves),
        simulateMove(Board, Col, NewBoard, Player),
        minimax_no_pruning(NewBoard, NewDepth, Opponent, _, OppScore),
        FinalScore is -OppScore
    ), ScoresCols),
    
    ScoresCols \= [],
    sort(ScoresCols, FinalSorted),
    last(FinalSorted, Score-BestCol),
    
    % Update call counter and log final decision
    retract(minimax_calls(CallCount)),
    NewCallCount is CallCount + 1,
    asserta(minimax_calls(NewCallCount)),
    
    debug_log('Depth ~w complete: Best move = column ~w with score ~w', 
              [Depth, BestCol, Score]).

% Fallback case: if no scores found, return neutral evaluation
minimax_no_pruning(Board, _Depth, Player, BestCol, Score) :-
    increment_position_counter,
    evaluate(Board, Player, Score),
    BestCol = -1,
    warning_log('Fallback case triggered: No valid moves found', []).

% =============================================================================
% LOGGING CONTROL PREDICATES
% =============================================================================

% Enable comprehensive logging
enable_minimax_logging :-
    retractall(minimax_logging_enabled(_)),
    asserta(minimax_logging_enabled(true)),
    info_log('Minimax logging enabled', []).

% Disable logging
disable_minimax_logging :-
    retractall(minimax_logging_enabled(_)),
    asserta(minimax_logging_enabled(false)).

% Enable debug mode
enable_minimax_debug :-
    retractall(minimax_debug_mode(_)),
    asserta(minimax_debug_mode(true)),
    info_log('Minimax debug mode enabled', []).

% Disable debug mode
disable_minimax_debug :-
    retractall(minimax_debug_mode(_)),
    asserta(minimax_debug_mode(false)).

% Set log level
set_minimax_log_level(Level) :-
    member(Level, [debug, info, warning, error]),
    retractall(minimax_log_level(_)),
    asserta(minimax_log_level(Level)),
    info_log('Log level set to ~w', [Level]).
set_minimax_log_level(Level) :-
    warning_log('Invalid log level: ~w. Use: debug, info, warning, error', [Level]).

% Enable performance tracking
enable_performance_tracking :-
    retractall(minimax_performance_tracking(_)),
    asserta(minimax_performance_tracking(true)),
    info_log('Performance tracking enabled', []).

% Disable performance tracking
disable_performance_tracking :-
    retractall(minimax_performance_tracking(_)),
    asserta(minimax_performance_tracking(false)).

% Get current statistics including alpha-beta info
get_minimax_stats(Calls, Positions, Prunes, WindowUpdates, TimeInfo) :-
    minimax_calls(Calls),
    minimax_positions_evaluated(Positions),
    alpha_beta_prunes(Prunes),
    alpha_beta_window_updates(WindowUpdates),
    (minimax_start_time(StartTime) -> 
        get_time(CurrentTime),
        Elapsed is CurrentTime - StartTime,
        sformat(TimeInfo, '~6f seconds', [Elapsed])
    ; TimeInfo = 'N/A').

% Backward compatibility version
get_minimax_stats(Calls, Positions, TimeInfo) :-
    minimax_calls(Calls),
    minimax_positions_evaluated(Positions),
    (minimax_start_time(StartTime) -> 
        get_time(CurrentTime),
        Elapsed is CurrentTime - StartTime,
        sformat(TimeInfo, '~6f seconds', [Elapsed])
    ; TimeInfo = 'N/A').

% =============================================================================
% COMPATIBILITY PREDICATES
% =============================================================================

% Maintain compatibility with original API while adding logging
minimax_ai_simple(Board, NewBoard, Player) :-
    % Temporarily disable logging for backward compatibility
    disable_minimax_logging,
    minimax_ai(Board, NewBoard, Player),
    enable_minimax_logging.

% Original minimax predicate for backward compatibility - now uses alpha-beta
minimax(Board, Depth, Player, BestCol, Score) :-
    Alpha is -100000,
    Beta is 100000,
    alpha_beta(Depth, Board, Alpha, Beta, Player, BestCol, Score).

% Original minimax predicate without alpha-beta (for testing/comparison)
minimax_original(Board, Depth, Player, BestCol, Score) :-
    % Temporarily disable debug logging for performance
    disable_minimax_debug,
    minimax_no_pruning(Board, Depth, Player, BestCol, Score),
    enable_minimax_debug.