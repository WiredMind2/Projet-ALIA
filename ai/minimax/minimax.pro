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
get_minimax_depth(4).  % Default search depth

% Alternative depth settings for different game phases
get_opening_depth(3).     % Faster moves in opening phase
get_middlegame_depth(4).  % Standard depth for midgame
get_endgame_depth(6).     % Deeper search in endgame when fewer pieces remain

% =============================================================================
% ENHANCED MINIMAX WITH LOGGING AND BEAM SEARCH
% =============================================================================

% Main entry point for Minimax AI with configurable depth and logging
minimax_ai(Board, NewBoard, Player) :-
    get_minimax_depth(Depth),
    info_log('Starting minimax AI with depth ~w', [Depth]),
    start_performance_timing,
    reset_counters,
    
    % 1. Vérifier si on peut gagner immédiatement
    (   find_winning_move(Board, Player, WinCol)
    ->  playMove(Board, WinCol, NewBoard, Player),
        info_log('Found immediate winning move in column ~w', [WinCol])
    % 2. Vérifier si on doit bloquer l'adversaire
    ;   changePlayer(Player, Opponent),
        find_winning_move(Board, Opponent, BlockCol)
    ->  playMove(Board, BlockCol, NewBoard, Player),
        info_log('Found blocking move in column ~w', [BlockCol])
    % 3. Sinon, utiliser minimax
    ;   minimax(Board, Depth, Player, BestCol, _),
        (   BestCol >= 0, BestCol =< 6
        ->  playMove(Board, BestCol, NewBoard, Player),
            info_log('AI made strategic move in column ~w', [BestCol])
        ;   random_ai:random_ai(Board, NewBoard, Player),
            info_log('AI made random move (no good moves found)', [])
        )
    ),
    
    end_performance_timing,
    % Log final statistics
    minimax_calls(CallCount),
    minimax_positions_evaluated(PositionCount),
    info_log('AI completed: ~w calls, ~w positions evaluated', [CallCount, PositionCount]).

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
    increment_position_counter,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: game is over
minimax(Board, _Depth, Player, BestCol, Score) :-
    game_over(Board, Result), 
    Result \= 'no',
    !,
    increment_position_counter,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Base case: no valid moves
minimax(Board, _Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves = [],
    !,
    increment_position_counter,
    evaluate(Board, Player, Score), 
    BestCol = -1.

% Recursive case: evaluate moves and keep only top 5 (beam search optimization)
minimax(Board, Depth, Player, BestCol, Score) :-
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
        minimax(NewBoard, NewDepth, Opponent, _, OppScore),
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

% Prendre les N premiers éléments d'une liste
take_first_n(0, _, []) :- !.
take_first_n(_, [], []) :- !.
take_first_n(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    take_first_n(N1, T, R).

% Fallback case: if no scores found, return neutral evaluation
minimax(Board, _Depth, Player, BestCol, Score) :-
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

% Get current statistics
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

% Original minimax predicate for backward compatibility
minimax_original(Board, Depth, Player, BestCol, Score) :-
    % Temporarily disable debug logging for performance
    disable_minimax_debug,
    minimax(Board, Depth, Player, BestCol, Score),
    enable_minimax_debug.