% =============================================================================
% MINIMAX DEBUG UTILITIES - Comprehensive AI Decision Tracking
% =============================================================================
% This module provides extensive debugging and analysis tools for the minimax AI
% Includes detailed thinking process visualization, move quality analysis, and performance monitoring

:- consult('../puissance4.pro').
:- consult('../ai/minimax/minimax.pro').
:- consult('../ai/game_utils.pro').
:- consult('../ai/evaluation.pro').

% =============================================================================
% DEBUG SESSION MANAGEMENT
% =============================================================================

% Start a comprehensive debug session
start_debug_session :-
    writeln('=== STARTING MINIMAX DEBUG SESSION ==='),
    enable_minimax_logging,
    enable_minimax_debug,
    enable_performance_tracking,
    set_minimax_log_level(debug),
    info_log('Debug session started', []),
    writeln('Debug logging enabled - all AI decisions will be tracked').

% End debug session
end_debug_session :-
    writeln('=== ENDING MINIMAX DEBUG SESSION ==='),
    disable_minimax_debug,
    disable_minimax_logging,
    get_minimax_stats(Calls, Positions, Time),
    format('Final Statistics: ~w calls, ~w positions, ~w~n', [Calls, Positions, Time]),
    info_log('Debug session ended', []).

% Quick debug on single move
debug_single_move(Board, Player, Depth) :-
    start_debug_session,
    format('Debugging single move: Player ~w, Depth ~w~n', [Player, Depth]),
    minimax(Board, Depth, Player, BestCol, Score),
    format('Result: Column ~w, Score ~w~n', [BestCol, Score]),
    visualize_board_state(Board, Player, Depth, BestCol, Score),
    end_debug_session.

% =============================================================================
% DETAILED AI THINKING PROCESS VISUALIZATION
% =============================================================================

% Show detailed thinking process for a move
debug_ai_thinking(Board, Player, Depth) :-
    writeln('=== AI THINKING PROCESS ==='),
    enable_minimax_debug,
    
    % Show initial board state
    writeln('Initial Board State:'),
    print_board(Board),
    format('Current Player: ~w~n', [Player]),
    format('Search Depth: ~w~n', [Depth]),
    
    % Get valid moves
    findall(Col, validMove(Col), ValidMoves),
    format('Valid Moves: ~w~n', [ValidMoves]),
    
    % Show evaluation of each possible move
    debug_move_evaluations(Board, Player, Depth, ValidMoves),
    
    % Show final decision
    minimax(Board, Depth, Player, BestCol, Score),
    format('~nFinal Decision: Column ~w with score ~w~n', [BestCol, Score]),
    
    % Analyze the decision
    analyze_decision_quality(Board, BestCol, Player, Score),
    disable_minimax_debug.

% Debug move evaluations step by step
debug_move_evaluations(_, _, _, []).
debug_move_evaluations(Board, Player, Depth, [Col|Rest]) :-
    simulateMove(Board, Col, NewBoard, Player),
    format('~n--- Evaluating Move in Column ~w ---', [Col]),
    print_board(NewBoard),
    
    % Get immediate evaluation
    evaluate(NewBoard, Player, ImmediateScore),
    format('Immediate Score: ~w~n', [ImmediateScore]),
    
    % Check for immediate win/loss
    (game_over(NewBoard, Result), Result \= 'no' ->
        format('Game Over Result: ~w~n', [Result])
    ;   % Continue with deeper search
        changePlayer(Player, Opponent),
        NewDepth is Depth - 1,
        format('Searching opponent response at depth ~w...~n', [NewDepth]),
        minimax(NewBoard, NewDepth, Opponent, _, OppScore),
        FinalScore is -OppScore,
        format('Final Score for Column ~w: ~w~n', [Col, FinalScore])
    ),
    
    debug_move_evaluations(Board, Player, Depth, Rest).

% Analyze decision quality
analyze_decision_quality(Board, BestCol, Player, Score) :-
    writeln('~n=== DECISION QUALITY ANALYSIS ==='),
    
    % Analyze score
    (Score > 9000 -> writeln('*** EXCELLENT: Winning move found!');
     Score > 5000 -> writeln('*** GOOD: Strong positional advantage');
     Score > 1000 -> writeln('*** OK: Reasonable move');
     Score > 0 -> writeln('*** NEUTRAL: Safe move');
     Score < -1000 -> writeln('*** POOR: Likely losing move');
     writeln('*** UNKNOWN: Insufficient evaluation')),
    
    % Analyze board position
    analyze_board_threats(Board, Player),
    analyze_board_opportunities(Board, Player).

% Analyze threats to current player
analyze_board_threats(Board, Player) :-
    changePlayer(Player, Opponent),
    findall(Col, (
        validMove(Col),
        simulateMove(Board, Col, NewBoard, Opponent),
        evaluate(NewBoard, Opponent, OppScore),
        OppScore > 5000  % Opponent has winning position
    ), ThreatCols),
    
    (ThreatCols = [] -> writeln('No immediate threats detected');
     format('THREATS DETECTED: Opponent can win in columns ~w', [ThreatCols])).

% Analyze opportunities for current player
analyze_board_opportunities(Board, Player) :-
    findall(Col, (
        validMove(Col),
        simulateMove(Board, Col, NewBoard, Player),
        evaluate(NewBoard, Player, Score),
        Score > 1000  % Good position for player
    ), OpportunityCols),
    
    (OpportunityCols = [] -> writeln('No major opportunities detected');
     format('OPPORTUNITIES: Can establish strong positions in columns ~w', [OpportunityCols])).

% =============================================================================
% BOARD VISUALIZATION TOOLS
% =============================================================================

% Enhanced board display with move analysis
visualize_move_options(Board, Player) :-
    writeln('=== MOVE OPTIONS ANALYSIS ==='),
    print_board(Board),
    writeln(''),
    
    findall(Col, validMove(Col), ValidMoves),
    format('Valid moves for ~w: ~w~n', [Player, ValidMoves]),
    
    % Analyze each valid move
    analyze_all_moves(Board, Player, ValidMoves).

% Analyze all valid moves
analyze_all_moves(_, _, []).
analyze_all_moves(Board, Player, [Col|Rest]) :-
    simulateMove(Board, Col, NewBoard, Player),
    evaluate(NewBoard, Player, Score),
    
    % Classify move type
    classify_move(Score, MoveType),
    format('Column ~w: ~w (Score: ~w)~n', [Col, MoveType, Score]),
    
    analyze_all_moves(Board, Player, Rest).

% Classify move based on score
classify_move(Score, MoveType) :-
    Score > 9000 -> MoveType = 'WINNING MOVE';
    Score > 5000 -> MoveType = 'BLOCKING MOVE';
    Score > 1000 -> MoveType = 'POSITIONAL MOVE';
    Score > 100 -> MoveType = 'SAFE MOVE';
    Score > 0 -> MoveType = 'NEUTRAL MOVE';
    Score < -1000 -> MoveType = 'RISKY MOVE';
    MoveType = 'UNKNOWN'.

% Show board with move annotations
show_annotated_board(Board, Player) :-
    writeln('=== ANNOTATED BOARD ==='),
    print_board(Board),
    writeln(''),
    
    % Add annotations for each column
    writeln('Move Annotations:'),
    forall(between(0,6,Col), (
        (validMove(Col) ->
            simulateMove(Board, Col, NewBoard, Player),
            evaluate(NewBoard, Player, Score),
            classify_move(Score, MoveType),
            format('Column ~w: ~w (Score: ~w)~n', [Col, MoveType, Score])
        ;   format('Column ~w: INVALID (Column full)~n', [Col]))
    )).

% =============================================================================
% PERFORMANCE MONITORING AND ANALYSIS
% =============================================================================

% Monitor AI performance across multiple games
monitor_ai_performance(Iterations, Depth) :-
    writeln('=== AI PERFORMANCE MONITOR ==='),
    format('Running ~w iterations at depth ~w~n', [Iterations, Depth]),
    enable_performance_tracking,
    
    % Reset statistics
    reset_counters,
    
    % Run iterations
    forall(between(1, Iterations, _), (
        board(Board),
        minimax(Board, Depth, 'x', _, _)
    )),
    
    % Report results
    get_minimax_stats(Calls, Positions, Time),
    format('Performance Results:~n'),
    format('  Total calls: ~w~n', [Calls]),
    format('  Positions evaluated: ~w~n', [Positions]),
    format('  Total time: ~w~n', [Time]),
    (Calls > 0 -> 
        format('  Avg positions per call: ~2f~n', [Positions/Calls])
    ;   true).

% Compare performance across different depths
compare_depth_performance(Depths) :-
    writeln('=== DEPTH PERFORMANCE COMPARISON ==='),
    
    forall(member(Depth, Depths), (
        format('~n--- Testing Depth ~w ---', [Depth]),
        monitor_ai_performance(10, Depth)
    )).

% =============================================================================
% MOVE QUALITY ANALYSIS
% =============================================================================

% Analyze quality of AI move
analyze_move_quality_detailed(Board, Move, Player) :-
    simulateMove(Board, Move, NewBoard, Player),
    
    writeln('=== MOVE QUALITY ANALYSIS ==='),
    format('Move: Column ~w by Player ~w~n', [Move, Player]),
    print_board(NewBoard),
    
    % Immediate evaluation
    evaluate(NewBoard, Player, Score),
    format('Immediate Score: ~w~n', [Score]),
    
    % Check for immediate game end
    (game_over(NewBoard, Result), Result \= 'no' ->
        format('Game Result: ~w~n', [Result])
    ;   % Analyze deeper
        analyze_move_consequences(NewBoard, Player, Score)
    ).

% Analyze consequences of a move
analyze_move_consequences(NewBoard, Player, Score) :-
    changePlayer(Player, Opponent),
    
    writeln('Analyzing opponent response...'),
    findall(Col, validMove(Col), OppMoves),
    
    % Check if opponent has winning response
    findall(Col, (
        member(Col, OppMoves),
        simulateMove(NewBoard, Col, OppBoard, Opponent),
        evaluate(OppBoard, Opponent, OppScore),
        OppScore > 5000
    ), WinningResponses),
    
    (WinningResponses = [] ->
        format('No immediate winning responses for opponent~n', [])
    ;   format('WARNING: Opponent can win in columns ~w~n', [WinningResponses])),
    
    % Overall assessment
    format('~nOverall Assessment:~n'),
    (Score > 5000 -> writeln('  - Strong move, good position');
     Score > 1000 -> writeln('  - Reasonable move');
     Score > 0 -> writeln('  - Safe move, no immediate danger');
     writeln('  - Questionable move, consider alternatives')).

% =============================================================================
% STRESS TESTING AND EDGE CASES
% =============================================================================

% Test AI behavior on edge cases
test_ai_edge_cases :-
    writeln('=== AI EDGE CASE TESTING ==='),
    
    % Test 1: Nearly full board
    test_nearly_full_board,
    
    % Test 2: Multiple winning threats
    test_multiple_threats,
    
    % Test 3: Complex positional play
    test_complex_position,
    
    % Test 4: Endgame scenario
    test_endgame_scenario.

% Test with nearly full board
test_nearly_full_board :-
    writeln('~n--- Testing Nearly Full Board ---'),
    NearlyFullBoard = [[1,2,1,2,1,2,1],
                      [2,1,2,1,2,1,2],
                      [1,2,1,2,1,2,1],
                      [2,1,2,1,2,1,2],
                      [1,2,1,2,1,2,1],
                      [2,1,2,1,2,1,0]],  % Only column 6 has space
    
    debug_ai_thinking(NearlyFullBoard, 'x', 3).

% Test with multiple threats
test_multiple_threats :-
    writeln('~n--- Testing Multiple Threats ---'),
    ThreatBoard = [['x','x','x',0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,'o','o','o',0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0]],
    
    debug_ai_thinking(ThreatBoard, 'x', 4).

% Test complex positional play
test_complex_position :-
    writeln('~n--- Testing Complex Position ---'),
    ComplexBoard = [[0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,'x',0,'o',0,0],
                    [0,0,0,'x','o',0,0],
                    [0,0,'x','o','x','o',0],
                    [0,'o','x','o','x','o','x']],
    
    debug_ai_thinking(ComplexBoard, 'x', 5).

% Test endgame scenario
test_endgame_scenario :-
    writeln('~n--- Testing Endgame Scenario ---'),
    EndgameBoard = [[0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,'x','x','x',0,0],
                    ['o','o','x','o','o','x','o']],
    
    debug_ai_thinking(EndgameBoard, 'x', 6).

% =============================================================================
% COMPREHENSIVE DEBUGGING TOOLS
% =============================================================================

% Complete debugging workflow
debug_complete_workflow(Board, Player, Depth) :-
    start_debug_session,
    writeln('=== COMPLETE DEBUG WORKFLOW ==='),
    
    % Step 1: Initial analysis
    format('~nStep 1: Initial Board Analysis~n'),
    show_annotated_board(Board, Player),
    
    % Step 2: Thinking process
    format('~nStep 2: AI Thinking Process~n'),
    debug_ai_thinking(Board, Player, Depth),
    
    % Step 3: Final decision analysis
    format('~nStep 3: Final Decision Analysis~n'),
    minimax(Board, Depth, Player, BestCol, Score),
    analyze_move_quality_detailed(Board, BestCol, Player),
    
    % Step 4: Performance statistics
    format('~nStep 4: Performance Statistics~n'),
    get_minimax_stats(Calls, Positions, Time),
    format('Total processing: ~w calls, ~w positions, ~w~n', [Calls, Positions, Time]),
    
    end_debug_session.

% Quick debugging for development
quick_debug(Board, Player) :-
    format('Quick debug for Player ~w~n', [Player]),
    enable_minimax_debug,
    minimax_ai(Board, NewBoard, Player),
    disable_minimax_debug,
    print_board(NewBoard).

% =============================================================================
% UTILITY PREDICATES
% =============================================================================

% Reset all debug state
reset_debug_state :-
    reset_counters,
    disable_minimax_debug,
    disable_minimax_logging,
    set_minimax_log_level(info),
    writeln('Debug state reset').

% Export debug report
export_debug_report(Filename) :-
    open(Filename, write, Stream),
    format(Stream, '=== MINIMAX DEBUG REPORT ~w ===~n', [date]),
    get_minimax_stats(Calls, Positions, Time),
    format(Stream, 'Statistics: ~w calls, ~w positions, ~w~n', [Calls, Positions, Time]),
    close(Stream),
    format('Debug report exported to ~w', [Filename]).

% =============================================================================
% EXAMPLE DEBUG SESSIONS
% =============================================================================

% Example 1: Simple winning position
example_debug_winning :-
    board(Board),
    % Set up a winning position
    retractall(last_index(_)),
    assert(last_index([0,0,0,0,0,0,0])),
    playMove(Board, 0, Board1, 'x'),
    playMove(Board1, 1, Board2, 'x'),
    playMove(Board2, 2, Board3, 'x'),
    debug_ai_thinking(Board3, 'x', 2).

% Example 2: Blocking move
example_debug_blocking :-
    board(Board),
    % Set up position where blocking is needed
    retractall(last_index(_)),
    assert(last_index([0,0,0,0,0,0,0])),
    playMove(Board, 0, Board1, 'o'),
    playMove(Board1, 1, Board2, 'o'),
    playMove(Board2, 2, Board3, 'o'),
    debug_ai_thinking(Board3, 'x', 2).

% Example 3: Performance comparison
example_debug_performance :-
    writeln('=== PERFORMANCE COMPARISON EXAMPLE ==='),
    compare_depth_performance([2, 3, 4, 5]).

% Example 4: Complete workflow demonstration
example_debug_complete :-
    board(Board),
    % Create a mid-game position
    retractall(last_index(_)),
    assert(last_index([0,0,0,0,0,0,0])),
    playMove(Board, 3, Board1, 'x'),
    playMove(Board1, 2, Board2, 'o'),
    playMove(Board2, 4, Board3, 'x'),
    playMove(Board3, 1, Board4, 'o'),
    
    debug_complete_workflow(Board4, 'x', 4).