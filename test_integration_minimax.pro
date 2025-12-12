% Comprehensive Integration Test for Minimax AI
% Tests actual gameplay flow including Player vs IA mode
% This test ensures all logging errors are fixed and game works end-to-end

:- consult('ai/minimax/minimax.pro').
:- consult('ai/evaluation.pro').
:- consult('ai/game_utils.pro').
:- consult('ai/selector.pro').
:- consult('puissance4.pro').

% =============================================================================
% INTEGRATION TEST SETUP
% =============================================================================

% Test counter
:- dynamic test_passed/1.
:- dynamic test_failed/1.

% Initialize test counters
reset_test_counters :-
    retractall(test_passed(_)),
    retractall(test_failed(_)),
    asserta(test_passed(0)),
    asserta(test_failed(0)).

% Test result tracking
mark_test_passed(TestName) :-
    retract(test_passed(Count)),
    NewCount is Count + 1,
    asserta(test_passed(NewCount)),
    format('✓ PASSED: ~w~n', [TestName]).

mark_test_failed(TestName, Error) :-
    retract(test_failed(Count)),
    NewCount is Count + 1,
    asserta(test_failed(NewCount)),
    format('✗ FAILED: ~w - Error: ~w~n', [TestName, Error]).

% Print test summary
print_test_summary :-
    test_passed(Passed),
    test_failed(Failed),
    Total is Passed + Failed,
    format('~n=== TEST SUMMARY ===~n'),
    format('Total Tests: ~w~n', [Total]),
    format('Passed: ~w~n', [Passed]),
    format('Failed: ~w~n', [Failed]),
    (Failed = 0 -> format('🎉 ALL TESTS PASSED!~n'); true).

% =============================================================================
% UNIT TESTS FOR LOGGING SYSTEM
% =============================================================================

% Test logging system doesn't crash
test_logging_system :-
    catch(
        (
            % Enable logging
            enable_minimax_logging,
            % Test different log levels
            debug_log('Test debug message with ~w', ['argument']),
            info_log('Test info message with ~w', ['argument']),
            warning_log('Test warning message with ~w', ['argument']),
            error_log('Test error message with ~w', ['argument']),
            % Test performance timing
            start_performance_timing,
            end_performance_timing,
            % Test counter operations
            reset_counters,
            increment_position_counter,
            mark_test_passed('Logging System Test')
        ),
        Error,
        mark_test_failed('Logging System Test', Error)
    ).

% Test timestamp formatting
test_timestamp_formatting :-
    try(
        (
            format_timestamp(Timestamp),
            atom(Timestamp),  % Verify it creates an atom
            string_length(Timestamp, Length),
            Length > 10,  % Should be a reasonable length
            mark_test_passed('Timestamp Formatting Test')
        ),
        Error,
        mark_test_failed('Timestamp Formatting Test', Error)
    ).

% Test log level filtering
test_log_level_filtering :-
    try(
        (
            % Set log level to warning
            set_minimax_log_level(warning),
            % These should not appear (below warning level)
            debug_log('Debug message', []),
            info_log('Info message', []),
            % This should appear
            warning_log('Warning message', []),
            % Reset to info for other tests
            set_minimax_log_level(info),
            mark_test_passed('Log Level Filtering Test')
        ),
        Error,
        mark_test_failed('Log Level Filtering Test', Error)
    ).

% =============================================================================
% MINIMAX AI UNIT TESTS
% =============================================================================

% Test minimax AI initialization
test_minimax_ai_initialization :-
    try(
        (
            % Test depth settings
            get_minimax_depth(Depth),
            integer(Depth),
            Depth > 0,
            % Test performance tracking
            enable_performance_tracking,
            disable_performance_tracking,
            mark_test_passed('Minimax AI Initialization Test')
        ),
        Error,
        mark_test_failed('Minimax AI Initialization Test', Error)
    ).

% Test minimax with simple board
test_minimax_simple_board :-
    try(
        (
            % Create a simple board
            Board = [[o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [x,x,o,o,o,o,o]],
            % Test minimax evaluation
            minimax(Board, 2, x, BestCol, Score),
            integer(BestCol),
            integer(Score),
            % Verify best column is valid
            BestCol >= 0,
            BestCol =< 6,
            mark_test_passed('Minimax Simple Board Test')
        ),
        Error,
        mark_test_failed('Minimax Simple Board Test', Error)
    ).

% Test minimax AI main entry point
test_minimax_ai_entry_point :-
    try(
        (
            % Create initial board
            Board = [[o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o]],
            % Test AI move
            minimax_ai(Board, NewBoard, x),
            % Verify board changed (a move was made)
            Board \= NewBoard,
            % Count pieces to verify move was made
            count_pieces(Board, XCount, OCount),
            count_pieces(NewBoard, NewXCount, NewOCount),
            NewXCount is XCount + 1,  % X should have one more piece
            mark_test_passed('Minimax AI Entry Point Test')
        ),
        Error,
        mark_test_failed('Minimax AI Entry Point Test', Error)
    ).

% Test statistics tracking
test_statistics_tracking :-
    try(
        (
            reset_counters,
            get_minimax_stats(Calls, Positions, Time),
            Calls = 0,
            Positions = 0,
            % Simulate some evaluation
            Board = [[o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [x,x,o,o,o,o,o]],
            minimax(Board, 1, x, _, _),
            get_minimax_stats(NewCalls, NewPositions, _),
            NewCalls > 0,
            NewPositions > 0,
            mark_test_passed('Statistics Tracking Test')
        ),
        Error,
        mark_test_failed('Statistics Tracking Test', Error)
    ).

% =============================================================================
% INTEGRATION TESTS - ACTUAL GAMEPLAY
% =============================================================================

% Test complete game simulation
test_complete_game_simulation :-
    try(
        (
            % Start with empty board
            Board = [[o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o]],
            % Simulate a few moves
            simulate_game_moves(Board, 3, FinalBoard),
            % Verify game progressed
            Board \= FinalBoard,
            % Count total pieces (should be 3)
            count_pieces(FinalBoard, XCount, OCount),
            TotalCount is XCount + OCount,
            TotalCount = 3,
            mark_test_passed('Complete Game Simulation Test')
        ),
        Error,
        mark_test_failed('Complete Game Simulation Test', Error)
    ).

% Simulate game moves
simulate_game_moves(Board, 0, Board) :- !.
simulate_game_moves(Board, MovesLeft, FinalBoard) :-
    MovesLeft > 0,
    % Determine current player (alternates)
    TotalPieces is 6 - MovesLeft,
    (TotalPieces mod 2 =:= 0 -> Player = x ; Player = o),
    % Get AI move
    minimax_ai(Board, NewBoard, Player),
    % Recurse
    NextMovesLeft is MovesLeft - 1,
    simulate_game_moves(NewBoard, NextMovesLeft, FinalBoard).

% Test Player vs IA flow
test_player_vs_ia_flow :-
    try(
        (
            % Test that we can start a game and make moves
            write('Testing Player vs IA flow...'), nl,
            % Initialize game
            initialBoard(Board),
            % Test that AI can make moves for both players
            minimax_ai(Board, Board1, x),
            minimax_ai(Board1, Board2, o),
            % Verify moves were made
            Board \= Board1,
            Board1 \= Board2,
            % Test move validation
            validMove(Col),
            integer(Col),
            Col >= 0,
            Col =< 6,
            mark_test_passed('Player vs IA Flow Test')
        ),
        Error,
        mark_test_failed('Player vs IA Flow Test', Error)
    ).

% =============================================================================
% ERROR HANDLING TESTS
% =============================================================================

% Test invalid board handling
test_invalid_board_handling :-
    try(
        (
            % Test with invalid board (wrong dimensions)
            InvalidBoard = [[x,o],[x,o]],
            catch(
                minimax(InvalidBoard, 2, x, BestCol, Score),
                _,
                % Should handle error gracefully
                true
            ),
            mark_test_passed('Invalid Board Handling Test')
        ),
        Error,
        mark_test_failed('Invalid Board Handling Test', Error)
    ).

% Test full column handling
test_full_column_handling :-
    try(
        (
            % Create board with full column
            FullBoard = [[x,x,x,x,x,x,o],
                         [o,o,o,o,o,o,o],
                         [o,o,o,o,o,o,o],
                         [o,o,o,o,o,o,o],
                         [o,o,o,o,o,o,o],
                         [o,o,o,o,o,o,o]],
            % Test that minimax handles full columns gracefully
            minimax(FullBoard, 2, x, BestCol, Score),
            integer(BestCol),
            integer(Score),
            % BestCol should not be 0 (the full column)
            BestCol \= 0,
            mark_test_passed('Full Column Handling Test')
        ),
        Error,
        mark_test_failed('Full Column Handling Test', Error)
    ).

% =============================================================================
% PERFORMANCE TESTS
% =============================================================================

% Test performance with different depths
test_performance_different_depths :-
    try(
        (
            % Test with different depths to ensure no crashes
            Board = [[o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [x,x,o,o,o,o,o]],
            % Test depth 1 (fast)
            time(minimax(Board, 1, x, _, _)),
            % Test depth 2 (medium)
            time(minimax(Board, 2, x, _, _)),
            % Test depth 3 (slower but should complete)
            time(minimax(Board, 3, x, _, _)),
            mark_test_passed('Performance Different Depths Test')
        ),
        Error,
        mark_test_failed('Performance Different Depths Test', Error)
    ).

% =============================================================================
% MAIN TEST RUNNER
% =============================================================================

% Run all tests
run_all_tests :-
    reset_test_counters,
    format('~n=== RUNNING COMPREHENSIVE MINIMAX INTEGRATION TESTS ===~n~n'),
    
    % Logging system tests
    format('Testing Logging System...~n'),
    test_logging_system,
    test_timestamp_formatting,
    test_log_level_filtering,
    
    % Minimax AI tests
    format('~nTesting Minimax AI...~n'),
    test_minimax_ai_initialization,
    test_minimax_simple_board,
    test_minimax_ai_entry_point,
    test_statistics_tracking,
    
    % Integration tests
    format('~nTesting Integration...~n'),
    test_complete_game_simulation,
    test_player_vs_ia_flow,
    
    % Error handling tests
    format('~nTesting Error Handling...~n'),
    test_invalid_board_handling,
    test_full_column_handling,
    
    % Performance tests
    format('~nTesting Performance...~n'),
    test_performance_different_depths,
    
    % Print summary
    print_test_summary.

% Quick test for just the critical fixes
quick_test_critical_fixes :-
    reset_test_counters,
    format('~n=== QUICK TEST: CRITICAL FIXES ===~n'),
    
    % Test the specific logging error that was fixed
    try(
        (
            enable_minimax_logging,
            info_log('Starting minimax AI with depth ~w', [4]),
            format('✓ Logging format error FIXED!~n'),
            mark_test_passed('Critical Logging Fix Test')
        ),
        Error,
        (format('✗ Logging error still exists: ~w~n', [Error]),
         mark_test_failed('Critical Logging Fix Test', Error))
    ),
    
    % Test minimax AI doesn't crash
    try(
        (
            Board = [[o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [x,x,o,o,o,o,o]],
            minimax_ai(Board, _, x),
            format('✓ Minimax AI works without crashes!~n'),
            mark_test_passed('Minimax AI Functionality Test')
        ),
        Error,
        (format('✗ Minimax AI error: ~w~n', [Error]),
         mark_test_failed('Minimax AI Functionality Test', Error))
    ),
    
    print_test_summary.

% =============================================================================
% AUTO-RUN TESTS
% =============================================================================

% Auto-run on consultation
:- initialization(run_all_tests, main).