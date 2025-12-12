% Simple Direct Test for Critical Minimax Fixes
% Tests the specific logging format errors that were fixed

:- consult('ai/minimax/minimax.pro').
:- consult('ai/evaluation.pro').
:- consult('ai/game_utils.pro').
:- consult('ai/selector.pro').
:- consult('puissance4.pro').

% =============================================================================
% CRITICAL TESTS - LOGGING FIXES
% =============================================================================

% Test the specific logging error that was reported
test_critical_logging_fix :-
    format('~n=== TESTING CRITICAL LOGGING FIX ===~n'),
    
    % Enable logging
    enable_minimax_logging,
    
    % Test the exact call that was failing: info_log with format arguments
    % This should NOT produce: ERROR: Arguments are not sufficiently instantiated
    catch(
        info_log('Starting minimax AI with depth ~w', [4]),
        Error,
        (format('CRITICAL: Logging still fails with error: ~w~n', [Error]), fail)
    ),
    
    format('SUCCESS: info_log with format arguments works!~n'),
    
    % Test other logging calls
    catch(
        (debug_log('Debug test', []),
         warning_log('Warning test', []),
         error_log('Error test', [])),
        Error2,
        (format('Other logging fails: ~w~n', [Error2]), fail)
    ),
    
    format('SUCCESS: All logging calls work!~n').

% Test minimax AI functionality
test_minimax_ai_functionality :-
    format('~n=== TESTING MINIMAX AI FUNCTIONALITY ===~n'),
    
    % Create test board
    TestBoard = [[o,o,o,o,o,o,o],
                 [o,o,o,o,o,o,o],
                 [o,o,o,o,o,o,o],
                 [o,o,o,o,o,o,o],
                 [o,o,o,o,o,o,o],
                 [x,x,o,o,o,o,o]],
    
    % Test minimax_ai (the main entry point)
    catch(
        minimax_ai(TestBoard, NewBoard, x),
        Error,
        (format('CRITICAL: minimax_ai fails: ~w~n', [Error]), fail)
    ),
    
    % Verify a move was made
    (TestBoard \= NewBoard ->
        format('SUCCESS: minimax_ai made a move!~n')
    ;   format('WARNING: minimax_ai did not change board~n')),
    
    % Test statistics
    catch(
        get_minimax_stats(Calls, Positions, Time),
        Error3,
        (format('Statistics tracking fails: ~w~n', [Error3]), fail)
    ),
    
    format('SUCCESS: Statistics tracking works!~n').

% Test game flow simulation
test_game_flow_simulation :-
    format('~n=== TESTING GAME FLOW SIMULATION ===~n'),
    
    % Start with empty board
    InitialBoard = [[o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o]],
    
    % Simulate a few AI moves
    catch(
        (
            % First move for player x
            minimax_ai(InitialBoard, Board1, x),
            % Second move for player o
            minimax_ai(Board1, Board2, o),
            % Third move for player x
            minimax_ai(Board2, FinalBoard, x),
            
            format('SUCCESS: Game flow simulation works!~n'),
            format('  Initial board changed: ~w~n', [InitialBoard \= FinalBoard])
        ),
        Error,
        (format('Game flow simulation fails: ~w~n', [Error]), fail)
    ).

% Test different logging levels
test_logging_levels :-
    format('~n=== TESTING LOGGING LEVELS ===~n'),
    
    % Test each log level
    catch(
        (
            set_minimax_log_level(debug),
            debug_log('Debug level test', []),
            
            set_minimax_log_level(info),
            info_log('Info level test', []),
            
            set_minimax_log_level(warning),
            warning_log('Warning level test', []),
            
            set_minimax_log_level(error),
            error_log('Error level test', []),
            
            % Reset to info for other tests
            set_minimax_log_level(info),
            
            format('SUCCESS: All logging levels work!~n')
        ),
        Error,
        (format('Logging levels fail: ~w~n', [Error]), fail)
    ).

% Test performance timing
test_performance_timing :-
    format('~n=== TESTING PERFORMANCE TIMING ===~n'),
    
    catch(
        (
            enable_performance_tracking,
            start_performance_timing,
            % Simulate some work
            Board = [[o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [o,o,o,o,o,o,o],
                     [x,x,o,o,o,o,o]],
            minimax(Board, 2, x, _, _),
            end_performance_timing,
            
            format('SUCCESS: Performance timing works!~n')
        ),
        Error,
        (format('Performance timing fails: ~w~n', [Error]), fail)
    ).

% =============================================================================
% MAIN TEST RUNNER
% =============================================================================

run_critical_tests :-
    format('~n===========================================~n'),
    format('   MINIMAX CRITICAL FIXES VERIFICATION~n'),
    format('===========================================~n'),
    
    % Run all critical tests
    (test_critical_logging_fix -> format('Logging Fix: PASSED~n'); format('Logging Fix: FAILED~n')),
    format('~n'),
    (test_minimax_ai_functionality -> format('AI Functionality: PASSED~n'); format('AI Functionality: FAILED~n')),
    format('~n'),
    (test_game_flow_simulation -> format('Game Flow: PASSED~n'); format('Game Flow: FAILED~n')),
    format('~n'),
    (test_logging_levels -> format('Logging Levels: PASSED~n'); format('Logging Levels: FAILED~n')),
    format('~n'),
    (test_performance_timing -> format('Performance Timing: PASSED~n'; format('Performance Timing: FAILED~n')),
    
    format('~n===========================================~n'),
    format('   TESTING COMPLETE~n'),
    format('===========================================~n~n').

% Quick test specifically for the original error
test_original_error_fixed :-
    format('~n=== TESTING ORIGINAL ERROR IS FIXED ===~n'),
    format('Original error was:~n'),
    format('  ERROR: Arguments are not sufficiently instantiated~n'),
    format('  In: format(_838,Starting minimax AI with depth ~w,[4])~n~n'),
    
    % Test the exact scenario
    catch(
        (
            enable_minimax_logging,
            % This was the failing call
            info_log('Starting minimax AI with depth ~w', [4]),
            format('ORIGINAL ERROR IS FIXED!~n'),
            format('  The info_log call now works without instantiation errors.~n')
        ),
        OriginalError,
        (format('ORIGINAL ERROR STILL EXISTS: ~w~n', [OriginalError]), fail)
    ).

% Run all tests
:- initialization(run_critical_tests, main).