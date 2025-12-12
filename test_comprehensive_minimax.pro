% =============================================================================
% COMPREHENSIVE MINIMAX TEST SUITE WITH LOGGING AND DEBUGGING
% =============================================================================
% This enhanced test suite includes comprehensive logging, performance benchmarking,
% scenario testing, and debugging capabilities for the minimax AI implementation

:- consult('puissance4.pro').
:- consult('ai/minimax/minimax.pro').
:- consult('ai/debug/minimax_debug.pro').
:- consult('ai/game_utils.pro').
:- consult('ai/evaluation.pro').

% =============================================================================
% TEST FRAMEWORK WITH ENHANCED REPORTING
% =============================================================================

:- dynamic test_results/4.  % test_results(TestName, Category, Status, Details)
:- dynamic benchmark_results/3.  % benchmark_results(TestName, Depth, Timing)
:- dynamic scenario_results/3.  % scenario_results(Scenario, Status, Score)

% =============================================================================
% ENHANCED TEST RESULT MANAGEMENT
% =============================================================================

% Record test result with details
record_result(TestName, Category, Status, Details) :-
    assertz(test_results(TestName, Category, Status, Details)).

% Clear all test results
clear_results :-
    retractall(test_results(_,_,_,_)),
    retractall(benchmark_results(_,_,_)),
    retractall(scenario_results(_,_,_)).

% Enhanced test summary with statistics
print_enhanced_summary :-
    findall((Cat, Status), test_results(_, Cat, Status, _), Results),
    sort(Results, UniqueResults),
    
    writeln('=== ENHANCED TEST SUMMARY ==='),
    forall(member((Cat, Status), UniqueResults), (
        findall(Test, test_results(Test, Cat, Status, _), Tests),
        length(Tests, Count),
        format('~w ~w: ~d tests~n', [Cat, Status, Count])
    )),
    
    % Category breakdown
    findall(Cat, test_results(_, Cat, _, _), Categories),
    sort(Categories, UniqueCategories),
    writeln('~nCategory Breakdown:'),
    forall(member(Cat, UniqueCategories), (
        findall(Status, test_results(_, Cat, Status, _), Statuses),
        sort(Statuses, UniqueStatuses),
        format('~w: ', [Cat]),
        forall(member(Status, UniqueStatuses), (
            findall(Test, test_results(Test, Cat, Status, _), Tests),
            length(Tests, Count),
            format('~w(~d) ', [Status, Count])
        )),
        nl
    )),
    
    % Count total tests
    findall(_, test_results(_,_,_,_), AllTests),
    length(AllTests, Total),
    format('Total tests run: ~d~n', [Total]).

% =============================================================================
% LOGGING AND DEBUGGING FUNCTIONALITY TESTS
% =============================================================================

% Test logging configuration
test_logging_configuration :-
    % Test enabling/disabling logging
    assert_test('logging_enable_disable', 'logging',
        (disable_minimax_logging,
         minimax_logging_enabled(false),
         enable_minimax_logging,
         minimax_logging_enabled(true)),
        'Logging can be enabled and disabled'),
    
    % Test debug mode configuration
    assert_test('debug_mode_toggle', 'logging',
        (disable_minimax_debug,
         minimax_debug_mode(false),
         enable_minimax_debug,
         minimax_debug_mode(true)),
        'Debug mode can be toggled'),
    
    % Test log level setting
    assert_test('log_level_setting', 'logging',
        (set_minimax_log_level(debug),
         set_minimax_log_level(info),
         set_minimax_log_level(warning)),
        'Log levels can be set correctly'),
    
    % Test invalid log level
    assert_test('invalid_log_level', 'logging',
        \+ set_minimax_log_level(invalid_level),
        'Invalid log levels are rejected').

% Test performance tracking
test_performance_tracking :-
    reset_counters,
    
    % Test counter initialization
    assert_test('counter_initialization', 'performance',
        (minimax_calls(0), minimax_positions_evaluated(0)),
        'Performance counters initialize correctly'),
    
    % Test counter updates during minimax
    assert_test('counter_updates', 'performance',
        (board(Board),
         minimax(Board, 1, 'x', _, _),
         minimax_calls(Calls),
         Calls > 0),
        'Counters update during minimax execution'),
    
    % Test statistics retrieval
    assert_test('statistics_retrieval', 'performance',
        get_minimax_stats(_, _, _),
        'Statistics can be retrieved successfully').

% Test debug visualization
test_debug_visualization :-
    TestBoard = [['x',0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0]],
    
    % Test board visualization doesn't crash
    assert_test('board_visualization', 'debug',
        catch(visualize_board_state(TestBoard, 'x', 2, 0, 100), _, fail),
        'Board visualization works without errors'),
    
    % Test move analysis doesn't crash
    assert_test('move_analysis', 'debug',
        catch(analyze_move_quality(TestBoard, 0, TestBoard, 'x', 100), _, fail),
        'Move analysis works without errors').

% =============================================================================
% SCENARIO-BASED TESTS
% =============================================================================

% Test winning move detection
test_winning_move_scenario :-
    setup,
    
    % Create board with immediate winning move
    WinBoard = [['x','x','x',0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0]],
    
    assert_test('winning_move_detection', 'scenario',
        (minimax(WinBoard, 2, 'x', BestCol, Score),
         Score > 9000,
         BestCol = 3),
        'Minimax detects winning moves correctly'),
    
    % Test with debug visualization
    assert_test('winning_move_debug', 'scenario',
        (enable_minimax_debug,
         debug_ai_thinking(WinBoard, 'x', 2),
         disable_minimax_debug),
        'Winning move debug visualization works'),
    
    test_teardown.

% Test blocking move scenario
test_blocking_move_scenario :-
    setup,
    
    % Create board where blocking is required
    BlockBoard = [['o','o','o',0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0]],
    
    assert_test('blocking_move_detection', 'scenario',
        (minimax(BlockBoard, 2, 'x', BestCol, Score),
         Score > 5000,
         BestCol = 3),
        'Minimax blocks opponent winning moves'),
    
    test_teardown.

% Test endgame scenario
test_endgame_scenario :-
    setup,
    
    % Create endgame position with limited moves
    EndgameBoard = [[0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,'x','x','x',0,0],
                    ['o','o','x','o','o','x','o']],
    
    assert_test('endgame_handling', 'scenario',
        (minimax(EndgameBoard, 4, 'x', BestCol, Score),
         between(0, 6, BestCol)),
        'Minimax handles endgame positions correctly'),
    
    % Test with deeper search in endgame
    assert_test('endgame_deep_search', 'scenario',
        (minimax(EndgameBoard, 6, 'x', BestColDeeper, ScoreDeeper),
         between(0, 6, BestColDeeper)),
        'Minimax can search deeper in endgame'),
    
    test_teardown.

% Test complex positional scenario
test_complex_positional_scenario :-
    setup,
    
    % Create complex positional board
    ComplexBoard = [[0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,'x',0,'o',0,0],
                    [0,0,0,'x','o',0,0],
                    [0,0,'x','o','x','o',0],
                    [0,'o','x','o','x','o','x']],
    
    assert_test('complex_positioning', 'scenario',
        (minimax(ComplexBoard, 3, 'x', BestCol, Score),
         between(0, 6, BestCol)),
        'Minimax handles complex positional play'),
    
    % Test detailed analysis
    assert_test('complex_position_debug', 'scenario',
        (enable_minimax_debug,
         debug_ai_thinking(ComplexBoard, 'x', 3),
         disable_minimax_debug),
        'Complex position debug analysis works'),
    
    test_teardown.

% Test multiple threat scenario
test_multiple_threats_scenario :-
    setup,
    
    % Create board with multiple threats
    ThreatBoard = [['x','x',0,0,0,0,0],
                   ['o','o',0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0]],
    
    assert_test('multiple_threats', 'scenario',
        (minimax(ThreatBoard, 3, 'x', BestCol, Score),
         between(0, 6, BestCol)),
        'Minimax handles multiple threats correctly'),
    
    test_teardown.

% =============================================================================
% PERFORMANCE BENCHMARKING TESTS
% =============================================================================

% Benchmark different search depths
benchmark_depth_performance :-
    board(Board),
    Depths = [1, 2, 3, 4, 5],
    
    writeln('=== DEPTH PERFORMANCE BENCHMARK ==='),
    
    forall(member(Depth, Depths), (
        format('~nBenchmarking depth ~w...', [Depth]),
        
        % Run multiple iterations for average
        findall(Time, (
            between(1, 3, _),
            reset_counters,
            get_time(Start),
            minimax(Board, Depth, 'x', _, _),
            get_time(End),
            Time is End - Start
        ), Times),
        
        % Calculate average
        sum_list(Times, Sum),
        length(Times, Count),
        AvgTime is Sum / Count,
        
        % Store result
        assertz(benchmark_results(depth_benchmark, Depth, AvgTime)),
        
        format(' Average time: ~6f seconds', [AvgTime])
    )).

% Benchmark different board complexities
benchmark_board_complexity :-
    writeln('~n=== BOARD COMPLEXITY BENCHMARK ==='),
    
    % Empty board
    board(EmptyBoard),
    benchmark_board(EmptyBoard, 'empty', 3),
    
    % Quarter full board
    create_quarter_board(QuarterBoard),
    benchmark_board(QuarterBoard, 'quarter_full', 3),
    
    % Half full board
    create_half_board(HalfBoard),
    benchmark_board(HalfBoard, 'half_full', 3),
    
    % Nearly full board
    create_nearly_full_board(NearlyFullBoard),
    benchmark_board(NearlyFullBoard, 'nearly_full', 3).

% Helper to benchmark a specific board
benchmark_board(Board, BoardType, Depth) :-
    format('Benchmarking ~w board...', [BoardType]),
    
    % Run timing test
    reset_counters,
    get_time(Start),
    minimax(Board, Depth, 'x', _, _),
    get_time(End),
    Elapsed is End - Start,
    
    % Get statistics
    minimax_calls(Calls),
    minimax_positions_evaluated(Positions),
    
    % Store results
    assertz(benchmark_results(board_complexity, BoardType, Elapsed)),
    
    format(' Time: ~6f seconds, Calls: ~w, Positions: ~w~n', 
           [Elapsed, Calls, Positions]).

% Helper to create test boards
create_quarter_board(Board) :-
    Board = [[0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             ['x','o','x','o','x','o',0],
             ['o','x','o','x','o','x',0]].

create_half_board(Board) :-
    Board = [[0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             ['x','o','x','o','x','o',0],
             ['o','x','o','x','o','x',0],
             ['x','o','x','o','x','o',0],
             ['o','x','o','x','o','x',0]].

create_nearly_full_board(Board) :-
    Board = [[1,2,1,2,1,2,1],
             [2,1,2,1,2,1,2],
             [1,2,1,2,1,2,1],
             [2,1,2,1,2,1,2],
             [1,2,1,2,1,2,1],
             [2,1,2,1,2,1,0]].

% =============================================================================
% STRESS TESTING
% =============================================================================

% Stress test with maximum depth
test_max_depth_stress :-
    setup,
    
    % Test with very deep search
    board(Board),
    
    assert_test('max_depth_stress', 'stress',
        catch(minimax(Board, 8, 'x', BestCol, Score), _, fail),
        'Minimax handles maximum depth without crashing'),
    
    % Test with complex board at high depth
    ComplexBoard = [[0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,'x',0,'o',0,0],
                    [0,0,0,'x','o',0,0],
                    [0,0,'x','o','x','o',0],
                    [0,'o','x','o','x','o','x']],
    
    assert_test('complex_max_depth', 'stress',
        catch(minimax(ComplexBoard, 6, 'x', BestCol, Score), _, fail),
        'Complex positions handled at high depth'),
    
    test_teardown.

% Stress test with rapid consecutive calls
test_rapid_succession_stress :-
    setup,
    
    % Test multiple rapid calls
    board(Board),
    
    assert_test('rapid_succession', 'stress',
        forall(between(1, 10, _),
               minimax(Board, 3, 'x', _, _)),
        'Multiple rapid minimax calls handled correctly'),
    
    test_teardown.

% Stress test with memory pressure
test_memory_pressure_stress :-
    setup,
    
    % Test with maximum valid moves
    board(EmptyBoard),
    
    assert_test('memory_pressure', 'stress',
        (findall(Col, validMove(Col), ValidMoves),
         length(ValidMoves, 7),
         forall(member(Col, ValidMoves),
                minimax(EmptyBoard, 4, 'x', _, _))),
        'Memory pressure with maximum moves handled'),
    
    test_teardown.

% =============================================================================
% EDGE CASE TESTING WITH LOGGING
% =============================================================================

% Test edge cases with debug enabled
test_edge_cases_with_debug :-
    setup,
    enable_minimax_debug,
    
    % Test empty board with debug
    assert_test('empty_board_debug', 'edge_case',
        (board(Board),
         debug_ai_thinking(Board, 'x', 2),
         minimax(Board, 2, 'x', BestCol, Score),
         between(0, 6, BestCol)),
        'Empty board handled with debug enabled'),
    
    % Test full board with debug
    assert_test('full_board_debug', 'edge_case',
        (FullBoard = [[1,2,1,2,1,2,1],
                      [2,1,2,1,2,1,2],
                      [1,2,1,2,1,2,1],
                      [2,1,2,1,2,1,2],
                      [1,2,1,2,1,2,1],
                      [2,1,2,1,2,1,2]],
         catch(debug_ai_thinking(FullBoard, 'x', 2), _, true)),
        'Full board handled gracefully with debug'),
    
    % Test invalid depth with debug
    assert_test('invalid_depth_debug', 'edge_case',
        (board(Board),
         minimax(Board, -1, 'x', BestCol, Score),
         Score = 0,
         BestCol = -1),
        'Invalid depth handled correctly with debug'),
    
    disable_minimax_debug,
    test_teardown.

% =============================================================================
% MOVE QUALITY ANALYSIS TESTS
% =============================================================================

% Test move quality analysis
test_move_quality_analysis :-
    setup,
    
    % Test winning move quality
    WinBoard = [['x','x','x',0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0]],
    
    assert_test('move_quality_winning', 'quality',
        (minimax(WinBoard, 2, 'x', BestCol, Score),
         analyze_move_quality_detailed(WinBoard, BestCol, 'x'),
         Score > 9000),
        'Move quality analysis for winning moves'),
    
    % Test blocking move quality
    BlockBoard = [['o','o','o',0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0]],
    
    assert_test('move_quality_blocking', 'quality',
        (minimax(BlockBoard, 2, 'x', BestCol, Score),
         analyze_move_quality_detailed(BlockBoard, BestCol, 'x'),
         Score > 5000),
        'Move quality analysis for blocking moves'),
    
    test_teardown.

% Test board annotation
test_board_annotation :-
    setup,
    
    TestBoard = [['x',0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0]],
    
    assert_test('board_annotation', 'quality',
        catch(show_annotated_board(TestBoard, 'x'), _, fail),
        'Board annotation works without errors'),
    
    test_teardown.

% =============================================================================
% COMPREHENSIVE TEST RUNNERS
% =============================================================================

% Run all logging and debugging tests
run_logging_debugging_tests :-
    writeln('=== RUNNING LOGGING & DEBUGGING TESTS ==='),
    test_logging_configuration,
    test_performance_tracking,
    test_debug_visualization.

% Run all scenario tests
run_scenario_tests :-
    writeln('=== RUNNING SCENARIO TESTS ==='),
    test_winning_move_scenario,
    test_blocking_move_scenario,
    test_endgame_scenario,
    test_complex_positional_scenario,
    test_multiple_threats_scenario.

% Run all benchmark tests
run_benchmark_tests :-
    writeln('=== RUNNING BENCHMARK TESTS ==='),
    benchmark_depth_performance,
    benchmark_board_complexity.

% Run all stress tests
run_stress_tests :-
    writeln('=== RUNNING STRESS TESTS ==='),
    test_max_depth_stress,
    test_rapid_succession_stress,
    test_memory_pressure_stress.

% Run all edge case tests
run_edge_case_tests :-
    writeln('=== RUNNING EDGE CASE TESTS ==='),
    test_edge_cases_with_debug.

% Run all quality analysis tests
run_quality_tests :-
    writeln('=== RUNNING QUALITY ANALYSIS TESTS ==='),
    test_move_quality_analysis,
    test_board_annotation.

% Run comprehensive test suite
run_comprehensive_tests :-
    clear_results,
    writeln('=== RUNNING COMPREHENSIVE MINIMAX TEST SUITE ==='),
    
    run_logging_debugging_tests,
    run_scenario_tests,
    run_benchmark_tests,
    run_stress_tests,
    run_edge_case_tests,
    run_quality_tests,
    
    print_enhanced_summary,
    
    % Print benchmark results
    print_benchmark_summary.

% Print benchmark summary
print_benchmark_summary :-
    findall((Test, Depth, Time), benchmark_results(Test, Depth, Time), Results),
    (Results = [] -> writeln('No benchmark results to display')
    ;   (
        writeln('~n=== BENCHMARK SUMMARY ==='),
        forall(member((Test, Depth, Time), Results), (
            format('~w depth ~w: ~6f seconds~n', [Test, Depth, Time])
        ))
    )).

% Quick comprehensive test for development
quick_comprehensive_test :-
    clear_results,
    writeln('=== QUICK COMPREHENSIVE TEST ==='),
    
    % Core functionality
    test_logging_configuration,
    test_performance_tracking,
    test_winning_move_scenario,
    test_blocking_move_scenario,
    
    % Basic benchmarks
    benchmark_depth_performance,
    
    print_enhanced_summary.

% =============================================================================
% COMPATIBILITY AND BACKWARD COMPATIBILITY TESTS
% =============================================================================

% Test backward compatibility
test_backward_compatibility :-
    setup,
    
    % Test that original API still works
    assert_test('original_api_compatibility', 'compatibility',
        (board(Board),
         minimax_original(Board, 2, 'x', BestCol, Score),
         between(0, 6, BestCol)),
        'Original minimax API still works'),
    
    % Test simple AI interface
    assert_test('simple_ai_compatibility', 'compatibility',
        (board(Board),
         minimax_ai_simple(Board, NewBoard, 'x'),
         Board \= NewBoard),
        'Simple AI interface works'),
    
    test_teardown.

% Run compatibility tests
run_compatibility_tests :-
    writeln('=== RUNNING COMPATIBILITY TESTS ==='),
    test_backward_compatibility.

% =============================================================================
% EXAMPLE DEMONSTRATIONS
% =============================================================================

% Demonstrate debugging capabilities
demonstrate_debugging_capabilities :-
    writeln('=== DEMONSTRATING DEBUGGING CAPABILITIES ==='),
    
    % Example 1: Winning move
    example_debug_winning,
    
    % Example 2: Blocking move
    example_debug_blocking,
    
    % Example 3: Performance comparison
    example_debug_performance.

% Demonstrate testing capabilities
demonstrate_testing_capabilities :-
    writeln('=== DEMONSTRATING TESTING CAPABILITIES ==='),
    
    quick_comprehensive_test,
    
    % Show specific examples
    writeln('~n--- Specific Test Examples ---'),
    test_move_quality_analysis,
    test_edge_cases_with_debug.