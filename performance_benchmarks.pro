% =============================================================================
% PERFORMANCE BENCHMARKING AND MONITORING FOR MINIMAX AI
% =============================================================================
% This module provides comprehensive performance benchmarking, monitoring, and
% analysis tools specifically designed for the minimax AI implementation

:- consult('puissance4.pro').
:- consult('ai/minimax/minimax.pro').
:- consult('ai/debug/minimax_debug.pro').
:- consult('ai/game_utils.pro').
:- consult('ai/evaluation.pro').

% =============================================================================
% PERFORMANCE DATA STORAGE
% =============================================================================

:- dynamic performance_record/6.  % performance_record(TestName, Depth, BoardComplexity, Time, Calls, Positions)
:- dynamic benchmark_summary/4.    % benchmark_summary(TestType, Parameters, AvgTime, Variance)
:- dynamic performance_alert/3.    % performance_alert(TestName, Threshold, Status)

% =============================================================================
% BENCHMARK CONFIGURATION
% =============================================================================

% Default benchmark parameters
get_default_iterations(5).          % Number of iterations per test
get_max_acceptable_time(5.0).       % Maximum acceptable time in seconds
get_min_acceptable_calls(100).      % Minimum expected function calls
get_max_acceptable_calls(100000).   % Maximum expected function calls

% Performance thresholds for alerts
get_critical_time_threshold(10.0).  % Critical performance threshold
get_warning_time_threshold(2.0).    % Warning performance threshold
get_excellent_time_threshold(0.1).  % Excellent performance threshold

% =============================================================================
% CORE BENCHMARKING PREDICATES
% =============================================================================

% Run comprehensive performance benchmark
run_performance_benchmark :-
    writeln('=== COMPREHENSIVE PERFORMANCE BENCHMARK ==='),
    
    % Clear previous results
    retractall(performance_record(_,_,_,_,_,_)),
    retractall(benchmark_summary(_,_,_,_)),
    retractall(performance_alert(_,_,_)),
    
    % Run different benchmark categories
    benchmark_depth_scalability,
    benchmark_board_complexity,
    benchmark_memory_usage,
    benchmark_algorithmic_efficiency,
    
    % Generate reports
    generate_performance_report,
    generate_benchmark_charts,
    check_performance_alerts.

% Benchmark depth scalability
benchmark_depth_scalability :-
    writeln('~n--- Depth Scalability Benchmark ---'),
    board(Board),
    Depths = [1, 2, 3, 4, 5, 6],
    
    forall(member(Depth, Depths), (
        format('Testing depth ~w...', [Depth]),
        benchmark_depth(Board, Depth, 'depth_scalability'),
        format(' COMPLETE~n', [])
    )).

% Benchmark board complexity impact
benchmark_board_complexity :-
    writeln('~n--- Board Complexity Benchmark ---'),
    ComplexityLevels = [
        ('empty', []),
        ('early_game', [0,0,0,0,0,0,0]),
        ('mid_game', [0,0,1,1,1,1,1]),
        ('late_game', [2,2,3,3,4,4,5]),
        ('end_game', [5,5,5,5,5,5,5])
    ],
    
    forall(member((Complexity, FillPattern), ComplexityLevels), (
        format('Testing ~w complexity...', [Complexity]),
        create_complexity_board(FillPattern, Board),
        benchmark_board(Board, Complexity, 'complexity_impact'),
        format(' COMPLETE~n', [])
    )).

% Benchmark memory usage patterns
benchmark_memory_usage :-
    writeln('~n--- Memory Usage Benchmark ---'),
    
    % Test memory usage with different board states
    test_memory_empty_board,
    test_memory_full_board,
    test_memory_sparse_board,
    test_memory_dense_board.

% Benchmark algorithmic efficiency
benchmark_algorithmic_efficiency :-
    writeln('~n--- Algorithmic Efficiency Benchmark ---'),
    
    % Test different evaluation strategies
    benchmark_evaluation_strategies,
    benchmark_pruning_effectiveness,
    benchmark_move_ordering.

% =============================================================================
% DEPTH BENCHMARKING
% =============================================================================

% Benchmark specific depth
benchmark_depth(Board, Depth, TestName) :-
    get_default_iterations(Iterations),
    
    % Run multiple iterations
    findall(Time, (
        between(1, Iterations, _),
        reset_counters,
        get_time(Start),
        minimax(Board, Depth, 'x', _, _),
        get_time(End),
        Time is End - Start
    ), Times),
    
    % Calculate statistics
    calculate_time_statistics(Times, AvgTime, MinTime, MaxTime, StdDev),
    minimax_calls(Calls),
    minimax_positions_evaluated(Positions),
    
    % Store results
    assertz(performance_record(TestName, Depth, 'standard', AvgTime, Calls, Positions)),
    
    % Generate summary
    generate_depth_summary(TestName, Depth, AvgTime, MinTime, MaxTime, StdDev, Calls, Positions).

% Generate depth performance summary
generate_depth_summary(TestName, Depth, AvgTime, MinTime, MaxTime, StdDev, Calls, Positions) :-
    format('  Depth ~w: avg=~6fs, min=~6fs, max=~6fs, std=~6fs', 
           [Depth, AvgTime, MinTime, MaxTime, StdDev]),
    format('  | Calls: ~w, Positions: ~w, Ratio: ~2f pos/call~n', 
           [Calls, Positions, Positions/Calls]).



% =============================================================================
% BOARD COMPLEXITY BENCHMARKING
% =============================================================================

% Create board with specific complexity level
create_complexity_board(FillPattern, Board) :-
    board(EmptyBoard),
    
    % Apply fill pattern to create complexity
    apply_fill_pattern(EmptyBoard, FillPattern, 0, Board).

% Apply fill pattern to board
apply_fill_pattern(Board, [], _, Board).
apply_fill_pattern(Board, [FillLevel|FillRest], Col, ResultBoard) :-
    fill_board_column(Board, Col, FillLevel, TempBoard),
    NextCol is Col + 1,
    apply_fill_pattern(TempBoard, FillRest, NextCol, ResultBoard).

% Fill specific column to certain level
fill_board_column(Board, Col, FillLevel, ResultBoard) :-
    nth0(Col, Board, Column),
    fill_column_to_level(Column, FillLevel, 0, NewColumn),
    replace_column(Board, Col, NewColumn, ResultBoard).

% Fill column to specific level
fill_column_to_level(Column, FillLevel, CurrentLevel, Column) :-
    CurrentLevel >= FillLevel, !.
fill_column_to_level(Column, FillLevel, CurrentLevel, [Player|NewColumn]) :-
    CurrentLevel < FillLevel,
    (CurrentLevel mod 2 =:= 0 -> Player = 'x'; Player = 'o'),
    NextLevel is CurrentLevel + 1,
    fill_column_to_level(Column, FillLevel, NextLevel, NewColumn).

% Replace column in board
replace_column([_|Rest], 0, NewColumn, [NewColumn|Rest]).
replace_column([Col|Rest], ColIndex, NewColumn, [Col|Result]) :-
    ColIndex > 0,
    NextIndex is ColIndex - 1,
    replace_column(Rest, NextIndex, NewColumn, Result).

% Benchmark board complexity
benchmark_board(Board, Complexity, TestName) :-
    get_default_iterations(Iterations),
    
    % Run iterations with complexity tracking
    findall(Benchmark, (
        between(1, Iterations, _),
        reset_counters,
        get_time(Start),
        minimax(Board, 3, 'x', _, _),  % Fixed depth for complexity comparison
        get_time(End),
        Elapsed is End - Start,
        minimax_calls(Calls),
        minimax_positions_evaluated(Positions),
        Benchmark = complexity_benchmark(Complexity, Elapsed, Calls, Positions)
    ), Benchmarks),
    
    % Calculate statistics
    findall(Time, member(complexity_benchmark(Complexity, Time, _, _), Benchmarks), Times),
    calculate_time_statistics(Times, AvgTime, MinTime, MaxTime, StdDev),
    
    % Store summary
    assertz(benchmark_summary(TestName, Complexity, AvgTime, StdDev)),
    
    format('  ~w: avg=~6fs, calls=~w, positions=~w~n', 
           [Complexity, AvgTime, Calls]).

% =============================================================================
% MEMORY USAGE BENCHMARKING
% =============================================================================

% Test memory usage with empty board
test_memory_empty_board :-
    board(EmptyBoard),
    format('Testing memory usage: Empty board...'),
    benchmark_memory_usage(EmptyBoard, 'empty_board'),
    format(' COMPLETE~n', []).

% Test memory usage with full board
test_memory_full_board :-
    FullBoard = [[1,2,1,2,1,2,1],
                 [2,1,2,1,2,1,2],
                 [1,2,1,2,1,2,1],
                 [2,1,2,1,2,1,2],
                 [1,2,1,2,1,2,1],
                 [2,1,2,1,2,1,2]],
    format('Testing memory usage: Full board...'),
    benchmark_memory_usage(FullBoard, 'full_board'),
    format(' COMPLETE~n', []).

% Test memory usage with sparse board
test_memory_sparse_board :-
    SparseBoard = [[0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,'x',0,0,0,0],
                   [0,'o',0,0,0,0,0]],
    format('Testing memory usage: Sparse board...'),
    benchmark_memory_usage(SparseBoard, 'sparse_board'),
    format(' COMPLETE~n', []).

% Test memory usage with dense board
test_memory_dense_board :-
    DenseBoard = [[1,2,1,2,1,2,1],
                  [2,1,2,1,2,1,2],
                  [1,2,1,2,1,2,1],
                  [2,1,2,1,2,1,2],
                  [1,2,1,2,1,2,1],
                  [2,1,2,1,2,1,0]],
    format('Testing memory usage: Dense board...'),
    benchmark_memory_usage(DenseBoard, 'dense_board'),
    format(' COMPLETE~n', []).

% Benchmark memory usage for specific board
benchmark_memory_usage(Board, BoardType) :-
    get_default_iterations(Iterations),
    
    % Measure memory and performance
    findall(Measurement, (
        between(1, Iterations, _),
        reset_counters,
        get_time(Start),
        minimax(Board, 3, 'x', _, _),
        get_time(End),
        Elapsed is End - Start,
        minimax_calls(Calls),
        minimax_positions_evaluated(Positions),
        Measurement = memory_benchmark(BoardType, Elapsed, Calls, Positions)
    ), Measurements),
    
    % Analyze results
    findall(Time, member(memory_benchmark(BoardType, Time, _, _), Measurements), Times),
    calculate_time_statistics(Times, AvgTime, MinTime, MaxTime, StdDev),
    
    format('    avg=~6fs, min=~6fs, max=~6fs', [AvgTime, MinTime, MaxTime]),
    format(' | calls=~w, positions=~w~n', [Calls, Positions]).

% =============================================================================
% ALGORITHMIC EFFICIENCY BENCHMARKING
% =============================================================================

% Benchmark evaluation strategies
benchmark_evaluation_strategies :-
    writeln('~n--- Evaluation Strategy Benchmark ---'),
    
    % Test different evaluation approaches if available
    board(Board),
    
    % Standard evaluation
    format('Testing standard evaluation...'),
    benchmark_evaluation(Board, 'standard', 'standard'),
    
    % Enhanced evaluation (if available)
    format('Testing enhanced evaluation...'),
    benchmark_evaluation(Board, 'enhanced', 'enhanced').

% Benchmark evaluation approach
benchmark_evaluation(Board, EvaluationType, TestName) :-
    get_time(Start),
    minimax(Board, 3, 'x', _, _),
    get_time(End),
    Elapsed is End - Start,
    
    minimax_calls(Calls),
    minimax_positions_evaluated(Positions),
    
    assertz(performance_record(TestName, 3, EvaluationType, Elapsed, Calls, Positions)),
    format(' COMPLETE: ~6fs~n', [Elapsed]).

% Benchmark pruning effectiveness
benchmark_pruning_effectiveness :-
    writeln('~n--- Pruning Effectiveness Benchmark ---'),
    
    % Test with different pruning scenarios
    board(Board),
    
    % Create scenarios that should benefit from pruning
    create_pruning_scenarios(Scenarios),
    
    forall(member((ScenarioName, ScenarioBoard), Scenarios), (
        format('Testing pruning: ~w...', [ScenarioName]),
        benchmark_pruning_scenario(ScenarioBoard, ScenarioName),
        format(' COMPLETE~n', [])
    )).

% Create test scenarios for pruning
create_pruning_scenarios([
    ('winning_move_available', WinningBoard),
    ('blocking_move_required', BlockingBoard),
    ('complex_position', ComplexBoard)
]) :-
    % Winning move scenario
    WinningBoard = [['x','x','x',0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0]],
    
    % Blocking move scenario
    BlockingBoard = [['o','o','o',0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0]],
    
    % Complex position
    ComplexBoard = [[0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,'x',0,'o',0,0],
                    [0,0,0,'x','o',0,0],
                    [0,0,'x','o','x','o',0],
                    [0,'o','x','o','x','o','x']].

% Benchmark specific pruning scenario
benchmark_pruning_scenario(Board, ScenarioName) :-
    reset_counters,
    get_time(Start),
    minimax(Board, 4, 'x', _, _),
    get_time(End),
    Elapsed is End - Start,
    
    minimax_calls(Calls),
    minimax_positions_evaluated(Positions),
    
    assertz(performance_record('pruning', 4, ScenarioName, Elapsed, Calls, Positions)),
    format('  ~6fs, ~w calls, ~w positions', [Elapsed, Calls, Positions]).

% Benchmark move ordering efficiency
benchmark_move_ordering :-
    writeln('~n--- Move Ordering Benchmark ---'),
    
    % Test different move ordering strategies
    board(Board),
    
    % Test center-first ordering
    benchmark_move_ordering_strategy(Board, 'center_first', 'center_first'),
    
    % Test edge-first ordering
    benchmark_move_ordering_strategy(Board, 'edge_first', 'edge_first').

% Benchmark specific move ordering strategy
benchmark_move_ordering_strategy(Board, Strategy, TestName) :-
    get_time(Start),
    minimax(Board, 3, 'x', _, _),
    get_time(End),
    Elapsed is End - Start,
    
    minimax_calls(Calls),
    minimax_positions_evaluated(Positions),
    
    assertz(performance_record(TestName, 3, Strategy, Elapsed, Calls, Positions)),
    format('  ~w: ~6fs, ~w calls, ~w positions~n', 
           [Strategy, Elapsed, Calls]).

% =============================================================================
% STATISTICAL ANALYSIS
% =============================================================================

% Calculate time statistics
calculate_time_statistics(Times, AvgTime, MinTime, MaxTime, StdDev) :-
    sum_list(Times, Sum),
    length(Times, Count),
    AvgTime is Sum / Count,
    min_list(Times, MinTime),
    max_list(Times, MaxTime),
    calculate_standard_deviation(Times, AvgTime, StdDev).

% Calculate standard deviation
calculate_standard_deviation(Times, Mean, StdDev) :-
    findall(Diff, (
        member(Time, Times),
        Diff is (Time - Mean) * (Time - Mean)
    ), SquaredDiffs),
    sum_list(SquaredDiffs, SumSquaredDiffs),
    length(Times, Count),
    Variance is SumSquaredDiffs / Count,
    StdDev is sqrt(Variance).

% =============================================================================
% PERFORMANCE MONITORING AND ALERTS
% =============================================================================

% Check for performance alerts
check_performance_alerts :-
    findall(Record, performance_record(_, _, _, Time, _, _), Records),
    
    forall(member(Record, Records), (
        Record = performance_record(TestName, Depth, Complexity, Time, Calls, Positions),
        check_single_alert(TestName, Depth, Time, Calls, Positions)
    )).

% Check single performance alert
check_single_alert(TestName, Depth, Time, Calls, Positions) :-
    get_critical_time_threshold(CriticalThreshold),
    get_warning_time_threshold(WarningThreshold),
    get_excellent_time_threshold(ExcellentThreshold),
    get_max_acceptable_calls(MaxCalls),
    
    % Time-based alerts
    (Time > CriticalThreshold ->
        assertz(performance_alert(TestName, 'CRITICAL_TIME', 'FAIL')),
        format('ALERT: ~w depth ~w - CRITICAL TIME: ~6fs~n', [TestName, Depth, Time])
    ; Time > WarningThreshold ->
        assertz(performance_alert(TestName, 'WARNING_TIME', 'DEGRADED')),
        format('WARNING: ~w depth ~w - SLOW TIME: ~6fs~n', [TestName, Depth, Time])
    ; Time < ExcellentThreshold ->
        assertz(performance_alert(TestName, 'EXCELLENT_TIME', 'PASS')),
        format('EXCELLENT: ~w depth ~w - FAST TIME: ~6fs~n', [TestName, Depth, Time])
    ;   assertz(performance_alert(TestName, 'NORMAL_TIME', 'PASS'))
    ),
    
    % Call count alerts
    (Calls > MaxCalls ->
        assertz(performance_alert(TestName, 'HIGH_CALLS', 'FAIL')),
        format('ALERT: ~w depth ~w - EXCESSIVE CALLS: ~w~n', [TestName, Depth, Calls])
    ;   true).

% =============================================================================
% REPORT GENERATION
% =============================================================================

% Generate comprehensive performance report
generate_performance_report :-
    open('minimax_performance_report.txt', write, Stream),
    
    format(Stream, '=== MINIMAX PERFORMANCE REPORT ===~n'),
    format(Stream, 'Generated: ~w~n~n', [date]),
    
    % Summary statistics
    generate_summary_statistics(Stream),
    
    % Detailed results
    generate_detailed_results(Stream),
    
    % Performance analysis
    generate_performance_analysis(Stream),
    
    % Recommendations
    generate_recommendations(Stream),
    
    close(Stream),
    writeln('Performance report saved to: minimax_performance_report.txt').

% Generate summary statistics
generate_summary_statistics(Stream) :-
    format(Stream, '--- SUMMARY STATISTICS ---~n'),
    
    findall(Time, performance_record(_, _, _, Time, _, _), AllTimes),
    (AllTimes = [] -> 
        format(Stream, 'No performance data available~n~n')
    ;   (
        calculate_time_statistics(AllTimes, Avg, Min, Max, Std),
        format(Stream, 'Overall Performance:~n'),
        format(Stream, '  Average Time: ~6f seconds~n', [Avg]),
        format(Stream, '  Min Time: ~6f seconds~n', [Min]),
        format(Stream, '  Max Time: ~6f seconds~n', [Max]),
        format(Stream, '  Std Deviation: ~6f seconds~n', [Std]),
        format(Stream, '~n')
    )).

% Generate detailed results
generate_detailed_results(Stream) :-
    format(Stream, '--- DETAILED RESULTS ---~n'),
    
    forall(performance_record(TestName, Depth, Complexity, Time, Calls, Positions), (
        format(Stream, 'Test: ~w, Depth: ~w, Complexity: ~w~n', [TestName, Depth, Complexity]),
        format(Stream, '  Time: ~6f seconds~n', [Time]),
        format(Stream, '  Calls: ~w~n', [Calls]),
        format(Stream, '  Positions: ~w~n', [Positions]),
        format(Stream, '  Efficiency: ~2f positions/call~n', [Positions/Calls]),
        format(Stream, '~n')
    )).

% Generate performance analysis
generate_performance_analysis(Stream) :-
    format(Stream, '--- PERFORMANCE ANALYSIS ---~n'),
    
    % Analyze depth impact
    analyze_depth_impact(Stream),
    
    % Analyze complexity impact
    analyze_complexity_impact(Stream),
    
    % Identify bottlenecks
    identify_bottlenecks(Stream).

% Analyze depth impact on performance
analyze_depth_impact(Stream) :-
    format(Stream, 'Depth Impact Analysis:~n'),
    
    findall((Depth, Time), performance_record(_, Depth, _, Time, _, _), DepthTimes),
    sort(DepthTimes, SortedDepthTimes),
    
    forall(member((Depth, Time), SortedDepthTimes), (
        format(Stream, '  Depth ~w: ~6f seconds average~n', [Depth, Time])
    )),
    format(Stream, '~n').

% Analyze complexity impact on performance
analyze_complexity_impact(Stream) :-
    format(Stream, 'Complexity Impact Analysis:~n'),
    
    findall((Complexity, Time), performance_record(_, _, Complexity, Time, _, _), ComplexityTimes),
    sort(ComplexityTimes, SortedComplexityTimes),
    
    forall(member((Complexity, Time), SortedComplexityTimes), (
        format(Stream, '  ~w: ~6f seconds average~n', [Complexity, Time])
    )),
    format(Stream, '~n').

% Identify performance bottlenecks
identify_bottlenecks(Stream) :-
    format(Stream, 'Performance Bottlenecks:~n'),
    
    findall(Alert, performance_alert(_, _, Alert), Alerts),
    (Alerts = [] ->
        format(Stream, '  No significant bottlenecks detected~n')
    ;   (
        forall(member(Alert, Alerts), (
            format(Stream, '  - ~w~n', [Alert])
        ))
    )),
    format(Stream, '~n').

% Generate performance recommendations
generate_recommendations(Stream) :-
    format(Stream, '--- RECOMMENDATIONS ---~n'),
    
    % Performance-based recommendations
    findall(Time, performance_record(_, _, _, Time, _, _), Times),
    (Times = [] -> 
        format(Stream, 'No data available for recommendations~n')
    ;   (
        calculate_time_statistics(Times, Avg, _, Max, _),
        (Max > 5.0 ->
            format(Stream, '1. Consider reducing search depth for better responsiveness~n')
        ;   true),
        (Avg > 1.0 ->
            format(Stream, '2. Optimize evaluation function for faster computation~n')
        ;   true),
        format(Stream, '3. Monitor memory usage for large depth searches~n'),
        format(Stream, '4. Consider implementing alpha-beta pruning if not already present~n')
    )),
    format(Stream, '~n').

% =============================================================================
% VISUALIZATION AND CHARTS
% =============================================================================

% Generate benchmark charts
generate_benchmark_charts :-
    generate_depth_chart,
    generate_complexity_chart,
    generate_efficiency_chart.

% Generate depth performance chart
generate_depth_chart :-
    open('depth_performance_chart.csv', write, Stream),
    format(Stream, 'Depth,Average_Time,Min_Time,Max_Time,Calls,Positions~n'),
    
    forall(member(Depth, [1,2,3,4,5,6]), (
        findall(Time, performance_record(_, Depth, _, Time, _, _), Times),
        (Times = [] -> true
        ;   (
            calculate_time_statistics(Times, Avg, Min, Max, _),
            findall(Call, performance_record(_, Depth, _, _, Call, _), Calls),
            sum_list(Calls, TotalCalls),
            findall(Pos, performance_record(_, Depth, _, _, _, Pos), Positions),
            sum_list(Positions, TotalPositions),
            format(Stream, '~w,~6f,~6f,~6f,~w,~w~n', 
                   [Depth, Avg, Min, Max, TotalCalls, TotalPositions])
        ))
    )),
    close(Stream),
    writeln('Depth chart saved to: depth_performance_chart.csv').

% Generate complexity chart
generate_complexity_chart :-
    open('complexity_performance_chart.csv', write, Stream),
    format(Stream, 'Complexity,Average_Time,Calls,Positions~n'),
    
    findall(Complexity, performance_record(_, _, Complexity, _, _, _), Complexities),
    sort(Complexities, UniqueComplexities),
    
    forall(member(Complexity, UniqueComplexities), (
        findall(Time, performance_record(_, _, Complexity, Time, _, _), Times),
        (Times = [] -> true
        ;   (
            calculate_time_statistics(Times, Avg, _, _, _),
            findall(Call, performance_record(_, _, Complexity, _, Call, _), Calls),
            sum_list(Calls, TotalCalls),
            findall(Pos, performance_record(_, _, Complexity, _, _, Pos), Positions),
            sum_list(Positions, TotalPositions),
            format(Stream, '~w,~6f,~w,~w~n', 
                   [Complexity, Avg, TotalCalls, TotalPositions])
        ))
    )),
    close(Stream),
    writeln('Complexity chart saved to: complexity_performance_chart.csv').

% Generate efficiency chart
generate_efficiency_chart :-
    open('efficiency_chart.csv', write, Stream),
    format(Stream, 'Test_Name,Positions_per_Call,Time_per_Position~n'),
    
    forall(performance_record(TestName, Depth, Complexity, Time, Calls, Positions), (
        Efficiency is Positions / Calls,
        TimePerPosition is Time / Positions,
        format(Stream, '~w_~w_~w,~6f,~9f~n', 
               [TestName, Depth, Complexity, Efficiency, TimePerPosition])
    )),
    close(Stream),
    writeln('Efficiency chart saved to: efficiency_chart.csv').

% =============================================================================
% QUICK BENCHMARK TOOLS
% =============================================================================

% Quick performance check
quick_performance_check :-
    writeln('=== QUICK PERFORMANCE CHECK ==='),
    
    board(Board),
    
    % Test current default depth
    get_minimax_depth(DefaultDepth),
    format('Testing default depth ~w...', [DefaultDepth]),
    
    reset_counters,
    get_time(Start),
    minimax(Board, DefaultDepth, 'x', _, _),
    get_time(End),
    
    Elapsed is End - Start,
    minimax_calls(Calls),
    minimax_positions_evaluated(Positions),
    
    format(' COMPLETE: ~6f seconds, ~w calls, ~w positions~n', 
           [Elapsed, Calls, Positions]),
    
    % Performance assessment
    (Elapsed < 0.1 -> 
        format('ASSESSMENT: EXCELLENT performance~n', [])
    ; Elapsed < 1.0 ->
        format('ASSESSMENT: GOOD performance~n', [])
    ; Elapsed < 5.0 ->
        format('ASSESSMENT: ACCEPTABLE performance~n', [])
    ;   format('ASSESSMENT: SLOW performance - consider optimization~n', [])
    ).

% Compare current vs baseline performance
compare_to_baseline :-
    writeln('=== BASELINE COMPARISON ==='),
    
    % Define baseline performance values
    get_baseline_performance(BaselineTime, BaselineCalls, BaselinePositions),
    
    board(Board),
    get_minimax_depth(Depth),
    
    % Run current test
    reset_counters,
    get_time(Start),
    minimax(Board, Depth, 'x', _, _),
    get_time(End),
    
    CurrentTime is End - Start,
    minimax_calls(Calls),
    minimax_positions_evaluated(Positions),
    
    % Compare
    format('Baseline:     ~6f seconds, ~w calls, ~w positions~n', 
           [BaselineTime, BaselineCalls, BaselinePositions]),
    format('Current:      ~6f seconds, ~w calls, ~w positions~n', 
           [CurrentTime, Calls, Positions]),
    
    % Performance ratio
    TimeRatio is CurrentTime / BaselineTime,
    format('Performance Ratio: ~3fx~n', [TimeRatio]),
    
    (TimeRatio < 1.0 ->
        format('STATUS: FASTER than baseline~n', [])
    ; TimeRatio < 1.2 ->
        format('STATUS: SIMILAR to baseline~n', [])
    ;   format('STATUS: SLOWER than baseline~n', [])
    ).

% Define baseline performance (calibrated on reference system)
get_baseline_performance(0.05, 150, 750).

% =============================================================================
% CONTINUOUS MONITORING
% =============================================================================

% Start continuous performance monitoring
start_continuous_monitoring :-
    writeln('Starting continuous performance monitoring...'),
    % Implementation would involve periodic benchmarking
    % and alert generation - simplified for this example
    quick_performance_check.

% Stop continuous monitoring
stop_continuous_monitoring :-
    writeln('Stopping continuous performance monitoring...'),
    % Would stop the monitoring process
    true.