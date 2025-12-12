% Simple validation test for enhanced minimax features
:- consult('puissance4.pro').
:- consult('ai/minimax/minimax.pro').

% Basic functionality test
test_basic_minimax :-
    board(Board),
    minimax(Board, 2, 'x', BestCol, Score),
    format('Basic minimax test: Column ~w, Score ~w~n', [BestCol, Score]).

% Logging test
test_logging :-
    enable_minimax_logging,
    info_log('Testing logging functionality', []),
    disable_minimax_logging.

% Debug test
test_debug :-
    enable_minimax_debug,
    board(Board),
    debug_ai_thinking(Board, 'x', 1),
    disable_minimax_debug.

% Performance test
test_performance :-
    reset_counters,
    board(Board),
    minimax(Board, 2, 'x', _, _),
    get_minimax_stats(Calls, Positions, Time),
    format('Performance test: ~w calls, ~w positions~n', [Calls, Positions]).

% Run all tests
run_all_tests :-
    writeln('=== RUNNING SIMPLE VALIDATION TESTS ==='),
    test_basic_minimax,
    test_logging,
    test_debug,
    test_performance,
    writeln('All tests completed!').

:- run_all_tests, halt.