% Master test runner for all Connect Four test suites
% This file orchestrates the execution of all test modules

:- consult('core_game_tests.pro').
:- consult('game_utils_tests.pro').
:- consult('ai_tests.pro').

% Run all test suites
run_all_tests :-
    writeln('========================================'),
    writeln('CONNECT FOUR COMPREHENSIVE TEST SUITE'),
    writeln('========================================'),
    nl,

    % Core game logic tests
    writeln('Running Core Game Logic Tests...'),
    run_all_core_tests,
    nl,

    % Game utilities tests
    writeln('Running Game Utilities Tests...'),
    run_all_game_utils_tests,
    nl,

    % AI implementation tests
    writeln('Running AI Implementation Tests...'),
    run_all_ai_tests,
    nl,

    writeln('========================================'),
    writeln('ALL TESTS COMPLETED SUCCESSFULLY!'),
    writeln('========================================').

% Quick test for development (run only essential tests)
quick_test :-
    writeln('Running Quick Test Suite...'),
    quick_core_test,
    quick_game_utils_test,
    quick_ai_test,
    writeln('Quick tests completed!').

% Individual test suite runners (for targeted testing)
run_core_tests :- run_all_core_tests.
run_utils_tests :- run_all_game_utils_tests.
run_ai_tests :- run_all_ai_tests.