% Test runner script for minimax AI tests
:- consult('puissance4.pro').
:- consult('test_minimax_comprehensive').

% Simple test to verify basic functionality
test_basic_minimax :-
    writeln('Testing basic minimax functionality...'),
    setup,
    board(Board),
    writeln('Empty board created'),
    
    % Test minimax with depth 0
    minimax(Board, 0, 'x', BestCol, Score),
    format('Depth 0: BestCol=~w, Score=~w~n', [BestCol, Score]),
    
    % Test minimax with depth 1
    minimax(Board, 1, 'x', BestCol1, Score1),
    format('Depth 1: BestCol=~w, Score=~w~n', [BestCol1, Score1]),
    
    % Test minimax_ai
    minimax_ai(Board, NewBoard, 'x'),
    writeln('minimax_ai completed successfully'),
    
    writeln('All basic tests passed!').

% Run comprehensive tests
run_comprehensive_tests :-
    writeln('Running comprehensive test suite...'),
    run_all_tests.

% Main entry point
main :-
    test_basic_minimax,
    nl,
    run_comprehensive_tests,
    halt.

% Start tests when script is run
:- initialization(main, main).