% Simple Test for Minimax Logging Fixes

:- consult('ai/minimax/minimax.pro').
:- consult('ai/evaluation.pro').
:- consult('ai/game_utils.pro').
:- consult('ai/selector.pro').
:- consult('puissance4.pro').

% Test 1: Test the critical logging fix
test_logging_fix :-
    write('Testing logging fix...'), nl,
    enable_minimax_logging,
    % This was the failing call - should work now
    info_log('Starting minimax AI with depth 4', []),
    write('Logging test PASSED'), nl.

% Test 2: Test minimax AI works
test_minimax_ai :-
    write('Testing minimax AI...'), nl,
    % Initialize game state
    setup,
    Board = [[o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [x,x,o,o,o,o,o]],
    minimax_ai(Board, NewBoard, x),
    write('Minimax AI test PASSED'), nl.

% Test 3: Test game flow
test_game_flow :-
    write('Testing game flow...'), nl,
    % Initialize game state
    setup,
    InitialBoard = [[o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o],
                    [o,o,o,o,o,o,o]],
    minimax_ai(InitialBoard, Board1, x),
    minimax_ai(Board1, Board2, o),
    write('Game flow test PASSED'), nl.

% Run all tests
run_tests :-
    write('=== MINIMAX FIXES TEST ==='), nl,
    test_logging_fix,
    test_minimax_ai,
    test_game_flow,
    write('=== ALL TESTS COMPLETE ==='), nl.

:- initialization(run_tests, main).