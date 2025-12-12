% Detailed Debug Test for Minimax

:- consult('ai/minimax/minimax.pro').
:- consult('ai/evaluation.pro').
:- consult('ai/game_utils.pro').
:- consult('ai/selector.pro').
:- consult('puissance4.pro').

% Detailed test with error catching
detailed_test :-
    write('=== DETAILED MINIMAX DEBUG TEST ==='), nl,
    
    % Test 1: Setup and Logging
    write('Step 1: Testing setup...'), nl,
    setup,
    write('✓ Setup complete'), nl,
    
    write('Step 2: Testing logging...'), nl,
    enable_minimax_logging,
    info_log('Test message with depth 4', []),
    write('✓ Logging works'), nl,
    
    % Test 2: Basic board and minimax
    write('Step 3: Testing basic minimax...'), nl,
    Board = [[o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [x,x,o,o,o,o,o]],
    
    write('Step 4: Calling minimax_ai...'), nl,
    catch(
        (minimax_ai(Board, NewBoard, x),
         write('✓ Minimax AI completed successfully'), nl,
         write('Board changed: '), write(Board \= NewBoard), nl),
        Error,
        (write('✗ Minimax AI failed: '), write(Error), nl, fail)
    ),
    
    % Test 3: Statistics
    write('Step 5: Testing statistics...'), nl,
    catch(
        get_minimax_stats(Calls, Positions, Time),
        (write('✓ Statistics: Calls='), write(Calls), 
         write(', Positions='), write(Positions), nl),
        Error2,
        (write('✗ Statistics failed: '), write(Error2), nl, fail)
    ),
    
    write('=== ALL TESTS COMPLETED ==='), nl.

% Test Player vs IA flow
test_player_vs_ia_flow :-
    write('=== TESTING PLAYER VS IA FLOW ==='), nl,
    
    setup,
    enable_minimax_logging,
    
    % Get initial board state
    board(InitialBoard),
    write('Initial board: '), write(InitialBoard), nl,
    
    % Test AI move for player x
    write('AI move for player x...'), nl,
    catch(
        minimax_ai(InitialBoard, Board1, x),
        Error,
        (write('✗ AI move failed: '), write(Error), nl, fail)
    ),
    
    write('✓ AI move completed'), nl,
    write('New board: '), write(Board1), nl,
    
    % Apply the move to game state
    applyBoard(InitialBoard, Board1),
    write('✓ Game state updated'), nl,
    
    write('=== PLAYER VS IA FLOW TEST COMPLETE ==='), nl.

% Run detailed tests
run_debug_tests :-
    detailed_test,
    nl, nl,
    test_player_vs_ia_flow.

:- initialization(run_debug_tests, main).