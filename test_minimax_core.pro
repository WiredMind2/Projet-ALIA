% Core Minimax Test - isolate the issue

:- consult('ai/minimax/minimax.pro').
:- consult('ai/evaluation.pro').
:- consult('ai/game_utils.pro').
:- consult('ai/selector.pro').
:- consult('puissance4.pro').

% Test just the core minimax algorithm
test_core_minimax :-
    write('=== TESTING CORE MINIMAX ALGORITHM ==='), nl,
    
    setup,
    enable_minimax_logging,
    
    % Create a simple test board
    Board = [[o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [x,x,o,o,o,o,o]],
    
    write('Testing minimax/5 directly...'), nl,
    catch(
        (minimax(Board, 2, x, BestCol, Score),
         write('✓ Core minimax works!'), nl,
         write('Best column: '), write(BestCol), nl,
         write('Score: '), write(Score), nl),
        Error,
        (write('✗ Core minimax failed: '), write(Error), nl, fail)
    ).
    
% Test the evaluation function
test_evaluation :-
    write('=== TESTING EVALUATION FUNCTION ==='), nl,
    
    setup,
    
    Board = [[o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [x,x,o,o,o,o,o]],
    
    catch(
        (evaluate(Board, x, Score),
         write('✓ Evaluation works!'), nl,
         write('Score: '), write(Score), nl),
        Error,
        (write('✗ Evaluation failed: '), write(Error), nl, fail)
    ).

% Test valid moves
test_valid_moves :-
    write('=== TESTING VALID MOVES ==='), nl,
    
    setup,
    
    catch(
        (findall(Col, validMove(Col), ValidMoves),
         write('✓ Valid moves found: '), write(ValidMoves), nl,
         ValidMoves \= []),
        Error,
        (write('✗ Valid moves failed: '), write(Error), nl, fail)
    ).

% Test simulateMove (used by minimax)
test_simulate_move :-
    write('=== TESTING SIMULATE MOVE ==='), nl,
    
    setup,
    
    Board = [[o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [o,o,o,o,o,o,o],
             [x,x,o,o,o,o,o]],
    
    catch(
        (simulateMove(Board, 0, NewBoard, x),
         write('✓ Simulate move works!'), nl,
         write('Original board: '), write(Board), nl,
         write('New board: '), write(NewBoard), nl,
         Board \= NewBoard),
        Error,
        (write('✗ Simulate move failed: '), write(Error), nl, fail)
    ).

% Run core tests
run_core_tests :-
    test_core_minimax,
    nl,
    test_evaluation,
    nl,
    test_valid_moves,
    nl,
    test_simulate_move,
    nl,
    write('=== CORE TESTS COMPLETE ==='), nl.

:- initialization(run_core_tests, main).