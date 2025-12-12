% Final test runner
:- consult('puissance4.pro').
:- consult('tests/test_minimax_comprehensive.pro').

% Run all the basic tests
run_basic_tests :-
    writeln('=== RUNNING BASIC MINIMAX TESTS ==='),
    
    % Test 1: Basic setup and valid moves
    setup,
    findall(C, validMove(C), ValidMoves),
    format('✓ Test 1 - Valid moves: Found ~w valid moves~n', [length(ValidMoves, _)]),
    
    % Test 2: Minimax depth 0
    board(Board),
    minimax(Board, 0, 'x', BestCol0, Score0),
    format('✓ Test 2 - Minimax depth 0: BestCol=~w, Score=~w~n', [BestCol0, Score0]),
    
    % Test 3: Minimax depth 1
    minimax(Board, 1, 'x', BestCol1, Score1),
    format('✓ Test 3 - Minimax depth 1: BestCol=~w, Score=~w~n', [BestCol1, Score1]),
    
    % Test 4: Minimax AI
    minimax_ai(Board, NewBoard, 'x'),
    format('✓ Test 4 - Minimax AI: Successfully made a move~n', []),
    
    % Test 5: Game over detection
    game_over(Board, Result),
    format('✓ Test 5 - Game over: Result=~w~n', [Result]),
    
    % Test 6: Evaluation function
    evaluate(Board, 'x', EvalScore),
    format('✓ Test 6 - Evaluation: Score=~w~n', [EvalScore]),
    
    writeln('=== ALL BASIC TESTS COMPLETED ===').

:- run_basic_tests, halt.