% Test script for enhanced evaluation
:- initialization(main).

main :-
    % Load required files
    consult('matrix.pro'),
    consult('win.pro'),
    consult('ai/evaluation_enhanced.pro'),
    
    % Run the test
    test_basic,
    
    % Additional test for enhanced evaluation
    writeln('Testing enhanced evaluation function...'),
    
    % Test board with some pieces
    TestBoard = [['x','o','.','.','.','.','.'],
                 ['x','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.']],
    
    % Test enhanced evaluation
    evaluate_enhanced(TestBoard, 'x', Score),
    format('Enhanced evaluation score for test board: ~w~n', [Score]),
    
    % Test game phase detection
    get_game_phase(TestBoard, Phase),
    format('Game phase detected: ~w~n', [Phase]),
    
    % Test pattern counting
    count_three_in_row(TestBoard, 'x', ThreeCount),
    format('Three in row count for x: ~w~n', [ThreeCount]),
    
    count_two_in_row(TestBoard, 'x', TwoCount),
    format('Two in row count for x: ~w~n', [TwoCount]),
    
    writeln('All tests completed successfully!'),
    halt.