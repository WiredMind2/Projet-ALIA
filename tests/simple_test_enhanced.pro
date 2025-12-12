% Simple test for enhanced evaluation
:- consult('matrix.pro').
:- consult('win.pro').
:- consult('ai/evaluation_enhanced.pro').

% Test basic functionality
test_basic :-
    % Test game phase detection
    EmptyBoard = [['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.']],
    
    get_game_phase(EmptyBoard, Phase),
    format('Empty board phase: ~w~n', [Phase]),
    
    % Test pattern counting
    TestBoard = [['x','x','x','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.']],
    
    count_three_in_row(TestBoard, 'x', Count),
    format('Three in row count: ~w~n', [Count]),
    
    % Test center control
    center_score_basic(TestBoard, 'x', CenterScore),
    format('Center score: ~w~n', [CenterScore]),
    
    writeln('Basic test completed successfully!').

:- test_basic, halt.