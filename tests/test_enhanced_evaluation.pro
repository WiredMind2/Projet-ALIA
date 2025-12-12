% Test file for enhanced evaluation function
:- consult('puissance4.pro').
:- consult('ai/evaluation.pro').
:- consult('ai/evaluation_enhanced.pro').

% Test basic functionality
test_enhanced_evaluation :-
    setup,
    board(Board),
    writeln('Testing enhanced evaluation...'),
    
    % Test empty board
    evaluate(Board, 'x', Score1),
    format('Empty board score: ~w~n', [Score1]),
    
    % Test with some pieces
    playMove(Board, 3, TestBoard, 'x'),
    evaluate(TestBoard, 'x', Score2),
    format('Board with center piece score: ~w~n', [Score2]),
    
    % Test game phase detection
    get_game_phase(Board, Phase),
    format('Empty board phase: ~w~n', [Phase]),
    
    writeln('Enhanced evaluation test completed successfully!').

% Run the test
:- test_enhanced_evaluation, halt.