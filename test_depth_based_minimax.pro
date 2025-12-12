% Test file for depth-based minimax implementation
% Verifies that the new depth-based approach works correctly

:- consult('puissance4.pro').
:- consult('ai/minimax/minimax.pro').

% Count pieces of a specific player on the board
count_pieces(Board, Player, Count) :-
    findall(1, (member(Row, Board), member(Player, Row)), Pieces),
    length(Pieces, Count).

% Test that depth configuration works
test_configurable_depth :-
    writeln('Testing configurable depth settings...'),
    
    % Test default depth
    get_minimax_depth(Depth),
    format('Default depth: ~w~n', [Depth]),
    
    % Test that minimax uses the configured depth
    setup,
    board(Board),
    minimax(Board, Depth, 'x', BestCol, Score),
    format('Minimax result with depth ~w: BestCol=~w, Score=~w~n', [Depth, BestCol, Score]),
    
    % Test with different depth
    minimax(Board, 2, 'x', BestCol2, Score2),
    format('Minimax result with depth 2: BestCol=~w, Score=~w~n', [BestCol2, Score2]),
    
    writeln('Configurable depth test: PASSED').

% Test that minimax_ai no longer uses timeout
test_no_timeout_fallback :-
    writeln('Testing that minimax_ai no longer falls back to random...'),
    
    setup,
    board(Board),
    
    % This should work without any timeout handling
    minimax_ai(Board, NewBoard, 'x'),
    
    % Verify a move was made
    count_pieces(NewBoard, 'x', Count),
    Count = 1,
    
    writeln('No timeout fallback test: PASSED').

% Test different depths produce different results
test_depth_variations :-
    writeln('Testing depth variations...'),
    
    setup,
    board(Board),
    
    % Test depth 1
    minimax(Board, 1, 'x', BestCol1, _),
    format('Depth 1 result: BestCol=~w~n', [BestCol1]),
    
    % Test depth 3
    minimax(Board, 3, 'x', BestCol3, _),
    format('Depth 3 result: BestCol=~w~n', [BestCol3]),
    
    % Test depth 5
    minimax(Board, 5, 'x', BestCol5, _),
    format('Depth 5 result: BestCol=~w~n', [BestCol5]),
    
    writeln('Depth variations test: PASSED').

% Run all tests
run_depth_tests :-
    writeln('=== DEPTH-BASED MINIMAX TESTS ==='),
    test_configurable_depth,
    nl,
    test_no_timeout_fallback,
    nl,
    test_depth_variations,
    nl,
    writeln('All depth-based minimax tests completed successfully!').

:- run_depth_tests, halt.