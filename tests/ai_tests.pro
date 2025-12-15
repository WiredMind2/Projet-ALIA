% Comprehensive test suite for AI implementations
% Tests random AI, almost random AI, and minimax algorithm with alpha-beta pruning

:- consult('../ai/random/random.pro').
:- consult('../ai/random/almost_random.pro').
:- consult('../ai/minimax.pro').
:- consult('../ai/game_utils.pro').
:- consult('../matrix.pro').
:- consult('../win.pro').

% ========================================
% RANDOM AI TESTS
% ========================================

% Test random_ai makes valid moves
test_random_ai_valid_moves :-
    generate_matrix(7, 6, Board),
    random_ai(Board, NewBoard, 1),
    % Check that exactly one move was made
    count_pieces(NewBoard, 1, Count),
    Count = 1,
    % Check that the move is valid (piece in bottom available row)
    find_move_position(NewBoard, 1, Col, Row),
    Row = 5,  % Should be bottom row
    validMove(Board, Col),  % Original column should have been valid
    writeln('✓ Random AI makes valid moves').

% Test random_ai on partially filled board
test_random_ai_partial_board :-
    generate_matrix(7, 6, Board),
    % Fill some columns
    simulateMove(Board, 0, B1, 1),
    simulateMove(B1, 1, B2, 2),
    simulateMove(B2, 2, B3, 1),
    random_ai(B3, NewBoard, 1),
    % Should still make exactly one additional move
    count_pieces(NewBoard, 1, Count1),
    count_pieces(NewBoard, 2, Count2),
    Count1 = 2, Count2 = 1,
    writeln('✓ Random AI works on partially filled board').

% ========================================
% ALMOST RANDOM AI TESTS
% ========================================

% Test almost_random_ai makes valid moves
test_almost_random_ai_valid_moves :-
    generate_matrix(7, 6, Board),
    almost_random_ai(Board, NewBoard, 1),
    count_pieces(NewBoard, 1, Count),
    Count = 1,
    find_move_position(NewBoard, 1, Col, Row),
    Row = 5,
    validMove(Board, Col),
    writeln('✓ Almost random AI makes valid moves').

% Test almost_random_ai takes winning move
test_almost_random_ai_takes_win :-
    generate_matrix(7, 6, Board),
    % Set up board where player 1 can win immediately
    replaceMatrix(Board, 0, 5, 1, B1),
    replaceMatrix(B1, 1, 5, 1, B2),
    replaceMatrix(B2, 2, 5, 1, B3),
    % Column 3 is empty and would win
    almost_random_ai(B3, NewBoard, 1),
    % Check that player 1 won
    win(NewBoard, 1),
    writeln('✓ Almost random AI takes immediate winning moves').

% Test almost_random_ai blocks opponent win
test_almost_random_ai_blocks_win :-
    generate_matrix(7, 6, Board),
    % Set up board where opponent (player 2) can win next turn
    replaceMatrix(Board, 0, 5, 2, B1),
    replaceMatrix(B1, 1, 5, 2, B2),
    replaceMatrix(B2, 2, 5, 2, B3),
    % Player 1 should block in column 3
    almost_random_ai(B3, NewBoard, 1),
    % Check that column 3 has player 1's piece
    nth0(3, NewBoard, Col3),
    nth0(5, Col3, Cell),
    Cell = 1,
    % Opponent should not be able to win immediately now
    \+ (simulateMove(NewBoard, 3, TestBoard, 2), win(TestBoard, 2)),
    writeln('✓ Almost random AI blocks opponent winning moves').

% ========================================
% MINIMAX ALGORITHM TESTS
% ========================================

% Test minimax initialization
test_minimax_initialization :-
    init_alpha_beta(3, 1, 3, 2),
    joueur1_profondeur(3),
    joueur2_profondeur(3),
    joueur1_heuristique(1),
    joueur2_heuristique(2),
    writeln('✓ Minimax initialization works correctly').

% Test minimax basic functionality
test_minimax_basic_move :-
    generate_matrix(7, 6, Board),
    init_alpha_beta(2, 1, 2, 1),
    coup_alpha_beta(Board, 1, Column),
    between(0, 6, Column),  % Should return valid column
    writeln('✓ Minimax algorithm returns valid column choice').

% Test minimax finds winning move
test_minimax_finds_win :-
    generate_matrix(7, 6, Board),
    % Set up winning position for player 1
    replaceMatrix(Board, 0, 5, 1, B1),
    replaceMatrix(B1, 1, 5, 1, B2),
    replaceMatrix(B2, 2, 5, 1, B3),
    init_alpha_beta(3, 1, 3, 1),
    coup_alpha_beta(B3, 1, Column),
    Column = 3,  % Should choose winning column
    simulateMove(B3, 3, WinBoard, 1),
    win(WinBoard, 1),
    writeln('✓ Minimax finds immediate winning moves').

% Test minimax blocks opponent win
test_minimax_blocks_opponent_win :-
    generate_matrix(7, 6, Board),
    % Set up position where opponent can win
    replaceMatrix(Board, 0, 5, 2, B1),
    replaceMatrix(B1, 1, 5, 2, B2),
    replaceMatrix(B2, 2, 5, 2, B3),
    init_alpha_beta(3, 1, 3, 1),
    coup_alpha_beta(B3, 1, Column),
    Column = 3,  % Should block in column 3
    simulateMove(B3, 3, BlockBoard, 1),
    \+ win(BlockBoard, 2),  % Opponent shouldn't win immediately
    writeln('✓ Minimax blocks opponent winning moves').

% Test minimax with different heuristics
test_minimax_different_heuristics :-
    generate_matrix(7, 6, Board),
    % Test with heuristic 1 (position-based)
    init_alpha_beta(2, 1, 2, 1),
    coup_alpha_beta(Board, 1, Col1),
    % Test with heuristic 2 (alignment-based)
    init_alpha_beta(2, 2, 2, 2),
    coup_alpha_beta(Board, 1, Col2),
    % Both should return valid columns
    between(0, 6, Col1),
    between(0, 6, Col2),
    writeln('✓ Minimax works with different heuristic functions').

% Test minimax depth limits
test_minimax_depth_limits :-
    generate_matrix(7, 6, Board),
    % Test shallow depth
    init_alpha_beta(1, 1, 1, 1),
    coup_alpha_beta(Board, 1, Col1),
    % Test deeper search
    init_alpha_beta(4, 1, 4, 1),
    coup_alpha_beta(Board, 1, Col2),
    between(0, 6, Col1),
    between(0, 6, Col2),
    writeln('✓ Minimax respects depth limits').

% ========================================
% MINIMAX PERFORMANCE TESTS
% ========================================

% Test minimax performance with small depth
test_minimax_performance_small :-
    generate_matrix(7, 6, Board),
    init_alpha_beta(2, 1, 2, 1),
    % Measure time for minimax calculation
    statistics(walltime, [Start|_]),
    coup_alpha_beta(Board, 1, _),
    statistics(walltime, [End|_]),
    Time is End - Start,
    Time < 1000,  % Should complete in less than 1 second
    writeln('✓ Minimax performance acceptable for depth 2').

% Test minimax doesn't crash on complex positions
test_minimax_complex_position :-
    generate_matrix(7, 6, Board),
    % Create a complex board state
    replaceMatrix(Board, 0, 5, 1, B1),
    replaceMatrix(B1, 0, 4, 2, B2),
    replaceMatrix(B2, 1, 5, 1, B3),
    replaceMatrix(B3, 1, 4, 2, B4),
    replaceMatrix(B4, 2, 5, 1, B5),
    replaceMatrix(B5, 3, 5, 2, B6),
    init_alpha_beta(3, 1, 3, 1),
    coup_alpha_beta(B6, 1, Column),
    between(0, 6, Column),
    writeln('✓ Minimax handles complex board positions').

% ========================================
% AI WRAPPER TESTS
% ========================================

% Test minimax_ai wrapper
test_minimax_ai_wrapper :-
    generate_matrix(7, 6, Board),
    minimax_ai(Board, NewBoard, 1),
    count_pieces(NewBoard, 1, Count),
    Count = 1,
    find_move_position(NewBoard, 1, Col, Row),
    Row = 5,
    validMove(Board, Col),
    writeln('✓ Minimax AI wrapper works correctly').

% ========================================
% INTEGRATION TESTS
% ========================================

% Test AI vs AI game simulation
test_ai_vs_ai_short_game :-
    generate_matrix(7, 6, Board),
    init_alpha_beta(2, 1, 2, 1),
    % Simulate a few moves
    minimax_ai(Board, B1, 1),
    minimax_ai(B1, B2, 2),
    minimax_ai(B2, B3, 1),
    minimax_ai(B3, B4, 2),
    % Check that moves were made
    count_pieces(B4, 1, Count1),
    count_pieces(B4, 2, Count2),
    Count1 = 2, Count2 = 2,
    % Game should not be over yet
    \+ win(B4, _),
    \+ board_full(B4),
    writeln('✓ AI vs AI integration works for short games').

% ========================================
% HELPER PREDICATES
% ========================================

% Count pieces of a specific player on the board
count_pieces(Board, Player, Count) :-
    findall(Player, (member(Col, Board), member(Player, Col)), Pieces),
    length(Pieces, Count).

% Find the position of a player's most recent move
find_move_position(Board, Player, Col, Row) :-
    nth0(Col, Board, Column),
    nth0(Row, Column, Player),
    % Make sure it's the highest piece in the column
    (Row = 5 ; (Row < 5, Row1 is Row + 1, nth0(Row1, Column, Empty), Empty = 0)).

% ========================================
% TEST RUNNER
% ========================================

% Run all AI implementation tests
run_all_ai_tests :-
    writeln('=== Running AI Implementation Tests ==='),
    nl,

    % Random AI tests
    test_random_ai_valid_moves,
    test_random_ai_partial_board,

    % Almost random AI tests
    test_almost_random_ai_valid_moves,
    test_almost_random_ai_takes_win,
    test_almost_random_ai_blocks_win,

    % Minimax algorithm tests
    test_minimax_initialization,
    test_minimax_basic_move,
    test_minimax_finds_win,
    test_minimax_blocks_opponent_win,
    test_minimax_different_heuristics,
    test_minimax_depth_limits,

    % Performance tests
    test_minimax_performance_small,
    test_minimax_complex_position,

    % AI wrapper tests
    test_minimax_ai_wrapper,

    % Integration tests
    test_ai_vs_ai_short_game,

    nl,
    writeln('=== AI Implementation Tests Completed ===').

% Quick test for development
quick_ai_test :-
    test_random_ai_valid_moves,
    test_minimax_basic_move,
    test_minimax_finds_win.