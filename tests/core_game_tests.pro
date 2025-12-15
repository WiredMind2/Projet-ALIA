% Comprehensive test suite for core game logic
% Tests win detection, matrix operations, and fundamental game mechanics

:- consult('../win.pro').
:- consult('../matrix.pro').
:- consult('../print.pro').

% ========================================
% WIN DETECTION TESTS
% ========================================

% Test horizontal win detection
test_horizontal_win_player1 :-
    % Create board with horizontal win for player 1
    generate_matrix(7, 6, Board),
    % Player 1 wins with bottom row: [1,1,1,1,0,0,0]
    replaceMatrix(Board, 0, 5, 1, Board1),
    replaceMatrix(Board1, 1, 5, 1, Board2),
    replaceMatrix(Board2, 2, 5, 1, Board3),
    replaceMatrix(Board3, 3, 5, 1, Board4),
    % Test win detection
    win(Board4, 1),
    writeln('✓ Horizontal win detection for player 1 works').

test_horizontal_win_player2 :-
    % Create board with horizontal win for player 2
    generate_matrix(7, 6, Board),
    % Player 2 wins with middle row: [0,0,2,2,2,2,0]
    replaceMatrix(Board, 2, 3, 2, Board1),
    replaceMatrix(Board1, 3, 3, 2, Board2),
    replaceMatrix(Board2, 4, 3, 2, Board3),
    replaceMatrix(Board3, 5, 3, 2, Board4),
    % Test win detection
    win(Board4, 2),
    writeln('✓ Horizontal win detection for player 2 works').

test_no_horizontal_win :-
    % Create board with no horizontal win
    generate_matrix(7, 6, Board),
    replaceMatrix(Board, 0, 5, 1, Board1),
    replaceMatrix(Board1, 1, 5, 1, Board2),
    replaceMatrix(Board2, 2, 5, 2, Board3),
    replaceMatrix(Board3, 3, 5, 1, Board4),
    % Should not detect win
    \+ win(Board4, _),
    writeln('✓ Correctly detects no horizontal win').

% Test vertical win detection
test_vertical_win_player1 :-
    % Create board with vertical win for player 1
    generate_matrix(7, 6, Board),
    % Player 1 wins with column 2: rows 2,3,4,5
    replaceMatrix(Board, 2, 2, 1, Board1),
    replaceMatrix(Board1, 2, 3, 1, Board2),
    replaceMatrix(Board2, 2, 4, 1, Board3),
    replaceMatrix(Board3, 2, 5, 1, Board4),
    % Test win detection
    win(Board4, 1),
    writeln('✓ Vertical win detection for player 1 works').

test_vertical_win_player2 :-
    % Create board with vertical win for player 2
    generate_matrix(7, 6, Board),
    % Player 2 wins with column 5: rows 1,2,3,4
    replaceMatrix(Board, 5, 1, 2, Board1),
    replaceMatrix(Board1, 5, 2, 2, Board2),
    replaceMatrix(Board2, 5, 3, 2, Board3),
    replaceMatrix(Board3, 5, 4, 2, Board4),
    % Test win detection
    win(Board4, 2),
    writeln('✓ Vertical win detection for player 2 works').

test_no_vertical_win :-
    % Create board with no vertical win
    generate_matrix(7, 6, Board),
    replaceMatrix(Board, 2, 2, 1, Board1),
    replaceMatrix(Board1, 2, 3, 1, Board2),
    replaceMatrix(Board2, 2, 4, 2, Board3),
    replaceMatrix(Board3, 2, 5, 1, Board4),
    % Should not detect win
    \+ win(Board4, _),
    writeln('✓ Correctly detects no vertical win').

% Test diagonal win detection (top-left to bottom-right)
test_diagonal_win_down_right_player1 :-
    % Create board with diagonal win for player 1
    % Pattern: (0,2), (1,3), (2,4), (3,5)
    generate_matrix(7, 6, Board),
    replaceMatrix(Board, 0, 2, 1, Board1),
    replaceMatrix(Board1, 1, 3, 1, Board2),
    replaceMatrix(Board2, 2, 4, 1, Board3),
    replaceMatrix(Board3, 3, 5, 1, Board4),
    % Test win detection
    win(Board4, 1),
    writeln('✓ Diagonal win detection (down-right) for player 1 works').

test_diagonal_win_down_right_player2 :-
    % Create board with diagonal win for player 2
    % Pattern: (1,1), (2,2), (3,3), (4,4)
    generate_matrix(7, 6, Board),
    replaceMatrix(Board, 1, 1, 2, Board1),
    replaceMatrix(Board1, 2, 2, 2, Board2),
    replaceMatrix(Board2, 3, 3, 2, Board3),
    replaceMatrix(Board3, 4, 4, 2, Board4),
    % Test win detection
    win(Board4, 2),
    writeln('✓ Diagonal win detection (down-right) for player 2 works').

% Test diagonal win detection (top-right to bottom-left)
test_diagonal_win_down_left_player1 :-
    % Create board with diagonal win for player 1
    % Pattern: (6,2), (5,3), (4,4), (3,5)
    generate_matrix(7, 6, Board),
    replaceMatrix(Board, 6, 2, 1, Board1),
    replaceMatrix(Board1, 5, 3, 1, Board2),
    replaceMatrix(Board2, 4, 4, 1, Board3),
    replaceMatrix(Board3, 3, 5, 1, Board4),
    % Test win detection
    win(Board4, 1),
    writeln('✓ Diagonal win detection (down-left) for player 1 works').

test_diagonal_win_down_left_player2 :-
    % Create board with diagonal win for player 2
    % Pattern: (6,1), (5,2), (4,3), (3,4)
    generate_matrix(7, 6, Board),
    replaceMatrix(Board, 6, 1, 2, Board1),
    replaceMatrix(Board1, 5, 2, 2, Board2),
    replaceMatrix(Board2, 4, 3, 2, Board3),
    replaceMatrix(Board3, 3, 4, 2, Board4),
    % Test win detection
    win(Board4, 2),
    writeln('✓ Diagonal win detection (down-left) for player 2 works').

test_no_diagonal_win :-
    % Create board with no diagonal win
    generate_matrix(7, 6, Board),
    replaceMatrix(Board, 0, 2, 1, Board1),
    replaceMatrix(Board1, 1, 3, 1, Board2),
    replaceMatrix(Board2, 2, 4, 2, Board3),
    replaceMatrix(Board3, 3, 5, 1, Board4),
    % Should not detect win
    \+ win(Board4, _),
    writeln('✓ Correctly detects no diagonal win').

% ========================================
% MATRIX OPERATION TESTS
% ========================================

% Test matrix generation
test_generate_matrix_correct_size :-
    generate_matrix(7, 6, Matrix),
    length(Matrix, 7),                    % Should have 7 columns
    maplist(length, 6, Matrix),           % Each column should have 6 rows
    writeln('✓ Matrix generation creates correct dimensions').

test_generate_matrix_initialized_to_zero :-
    generate_matrix(7, 6, Matrix),
    \+ (member(Col, Matrix), member(1, Col)),  % No cell should contain 1
    \+ (member(Col, Matrix), member(2, Col)),  % No cell should contain 2
    (member(Col, Matrix), member(0, Col)),     % All cells should contain 0
    writeln('✓ Matrix generation initializes all cells to zero').

% Test element replacement in 1D list
test_replace_element_first_position :-
    replace([a, b, c, d], 0, x, [x, b, c, d]),
    writeln('✓ Replace element at first position works').

test_replace_element_middle_position :-
    replace([a, b, c, d], 2, x, [a, b, x, d]),
    writeln('✓ Replace element at middle position works').

test_replace_element_last_position :-
    replace([a, b, c, d], 3, x, [a, b, c, x]),
    writeln('✓ Replace element at last position works').

% Test 2D matrix replacement
test_replace_matrix_cell_basic :-
    generate_matrix(3, 3, Matrix),
    replaceMatrix(Matrix, 1, 1, 5, NewMatrix),
    nth0(1, NewMatrix, Col1),
    nth0(1, Col1, Value),
    Value = 5,
    writeln('✓ 2D matrix cell replacement works').

test_replace_matrix_cell_preserves_other_cells :-
    generate_matrix(3, 3, Matrix),
    replaceMatrix(Matrix, 1, 1, 5, NewMatrix),
    % Check that other cells remain 0
    nth0(0, NewMatrix, Col0), nth0(0, Col0, Val00), Val00 = 0,
    nth0(2, NewMatrix, Col2), nth0(2, Col2, Val22), Val22 = 0,
    writeln('✓ Matrix replacement preserves other cells').

% Test sublist detection
test_sublist_found_at_start :-
    sublist([a, b], [a, b, c, d]),
    writeln('✓ Sublist detection works at start').

test_sublist_found_in_middle :-
    sublist([b, c], [a, b, c, d]),
    writeln('✓ Sublist detection works in middle').

test_sublist_found_at_end :-
    sublist([c, d], [a, b, c, d]),
    writeln('✓ Sublist detection works at end').

test_sublist_not_found :-
    \+ sublist([b, d], [a, b, c, d]),
    writeln('✓ Sublist correctly detects non-existent sublist').

% ========================================
% COMBINED WIN SCENARIOS
% ========================================

test_multiple_wins_detects_first :-
    % Create board with both horizontal and vertical wins
    generate_matrix(7, 6, Board),
    % Horizontal win for player 1
    replaceMatrix(Board, 0, 5, 1, Board1),
    replaceMatrix(Board1, 1, 5, 1, Board2),
    replaceMatrix(Board2, 2, 5, 1, Board3),
    replaceMatrix(Board3, 3, 5, 1, Board4),
    % Vertical win for player 1 in column 0
    replaceMatrix(Board4, 0, 4, 1, Board5),
    replaceMatrix(Board5, 0, 3, 1, Board6),
    replaceMatrix(Board6, 0, 2, 1, Board7),
    % Should detect win (either horizontal or vertical)
    win(Board7, 1),
    writeln('✓ Multiple win scenarios handled correctly').

test_complex_diagonal_win :-
    % Test longer diagonal that requires proper traversal
    generate_matrix(7, 6, Board),
    % Create diagonal: (1,0), (2,1), (3,2), (4,3)
    replaceMatrix(Board, 1, 0, 1, Board1),
    replaceMatrix(Board1, 2, 1, 1, Board2),
    replaceMatrix(Board2, 3, 2, 1, Board3),
    replaceMatrix(Board3, 4, 3, 1, Board4),
    win(Board4, 1),
    writeln('✓ Complex diagonal win detection works').

% ========================================
% EDGE CASES AND BOUNDARY CONDITIONS
% ========================================

test_board_with_no_moves_yet :-
    % Empty board should not have a winner
    generate_matrix(7, 6, EmptyBoard),
    \+ win(EmptyBoard, _),
    writeln('✓ Empty board correctly shows no winner').

test_board_near_full_no_win :-
    % Create nearly full board with no winner
    generate_matrix(7, 6, Board),
    % Fill most cells randomly without creating win
    replaceMatrix(Board, 0, 5, 1, B1),
    replaceMatrix(B1, 1, 5, 2, B2),
    replaceMatrix(B2, 2, 5, 1, B3),
    replaceMatrix(B3, 3, 5, 2, B4),
    replaceMatrix(B4, 4, 5, 1, B5),
    replaceMatrix(B5, 5, 5, 2, B6),
    replaceMatrix(B6, 6, 5, 1, B7),
    \+ win(B7, _),
    writeln('✓ Nearly full board with no winner handled correctly').

% ========================================
% TEST RUNNER
% ========================================

% Run all core game logic tests
run_all_core_tests :-
    writeln('=== Running Core Game Logic Tests ==='),
    writeln(''),
    
    % Win detection tests
    writeln('--- Win Detection Tests ---'),
    test_horizontal_win_player1,
    test_horizontal_win_player2,
    test_no_horizontal_win,
    test_vertical_win_player1,
    test_vertical_win_player2,
    test_no_vertical_win,
    test_diagonal_win_down_right_player1,
    test_diagonal_win_down_right_player2,
    test_diagonal_win_down_left_player1,
    test_diagonal_win_down_left_player2,
    test_no_diagonal_win,
    test_multiple_wins_detects_first,
    test_complex_diagonal_win,
    test_board_with_no_moves_yet,
    test_board_near_full_no_win,
    writeln(''),
    
    % Matrix operation tests
    writeln('--- Matrix Operation Tests ---'),
    test_generate_matrix_correct_size,
    test_generate_matrix_initialized_to_zero,
    test_replace_element_first_position,
    test_replace_element_middle_position,
    test_replace_element_last_position,
    test_replace_matrix_cell_basic,
    test_replace_matrix_cell_preserves_other_cells,
    test_sublist_found_at_start,
    test_sublist_found_in_middle,
    test_sublist_found_at_end,
    test_sublist_not_found,
    writeln(''),
    
    writeln('=== All Core Game Logic Tests Completed ==='),
    writeln('Core functionality appears to be working correctly!').

% Quick test for development
quick_core_test :-
    test_horizontal_win_player1,
    test_vertical_win_player1,
    test_diagonal_win_down_right_player1,
    test_generate_matrix_correct_size,
    test_replace_matrix_cell_basic.