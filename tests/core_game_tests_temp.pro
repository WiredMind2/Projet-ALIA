% Quick test for development
quick_core_test :-
    test_horizontal_win_player1,
    test_vertical_win_player1,
    test_diagonal_win_down_right_player1,
    test_generate_matrix_correct_size,
    test_replace_matrix_cell_basic,
    true.  % Ensure the predicate succeeds