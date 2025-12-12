:- consult('puissance4.pro').
:- consult('ai/minimax/minimax.pro').
:- consult('ai/game_utils.pro').
:- consult('ai/evaluation.pro').

% =============================================================================
% COMPREHENSIVE MINIMAX AI TEST SUITE
% =============================================================================
% This test suite provides comprehensive coverage for the minimax AI implementation
% Tests are organized into categories for better maintainability and debugging

:- dynamic test_results/3.  % test_results(TestName, Category, Status)

% =============================================================================
% TEST RESULT MANAGEMENT
% =============================================================================

% Record test result
record_result(TestName, Category, Status) :-
    assertz(test_results(TestName, Category, Status)).

% Clear all test results
clear_results :-
    retractall(test_results(_,_,_)).

% Print test summary
print_summary :-
    findall((Cat, Status), test_results(_, Cat, Status), Results),
    sort(Results, UniqueResults),
    writeln('=== TEST SUMMARY ==='),
    forall(member((Cat, Status), UniqueResults), (
        findall(Test, test_results(Test, Cat, Status), Tests),
        length(Tests, Count),
        format('~w ~w: ~d tests~n', [Cat, Status, Count])
    )),
    % Count total tests
    findall(_, test_results(_,_,_), AllTests),
    length(AllTests, Total),
    format('Total tests run: ~d~n', [Total]).

% =============================================================================
% TEST HELPERS
% =============================================================================

% Test setup and teardown
test_setup :-
    setup.

test_teardown :-
    true.  % Currently no cleanup needed

% Assert helper for testing
assert_test(TestName, Category, Condition, Description) :-
    (   Condition
    ->  record_result(TestName, Category, 'PASS'),
        format('✓ ~w: ~w~n', [TestName, Description])
    ;   record_result(TestName, Category, 'FAIL'),
        format('✗ ~w: ~w~n', [TestName, Description]),
        fail
    ).

% Run test with setup/teardown
run_test(TestName, Category, SetupGoal, TestGoal, Description) :-
    test_setup,
    SetupGoal,
    assert_test(TestName, Category, TestGoal, Description),
    test_teardown.

% =============================================================================
% UNIT TESTS - GAME UTILITIES
% =============================================================================

% Test validMove function
test_validMove_unit :-
    test_setup,
    % Test valid moves on empty board
    assert_test('validMove_empty_board', 'unit', 
        (findall(C, validMove(C), Valid), length(Valid, 7)), 
        'Empty board should have 7 valid moves'),
    
    % Test with partially filled board
    retractall(last_index(_)),
    assert(last_index([2,3,0,4,5,1,6])),  % Column 2 is full
    assert_test('validMove_partial_board', 'unit',
        (findall(C, validMove(C), Valid), length(Valid, 6)),
        'Board with one full column should have 6 valid moves'),
    
    % Test with nearly full board
    retractall(last_index(_)),
    assert(last_index([6,6,6,6,6,6,5])),  % Only column 6 has space
    assert_test('validMove_nearly_full', 'unit',
        (findall(C, validMove(C), Valid), Valid = [6]),
        'Nearly full board should have only 1 valid move'),
    
    % Test completely full board
    retractall(last_index(_)),
    assert(last_index([6,6,6,6,6,6,6])),  % All columns full
    assert_test('validMove_full_board', 'unit',
        findall(C, validMove(C), Valid), Valid = [],
        'Full board should have no valid moves'),
    
    test_teardown.

% Test playMove function
test_playMove_unit :-
    test_setup,
    % Test normal move
    assert_test('playMove_normal', 'unit',
        (playMove([[0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0]], 0, NewBoard, 'x'),
         nth0(0, NewBoard, Row), nth0(0, Row, 'x')),
        'playMove should place piece in correct position'),
    
    % Test that last_index is updated
    assert_test('playMove_updates_index', 'unit',
        (retractall(last_index(_)),
         assert(last_index([0,0,0,0,0,0,0])),
         playMove([[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,0,0,0,0,0,0]], 0, _, 'x'),
         last_index([1|_])),
        'playMove should update last_index correctly'),
    
    test_teardown.

% Test changePlayer function
test_changePlayer_unit :-
    assert_test('changePlayer_x_to_o', 'unit', changePlayer('x', 'o'), 'x should change to o'),
    assert_test('changePlayer_o_to_x', 'unit', changePlayer('o', 'x'), 'o should change to x').

% =============================================================================
% UNIT TESTS - EVALUATION FUNCTIONS
% =============================================================================

% Test basic evaluation
test_evaluate_unit :-
    test_setup,
    % Test empty board evaluation
    assert_test('evaluate_empty_board', 'unit',
        (board(Board), evaluate(Board, 'x', Score), Score = 0),
        'Empty board should evaluate to 0'),
    
    % Test winning board for 'x'
    WinningBoardX = [['x','x','x','x',0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0]],
    assert_test('evaluate_win_x', 'unit',
        evaluate(WinningBoardX, 'x', 10000),
        'Winning board for x should score 10000'),
    
    % Test losing board for 'x' (opponent wins)
    WinningBoardO = [['o','o','o','o',0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0]],
    assert_test('evaluate_loss_x', 'unit',
        evaluate(WinningBoardO, 'x', -10000),
        'Losing board for x should score -10000'),
    
    % Test draw board - simplified test
    assert_test('evaluate_draw', 'unit',
        evaluate([[1,2,1,2,1,2,1],
                  [2,1,2,1,2,1,2],
                  [1,2,1,2,1,2,1],
                  [2,1,2,1,2,1,2],
                  [1,2,1,2,1,2,1],
                  [2,1,2,1,2,1,2]], 'x', 0),
        'Full board should evaluate to 0 (draw)'),
    
    test_teardown.

% =============================================================================
% INTEGRATION TESTS - MINIMAX ALGORITHM
% =============================================================================

% Test minimax with depth 0
test_minimax_depth0_integration :-
    test_setup,
    assert_test('minimax_depth0_empty', 'integration',
        (board(Board), minimax(Board, 0, 'x', BestCol, Score),
         Score = 0, BestCol = -1),
        'Minimax depth 0 should return score 0 and no move'),
    
    test_teardown.

% Test minimax with winning move available
test_minimax_winning_move_integration :-
    test_setup,
    % Board where 'x' can win immediately
    WinBoard = [['x','x','x',0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0]],
    
    assert_test('minimax_find_winning_move', 'integration',
        (minimax(WinBoard, 1, 'x', BestCol, Score),
         Score > 9000, between(0, 6, BestCol)),
        'Minimax should find winning move with high score'),
    
    test_teardown.

% Test minimax with blocking move needed
test_minimax_blocking_move_integration :-
    test_setup,
    % Board where 'x' must block 'o's win
    BlockBoard = [['o','o','o',0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0],
                  [0,0,0,0,0,0,0]],
    
    assert_test('minimax_block_opponent_win', 'integration',
        (minimax(BlockBoard, 1, 'x', BestCol, Score),
         Score > 5000, BestCol = 3),
        'Minimax should block opponent winning move'),
    
    test_teardown.

% Test minimax_ai main function
test_minimax_ai_integration :-
    test_setup,
    assert_test('minimax_ai_empty_board', 'integration',
        (board(Board), minimax_ai(Board, NewBoard, 'x'),
         Board \= NewBoard),
        'minimax_ai should make a move on empty board'),
    
    % Test that it returns a valid board state
    assert_test('minimax_ai_valid_move', 'integration',
        (board(Board), minimax_ai(Board, NewBoard, 'x'),
         count_pieces(NewBoard, 'x', Count), Count = 1),
        'minimax_ai should place exactly one piece'),
    
    test_teardown.

% =============================================================================
% EDGE CASE TESTS
% =============================================================================

% Test with full board (should handle gracefully)
test_full_board_edge_case :-
    test_setup,
    % Create a full board
    FullBoard = [[1,2,1,2,1,2,1],
                 [2,1,2,1,2,1,2],
                 [1,2,1,2,1,2,1],
                 [2,1,2,1,2,1,2],
                 [1,2,1,2,1,2,1],
                 [2,1,2,1,2,1,2]],
    
    assert_test('minimax_full_board', 'edge_case',
        catch(minimax(FullBoard, 2, 'x', _, _), _, fail),
        'Minimax should handle full board without crashing'),
    
    test_teardown.

% Test with invalid depth
test_invalid_depth_edge_case :-
    test_setup,
    board(Board),
    assert_test('minimax_negative_depth', 'edge_case',
        (minimax(Board, -1, 'x', BestCol, Score), Score = 0, BestCol = -1),
        'Minimax with negative depth should return base case'),
    
    test_teardown.

% Test with invalid player
test_invalid_player_edge_case :-
    test_setup,
    board(Board),
    assert_test('minimax_invalid_player', 'edge_case',
        catch(minimax(Board, 1, 'z', _, _), _, fail),
        'Minimax should handle invalid player gracefully'),
    
    test_teardown.

% =============================================================================
% PERFORMANCE TESTS
% =============================================================================

% Test timeout handling
test_timeout_performance :-
    test_setup,
    % Create a complex board for performance testing
    ComplexBoard = [[0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0],
                    [1,2,1,2,1,2,1]],
    
    assert_test('minimax_timeout_handling', 'performance',
        catch(call_with_time_limit(0.1, 
            minimax(ComplexBoard, 5, 'x', _, _)), 
            time_limit_exceeded, true),
        'Minimax should respect timeout limits'),
    
    test_teardown.

% Test execution time for different depths
test_execution_time_performance :-
    test_setup,
    board(Board),
    
    % Test depth 1
    time_test(1, 'depth_1'),
    % Test depth 2
    time_test(2, 'depth_2'),
    % Test depth 3
    time_test(3, 'depth_3'),
    
    test_teardown.

% Helper for timing tests
time_test(Depth, TestName) :-
    catch(call_with_time_limit(5, (
        board(_Board),
        minimax(_Board, Depth, 'x', _, _)
    )), time_limit_exceeded, (
        record_result(TestName, 'performance', 'TIMEOUT'),
        format('⚠ ~w: TIMEOUT (>5s)~n', [TestName])
    )).

% =============================================================================
% HEURISTIC ACCURACY TESTS
% =============================================================================

% Test center positioning heuristic
test_center_positioning_heuristic :-
    test_setup,
    % Board with center piece
    CenterBoard = [[0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,'x',0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0]],
    
    assert_test('center_position_bonus', 'heuristic',
        evaluate(CenterBoard, 'x', Score), Score > 0),
    
    % Board with edge piece
    EdgeBoard = [[0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 ['x',0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0]],
    
    assert_test('edge_position_lower', 'heuristic',
        evaluate(EdgeBoard, 'x', EdgeScore),
        evaluate(CenterBoard, 'x', CenterScore),
        CenterScore > EdgeScore),
    
    test_teardown.

% Test pattern recognition heuristics
test_pattern_heuristics :-
    test_setup,
    
    % Test three in a row pattern
    ThreeRowBoard = [['x','x','x',0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0]],
    
    assert_test('three_in_row_pattern', 'heuristic',
        evaluate(ThreeRowBoard, 'x', Score), Score > 10),
    
    test_teardown.

% =============================================================================
% ERROR HANDLING TESTS
% =============================================================================

% Test with invalid board dimensions
test_invalid_board_error_handling :-
    test_setup,
    InvalidBoard = [[1,2,3],[4,5,6]],  % Wrong dimensions
    
    assert_test('minimax_invalid_board', 'error_handling',
        catch(minimax(InvalidBoard, 1, 'x', _, _), _, fail),
        'Minimax should handle invalid board gracefully'),
    
    test_teardown.

% Test with non-list board
test_non_list_board_error_handling :-
    assert_test('minimax_non_list_board', 'error_handling',
        catch(minimax("not_a_board", 1, 'x', _, _), _, fail),
        'Minimax should handle non-list board gracefully').

% =============================================================================
% HELPER PREDICATES
% =============================================================================

% Count pieces of a specific player on the board
count_pieces(Board, Player, Count) :-
    findall(1, (member(Row, Board), member(Player, Row)), Pieces),
    length(Pieces, Count).

% =============================================================================
% TEST RUNNERS
% =============================================================================

% Run all unit tests
run_unit_tests :-
    writeln('=== RUNNING UNIT TESTS ==='),
    test_validMove_unit,
    test_playMove_unit,
    test_changePlayer_unit,
    test_evaluate_unit.

% Run all integration tests
run_integration_tests :-
    writeln('=== RUNNING INTEGRATION TESTS ==='),
    test_minimax_depth0_integration,
    test_minimax_winning_move_integration,
    test_minimax_blocking_move_integration,
    test_minimax_ai_integration.

% Run all edge case tests
run_edge_case_tests :-
    writeln('=== RUNNING EDGE CASE TESTS ==='),
    test_full_board_edge_case,
    test_invalid_depth_edge_case,
    test_invalid_player_edge_case.

% Run all performance tests
run_performance_tests :-
    writeln('=== RUNNING PERFORMANCE TESTS ==='),
    test_timeout_performance,
    test_execution_time_performance.

% Run all heuristic tests
run_heuristic_tests :-
    writeln('=== RUNNING HEURISTIC TESTS ==='),
    test_center_positioning_heuristic,
    test_pattern_heuristics.

% Run all error handling tests
run_error_handling_tests :-
    writeln('=== RUNNING ERROR HANDLING TESTS ==='),
    test_invalid_board_error_handling,
    test_non_list_board_error_handling.

% Run all tests
run_all_tests :-
    clear_results,
    run_unit_tests,
    run_integration_tests,
    run_edge_case_tests,
    run_performance_tests,
    run_heuristic_tests,
    run_error_handling_tests,
    print_summary.

% Quick test suite for development
quick_test :-
    clear_results,
    test_evaluate_unit,
    test_minimax_depth0_integration,
    test_minimax_ai_integration,
    print_summary.