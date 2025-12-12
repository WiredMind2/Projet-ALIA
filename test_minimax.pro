:- consult('puissance4.pro').

% Test predicates for minimax AI

% Test evaluate on empty board
test_evaluate_empty :-
    setup,
    board(Board),
    evaluate(Board, 'x', Score),
    Score = 0,  % Should be 0 for non-terminal
    write('Test evaluate empty board: PASSED'), nl.

% Test evaluate on winning board for 'x'
test_evaluate_win_x :-
    setup,
    % Manually set a winning board for 'x' (4 in a row)
    Board = [['x','x','x','x',0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0]],
    evaluate(Board, 'x', Score),
    Score = 10000,
    write('Test evaluate win for x: PASSED'), nl.

% Test evaluate on winning board for 'o'
test_evaluate_win_o :-
    setup,
    Board = [['o','o','o','o',0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0]],
    evaluate(Board, 'o', Score),
    Score = 10000,
    write('Test evaluate win for o: PASSED'), nl.

% Test minimax depth 0 on empty board
test_minimax_depth0_empty :-
    setup,
    board(Board),
    minimax(Board, 0, 'x', BestCol, Score),
    Score = 0,
    BestCol = -1,
    write('Test minimax depth 0 empty: PASSED'), nl.

% Test minimax depth 1 on empty board
test_minimax_depth1_empty :-
    setup,
    board(Board),
    minimax(Board, 1, 'x', BestCol, Score),
    integer(BestCol),
    BestCol >= 0, BestCol =< 6,
    write('Test minimax depth 1 empty: BestCol='), write(BestCol), write(', Score='), write(Score), nl.

% Test minimax on board with winning move
test_minimax_win :-
    setup,
    % Board where 'x' can win immediately
    Board = [['x','x','x',0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0]],
    retractall(last_index(_)),
    assert(last_index([1,1,1,0,0,0,0])),  % Update last_index to match the board
    minimax(Board, 1, 'x', BestCol, Score),  % Use depth 1
    Score > 9000,
    between(0, 6, BestCol),
    write('Test minimax win: PASSED, BestCol='), write(BestCol), write(', Score='), write(Score), nl.

% Test iaMinimax on empty board
test_ia_minimax_empty :-
    setup,
    board(Board),
    iaMinimax(Board, NewBoard, 'x'),
    % Check that a move was made
    Board \= NewBoard,
    write('Test iaMinimax empty: PASSED'), nl.

% Run all tests
run_tests :-
    test_evaluate_empty,
    test_evaluate_win_x,
    test_evaluate_win_o,
    test_minimax_depth0_empty,
    test_minimax_depth1_empty,
    test_minimax_win,
    test_ia_minimax_empty,
    write('All tests completed.'), nl.