% Diagnostic test for minimax AI
:- consult('puissance4.pro').

% Test the individual components
diagnostic_test :-
    writeln('=== DIAGNOSTIC TEST ==='),
    
    % Test setup
    setup,
    writeln('Setup completed'),
    
    % Test board state
    board(Board),
    writeln('Board state:'),
    print_board(Board),
    
    % Test valid moves
    findall(C, validMove(C), ValidMoves),
    format('Valid moves: ~w~n', [ValidMoves]),
    length(ValidMoves, Count),
    format('Number of valid moves: ~w~n', [Count]),
    
    % Test last_index state
    last_index(Indices),
    format('Last index state: ~w~n', [Indices]),
    
    % Test minimax with depth 1
    format('~nTesting minimax depth 1...~n'),
    (   minimax(Board, 1, 'x', BestCol, Score)
    ->  format('Minimax result: BestCol=~w, Score=~w~n', [BestCol, Score])
    ;   format('Minimax failed~n', [])
    ),
    
    % Test minimax_ai
    format('~nTesting minimax_ai...~n'),
    (   minimax_ai(Board, NewBoard, 'x')
    ->  writeln('minimax_ai succeeded'),
        print_board(NewBoard)
    ;   writeln('minimax_ai failed')
    ),
    
    % Test evaluate function
    format('~nTesting evaluate function...~n'),
    (   evaluate(Board, 'x', EvalScore)
    ->  format('Evaluation score: ~w~n', [EvalScore])
    ;   format('Evaluation failed~n', [])
    ),
    
    writeln('=== DIAGNOSTIC COMPLETE ===').

:- diagnostic_test, halt.