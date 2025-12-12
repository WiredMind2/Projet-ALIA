% Detailed debug test for minimax AI
:- consult('puissance4.pro').

% Debug the minimax step by step
debug_minimax :-
    writeln('=== DETAILED MINIMAX DEBUG ==='),
    
    % Setup
    setup,
    
    % Test with a simple board
    TestBoard = [['x',0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0],
                 [0,0,0,0,0,0,0]],
    
    writeln('Test board:'),
    print_board(TestBoard),
    
    % Test simulateMove
    format('~nTesting simulateMove...~n'),
    (   simulateMove(TestBoard, 1, SimulatedBoard, 'x')
    ->  writeln('SimulateMove succeeded:'),
        print_board(SimulatedBoard)
    ;   writeln('SimulateMove failed')
    ),
    
    % Test validMove specifically
    format('~nTesting validMove for each column...~n'),
    forall(between(0,6,Col), (
        (   validMove(Col)
        ->  format('Column ~w: VALID~n', [Col])
        ;   format('Column ~w: INVALID~n', [Col])
        )
    )),
    
    % Test minimax with a simpler approach
    format('~nTesting minimax step by step...~n'),
    
    % First check if we can get valid moves
    findall(C, validMove(C), ValidMoves),
    format('Valid moves found: ~w~n', [ValidMoves]),
    
    % Now try minimax with very small depth
    format('~nTesting minimax depth 1...~n'),
    (   catch(minimax(TestBoard, 1, 'x', BestCol, Score), Error, 
             format('Minimax error: ~w~n', [Error]))
    ->  format('Minimax result: BestCol=~w, Score=~w~n', [BestCol, Score])
    ;   format('Minimax failed silently~n', [])
    ),
    
    % Test with depth 0 (should be simple)
    format('~nTesting minimax depth 0...~n'),
    (   catch(minimax(TestBoard, 0, 'x', BestCol0, Score0), Error0, 
             format('Minimax depth 0 error: ~w~n', [Error0]))
    ->  format('Minimax depth 0 result: BestCol=~w, Score=~w~n', [BestCol0, Score0])
    ;   format('Minimax depth 0 failed silently~n', [])
    ),
    
    writeln('=== DEBUG COMPLETE ===').

:- debug_minimax, halt.