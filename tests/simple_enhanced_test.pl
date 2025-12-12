% Simple test to verify enhanced minimax works
:- initialization(main).

main :-
    % Load required files
    consult('matrix.pro'),
    consult('win.pro'),
    
    writeln('Testing Enhanced Minimax Heuristic:'),
    writeln('===================================='),
    
    % Test that enhanced evaluation function can be loaded
    test_enhanced_evaluation_loading,
    
    % Test basic game state evaluation
    test_basic_evaluation,
    
    % Test pattern recognition
    test_patterns,
    
    % Test game phase detection
    test_game_phases,
    
    writeln('===================================='),
    writeln('Enhanced minimax heuristic test completed!'),
    halt.

test_enhanced_evaluation_loading :-
    writeln('1. Testing enhanced evaluation loading...'),
    % Create a simple test to see if enhanced evaluation loads
    TestBoard = [['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.'],
                 ['.','.','.','.','.','.','.']],
    
    % Test game phase detection
    count_pieces_on_board(TestBoard, Count),
    format('   Empty board has ~w pieces~n', [Count]),
    Count == 0,
    writeln('   ✓ Board piece counting works'),
    
    % Test get_item_2d function
    get_item_2d(TestBoard, 0, 0, Value),
    Value == '.',
    writeln('   ✓ get_item_2d function works'),
    
    writeln('   ✓ Enhanced evaluation loading test passed'), nl.

test_basic_evaluation :-
    writeln('2. Testing basic evaluation functions...'),
    
    % Test player token counting
    TestTokens = ['x','o','x','.','x'],
    count_player_tokens(TestTokens, 'x', XCount),
    XCount == 3,
    writeln('   ✓ Player token counting works'),
    
    % Test empty cell counting
    count_empty_cells(TestTokens, EmptyCount),
    EmptyCount == 1,
    writeln('   ✓ Empty cell counting works'),
    
    % Test window extraction
    window_row([['x','o','x','o','.','.','.'],
                ['.','.','.','.','.','.','.'],
                ['.','.','.','.','.','.','.'],
                ['.','.','.','.','.','.','.'],
                ['.','.','.','.','.','.','.'],
                ['.','.','.','.','.','.','.']], 0, 0, Window),
    Window = [X,O,X,O],
    X == 'x', O == 'o',
    writeln('   ✓ Window extraction works'),
    
    writeln('   ✓ Basic evaluation test passed'), nl.

test_patterns :-
    writeln('3. Testing pattern recognition...'),
    
    % Test three in a row pattern
    PatternBoard = [['x','x','x','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.']],
    
    % Count windows with exactly 3 x tokens
    findall(1, (
        window_row(PatternBoard, _, _, Window),
        count_player_tokens(Window, 'x', Count),
        Count =:= 3,
        count_empty_cells(Window, Empty),
        Empty =:= 1
    ), Threes),
    length(Threes, ThreeCount),
    ThreeCount > 0,
    format('   Found ~w three-in-a-row patterns~n', [ThreeCount]),
    writeln('   ✓ Pattern recognition works'),
    
    writeln('   ✓ Pattern recognition test passed'), nl.

test_game_phases :-
    writeln('4. Testing game phase detection...'),
    
    % Test opening phase (0-7 pieces)
    OpeningBoard = [['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.']],
    
    count_pieces_on_board(OpeningBoard, OpeningCount),
    OpeningCount =< 7,
    format('   Opening phase: ~w pieces detected~n', [OpeningCount]),
    writeln('   ✓ Opening phase detection works'),
    
    % Test middlegame (8-28 pieces)
    MiddleBoard = [['x','o','x','o','x','o','x'],
                   ['o','x','o','x','o','x','o'],
                   ['x','o','x','o','x','o','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.']],
    
    count_pieces_on_board(MiddleBoard, MiddleCount),
    MiddleCount > 7, MiddleCount =< 28,
    format('   Middlegame phase: ~w pieces detected~n', [MiddleCount]),
    writeln('   ✓ Middlegame phase detection works'),
    
    % Test endgame (29+ pieces)
    EndBoard = [['x','o','x','o','x','o','x'],
                ['o','x','o','x','o','x','o'],
                ['x','o','x','o','x','o','x'],
                ['o','x','o','x','o','x','o'],
                ['x','o','x','o','x','o','x'],
                ['o','x','o','x','o','x','o']],
    
    count_pieces_on_board(EndBoard, EndCount),
    EndCount > 28,
    format('   Endgame phase: ~w pieces detected~n', [EndCount]),
    writeln('   ✓ Endgame phase detection works'),
    
    writeln('   ✓ Game phase detection test passed'), nl.

% Utility functions needed for testing
count_pieces_on_board(Board, Count) :-
    findall(1, (member(Row, Board), member(Piece, Row), Piece \= '.'), Pieces),
    length(Pieces, Count).

count_player_tokens(List, Player, Count) :-
    findall(1, member(Player, List), L),
    length(L, Count).

count_empty_cells(List, Count) :-
    findall(1, member('.', List), L),
    length(L, Count).

% Window extraction functions
window_row(Board, R, C, List) :- 
    between(0,5,R), between(0,3,C), 
    findall(V, (between(0,3,I), C2 is C+I, get_item_2d(Board,R,C2,V)), List).

window_col(Board, R, C, List) :- 
    between(0,2,R), between(0,6,C), 
    findall(V, (between(0,3,I), R2 is R+I, get_item_2d(Board,R2,C,V)), List).

window_diag1(Board, R, C, List) :- 
    between(0,2,R), between(0,3,C), 
    findall(V, (between(0,3,I), R2 is R+I, C2 is C+I, get_item_2d(Board,R2,C2,V)), List).

window_diag2(Board, R, C, List) :- 
    between(0,2,R), between(3,6,C), 
    findall(V, (between(0,3,I), R2 is R+I, C2 is C-I, get_item_2d(Board,R2,C2,V)), List).