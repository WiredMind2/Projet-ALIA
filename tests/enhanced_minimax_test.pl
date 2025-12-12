% Standalone test for enhanced minimax heuristic - No circular dependencies
:- initialization(main).

main :-
    % Load basic required files
    consult('matrix.pro'),
    consult('win.pro'),
    
    writeln('Testing Enhanced Minimax Heuristic:'),
    writeln('===================================='),
    
    % Test basic connectivity
    test_basic_functionality,
    
    % Test pattern recognition
    test_pattern_recognition,
    
    % Test threat detection
    test_threat_detection,
    
    % Test game phase detection
    test_game_phase_detection,
    
    % Test strategic control
    test_strategic_control,
    
    writeln('===================================='),
    writeln('All enhanced heuristic tests passed!'),
    halt.

% Test basic functionality
test_basic_functionality :-
    writeln('1. Testing basic board operations...'),
    % Test get_item_2d
    get_item_2d([['x','o','.'],
                 ['.','.','.'],
                 ['.','.','.']], 0, 0, 'x'),
    writeln('   ✓ get_item_2d works'),
    
    % Test window extraction
    window_row([['x','o','.','.','.'],
                ['.','.','.','.','.'],
                ['.','.','.','.','.']], 0, 0, [X,O,'.','.']),
    X == 'x', O == 'o',
    writeln('   ✓ window_row extraction works'),
    
    % Test piece counting
    count_player_tokens(['x','o','x','.'], 'x', Count),
    Count == 2,
    writeln('   ✓ piece counting works'),
    
    % Test empty cell counting
    count_empty_cells(['x','o','x','.'], EmptyCount),
    EmptyCount == 1,
    writeln('   ✓ empty cell counting works'),
    
    writeln('   ✓ Basic functionality test passed'), nl.

% Test pattern recognition
test_pattern_recognition(Board) :-
    writeln('2. Testing pattern recognition...'),
    
    % Create board with three in a row pattern
    PatternBoard = [['x','x','x','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.'],
                    ['.','.','.','.','.','.','.']],
    
    % Test three in a row counting
    count_three_in_row(PatternBoard, 'x', ThreeCount),
    format('   Found ~w three-in-a-row patterns~n', [ThreeCount]),
    ThreeCount > 0,
    writeln('   ✓ Three-in-a-row detection works'),
    
    % Test two in a row counting
    TwoInRowBoard = [['x','x','.','.','.','.','.'],
                     ['.','.','.','.','.','.','.'],
                     ['.','.','.','.','.','.','.'],
                     ['.','.','.','.','.','.','.'],
                     ['.','.','.','.','.','.','.'],
                     ['.','.','.','.','.','.','.']],
    
    count_two_in_row(TwoInRowBoard, 'x', TwoCount),
    format('   Found ~w two-in-a-row patterns~n', [TwoCount]),
    TwoCount > 0,
    writeln('   ✓ Two-in-a-row detection works'),
    
    writeln('   ✓ Pattern recognition test passed'), nl.

% Test threat detection
test_threat_detection(Board) :-
    writeln('3. Testing threat detection...'),
    
    % Create board with immediate threat
    ThreatBoard = [['x','x','x','.','.','.','.'],
                   ['o','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.']],
    
    count_immediate_wins(ThreatBoard, 'x', ThreatCount),
    format('   Found ~w immediate winning threats~n', [ThreatCount]),
    ThreatCount > 0,
    writeln('   ✓ Threat detection works'),
    
    writeln('   ✓ Threat detection test passed'), nl.

% Test game phase detection
test_game_phase_detection(Board) :-
    writeln('4. Testing game phase detection...'),
    
    % Test different game phases
    EmptyBoard = [['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.'],
                  ['.','.','.','.','.','.','.']],
    
    get_game_phase(EmptyBoard, Phase1),
    format('   Empty board phase: ~w~n', [Phase1]),
    Phase1 == 'opening',
    writeln('   ✓ Opening phase detection works'),
    
    % Test middlegame (15 pieces)
    MidBoard = [['x','o','x','o','x','o','x'],
                ['o','x','o','x','o','x','o'],
                ['x','o','x','o','x','o','.'],
                ['.','.','.','.','.','.','.'],
                ['.','.','.','.','.','.','.'],
                ['.','.','.','.','.','.','.']],
    
    get_game_phase(MidBoard, Phase2),
    format('   Mid-game board phase: ~w~n', [Phase2]),
    Phase2 == 'middlegame',
    writeln('   ✓ Middlegame phase detection works'),
    
    % Test endgame (35 pieces)
    EndBoard = [['x','o','x','o','x','o','x'],
                ['o','x','o','x','o','x','o'],
                ['x','o','x','o','x','o','x'],
                ['o','x','o','x','o','x','o'],
                ['x','o','x','o','x','o','x'],
                ['o','x','o','x','o','x','o']],
    
    get_game_phase(EndBoard, Phase3),
    format('   Endgame board phase: ~w~n', [Phase3]),
    Phase3 == 'endgame',
    writeln('   ✓ Endgame phase detection works'),
    
    writeln('   ✓ Game phase detection test passed'), nl.

% Test strategic control
test_strategic_control(Board) :-
    writeln('5. Testing strategic position control...'),
    
    % Test center control
    CenterBoard = [['.','.','x','.','.','.','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.'],
                   ['.','.','.','.','.','.','.']],
    
    evaluate_center_control(CenterBoard, 'x', CenterScore),
    format('   Center control score: ~w~n', [CenterScore]),
    CenterScore > 0,
    writeln('   ✓ Center control evaluation works'),
    
    % Test diagonal control
    DiagonalBoard = [['x','.','.','.','.','.','.'],
                     ['.','x','.','.','.','.','.'],
                     ['.','.','x','.','.','.','.'],
                     ['.','.','.','.','.','.','.'],
                     ['.','.','.','.','.','.','.'],
                     ['.','.','.','.','.','.','.']],
    
    evaluate_diagonal_control(DiagonalBoard, 'x', DiagonalScore),
    format('   Diagonal control score: ~w~n', [DiagonalScore]),
    DiagonalScore > 0,
    writeln('   ✓ Diagonal control evaluation works'),
    
    writeln('   ✓ Strategic control test passed'), nl.

% Enhanced evaluation functions (simplified versions for testing)
get_game_phase(Board, Phase) :-
    count_pieces_on_board(Board, TotalPieces),
    (TotalPieces =< 7 -> Phase = 'opening'
    ; TotalPieces =< 28 -> Phase = 'middlegame'
    ; Phase = 'endgame').

count_pieces_on_board(Board, Count) :-
    findall(1, (member(Row, Board), member(Piece, Row), Piece \= '.'), Pieces),
    length(Pieces, Count).

count_three_in_row(Board, Player, Count) :-
    findall(1, (
        window_with_exactly_n_tokens(Board, 3, Player),
        \+ window_with_n_tokens(Board, 4, Player)
    ), Threes),
    length(Threes, Count).

count_two_in_row(Board, Player, Count) :-
    findall(1, (
        window_with_exactly_n_tokens(Board, 2, Player),
        \+ window_with_n_tokens(Board, 3, Player),
        \+ window_with_n_tokens(Board, 4, Player)
    ), Twos),
    length(Twos, Count).

count_immediate_wins(Board, Player, Count) :-
    findall(1, (
        (window_row(Board, _, _, Window)
        ; window_col(Board, _, _, Window)
        ; window_diag1(Board, _, _, Window)
        ; window_diag2(Board, _, _, Window)),
        count_player_tokens(Window, Player, PlayerCount),
        count_empty_cells(Window, EmptyCount),
        PlayerCount =:= 3,
        EmptyCount =:= 1
    ), Wins),
    length(Wins, Count).

evaluate_center_control(Board, Player, Score) :-
    findall(Weight, (
        between(0, 6, Col),
        (Col = 3 -> Weight = 4
        ; (Col = 2; Col = 4) -> Weight = 3
        ; (Col = 1; Col = 5) -> Weight = 2
        ; Weight = 1),
        count_column_tokens(Board, Col, Player, Count),
        ColumnScore is Weight * Count
    ), Scores),
    sum_list(Scores, Score).

evaluate_diagonal_control(Board, Player, Score) :-
    findall(Score, (
        between(0, 5, Row),
        between(0, 6, Col),
        get_cell(Board, Row, Col, Player),
        diagonal_key_position(Row, Col, Weight),
        Score is Weight
    ), Scores),
    sum_list(Scores, Score).

diagonal_key_position(Row, Col, Weight) :-
    ((Row =:= Col, Row >= 2, Row =< 4) -> Weight = 2
    ; (Row + Col =:= 6, Row >= 2, Row =< 4) -> Weight = 2
    ; Weight = 1).

window_with_exactly_n_tokens(Board, N, Player) :-
    (window_row(Board, _, _, Window)
    ; window_col(Board, _, _, Window)
    ; window_diag1(Board, _, _, Window)
    ; window_diag2(Board, _, _, Window)),
    count_player_tokens(Window, Player, Count),
    Count = N.

window_with_n_tokens(Board, N, Player) :-
    (window_row(Board, _, _, Window)
    ; window_col(Board, _, _, Window)
    ; window_diag1(Board, _, _, Window)
    ; window_diag2(Board, _, _, Window)),
    count_player_tokens(Window, Player, Count),
    Count >= N.

count_column_tokens(Board, Col, Player, Count) :-
    findall(1, (
        between(0, 5, Row),
        get_cell(Board, Row, Col, Player)
    ), Tokens),
    length(Tokens, Count).