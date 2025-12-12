% Comprehensive test suite for enhanced minimax AI
% Tests the new enhanced heuristic implementation

:- consult('puissance4.pro').
:- consult('ai/minimax/minimax.pro').
:- consult('ai/evaluation.pro').
:- consult('ai/evaluation_enhanced.pro').
:- consult('tests/test_minimax_comprehensive.pro').

% =============================================================================
% ENHANCED HEURISTIC TEST SUITE
% =============================================================================

% Test enhanced evaluation vs basic evaluation
test_enhanced_vs_basic :-
    setup,
    writeln('=== Testing Enhanced vs Basic Evaluation ==='),
    
    % Test case 1: Empty board
    board(EmptyBoard),
    evaluate_enhanced(EmptyBoard, 'x', EnhancedScore1),
    evaluate_legacy(EmptyBoard, 'x', BasicScore1),
    format('Empty Board - Enhanced: ~w, Basic: ~w~n', [EnhancedScore1, BasicScore1]),
    
    % Test case 2: Center piece
    playMove(EmptyBoard, 3, CenterBoard, 'x'),
    evaluate_enhanced(CenterBoard, 'x', EnhancedScore2),
    evaluate_legacy(CenterBoard, 'x', BasicScore2),
    format('Center Piece - Enhanced: ~w, Basic: ~w~n', [EnhancedScore2, BasicScore2]),
    
    % Test case 3: Three in a row pattern
    ThreeRowBoard = [['x','x','x',0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0]],
    evaluate_enhanced(ThreeRowBoard, 'x', EnhancedScore3),
    evaluate_legacy(ThreeRowBoard, 'x', BasicScore3),
    format('Three in Row - Enhanced: ~w, Basic: ~w~n', [EnhancedScore3, BasicScore3]),
    
    writeln('Enhanced vs Basic comparison completed').

% Test game phase detection
test_game_phase_detection :-
    setup,
    writeln('=== Testing Game Phase Detection ==='),
    
    % Empty board (opening)
    board(EmptyBoard),
    get_game_phase(EmptyBoard, OpeningPhase),
    format('Empty board phase: ~w~n', [OpeningPhase]),
    
    % Middlegame simulation
    playMove(EmptyBoard, 3, Board1, 'x'),
    playMove(Board1, 3, Board2, 'o'),
    playMove(Board2, 2, Board3, 'x'),
    playMove(Board3, 2, Board4, 'o'),
    playMove(Board4, 4, Board5, 'x'),
    playMove(Board5, 4, Board6, 'o'),
    playMove(Board6, 1, Board7, 'x'),
    playMove(Board7, 1, Board8, 'o'),
    get_game_phase(Board8, MiddlePhase),
    format('Middlegame board phase: ~w~n', [MiddlePhase]),
    
    % Endgame simulation (fill most of board)
    create_endgame_board(EndgameBoard),
    get_game_phase(EndgameBoard, EndgamePhase),
    format('Endgame board phase: ~w~n', [EndgamePhase]),
    
    writeln('Game phase detection test completed').

% Create a board that represents endgame state
create_endgame_board(Board) :-
    Board = [['x','o','x','o','x','o','x'],
             ['o','x','o','x','o','x','o'],
             ['x','o','x','o','x','o','x'],
             ['o','x','o','x','o','x','o'],
             ['x','o','x','o','x','o','x'],
             ['o','x','o','x','o','x','.']].

% Test pattern recognition
test_pattern_recognition :-
    setup,
    writeln('=== Testing Pattern Recognition ==='),
    
    % Test three in a row detection
    ThreeRowBoard = [['x','x','x',0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0],
                     [0,0,0,0,0,0,0]],
    count_three_in_row(ThreeRowBoard, 'x', ThreeCount),
    format('Three in row detection: ~w patterns found~n', [ThreeCount]),
    
    % Test immediate win detection
    ImmediateWinBoard = [['x','x','x',0,0,0,0],
                         [0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0]],
    count_immediate_wins(ImmediateWinBoard, 'x', WinCount),
    format('Immediate win detection: ~w opportunities found~n', [WinCount]),
    
    % Test strategic control
    StrategicBoard = [[0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0],
                      [0,0,0,'x',0,0,0],
                      [0,0,'x',0,0,0,0],
                      [0,'x',0,0,0,0,0],
                      [0,0,0,0,0,0,0]],
    evaluate_strategic_control(StrategicBoard, 'x', StrategicScore),
    format('Strategic control score: ~w~n', [StrategicScore]),
    
    writeln('Pattern recognition test completed').

% Test threat detection
test_threat_detection :-
    setup,
    writeln('=== Testing Threat Detection ==='),
    
    % Create board with opponent threat
    ThreatBoard = [['o','o','o',0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0],
                   [0,0,0,0,0,0,0]],
    
    evaluate_threats(ThreatBoard, 'x', ThreatScore),
    format('Threat detection score: ~w~n', [ThreatScore]),
    
    % Positive: board with own threats
    OwnThreatBoard = [['x','x','x',0,0,0,0],
                      [0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0],
                      [0,0,0,0,0,0,0]],
    
    evaluate_threats(OwnThreatBoard, 'x', OwnThreatScore),
    format('Own threat creation score: ~w~n', [OwnThreatScore]),
    
    writeln('Threat detection test completed').

% Test minimax AI with enhanced evaluation
test_minimax_enhanced_ai :-
    setup,
    writeln('=== Testing Enhanced Minimax AI ==='),
    
    % Test basic AI move generation
    board(EmptyBoard),
    minimax_ai(EmptyBoard, NewBoard, 'x'),
    format('AI made move on empty board~n'),
    
    % Test with some existing pieces
    playMove(EmptyBoard, 3, PartialBoard, 'x'),
    playMove(PartialBoard, 3, TestBoard, 'o'),
    minimax_ai(TestBoard, NewTestBoard, 'x'),
    format('AI made move on partial board~n'),
    
    % Test move quality analysis
    analyze_ai_move_quality(TestBoard, NewTestBoard, 'x'),
    
    writeln('Enhanced minimax AI test completed').

% Analyze the quality of AI moves
analyze_ai_move_quality(OldBoard, NewBoard, Player) :-
    % Count patterns before and after
    evaluate_patterns(OldBoard, Player, OldPatternScore),
    evaluate_patterns(NewBoard, Player, NewPatternScore),
    PatternImprovement is NewPatternScore - OldPatternScore,
    
    % Count threats before and after
    evaluate_threats(OldBoard, Player, OldThreatScore),
    evaluate_threats(NewBoard, Player, NewThreatScore),
    ThreatImprovement is NewThreatScore - OldThreatScore,
    
    format('Pattern score improvement: ~w~n', [PatternImprovement]),
    format('Threat score improvement: ~w~n', [ThreatImprovement]).

% =============================================================================
% PERFORMANCE COMPARISON TESTS
% =============================================================================

% Compare execution time between enhanced and basic evaluation
test_performance_comparison :-
    setup,
    writeln('=== Performance Comparison Test ==='),
    
    board(Board),
    
    % Test enhanced evaluation performance
    get_time(Start1),
    forall(between(1, 100, _), evaluate_enhanced(Board, 'x', _)),
    get_time(End1),
    EnhancedTime is End1 - Start1,
    
    % Test basic evaluation performance
    get_time(Start2),
    forall(between(1, 100, _), evaluate_legacy(Board, 'x', _)),
    get_time(End2),
    BasicTime is End2 - Start2,
    
    format('Enhanced evaluation time: ~6f seconds~n', [EnhancedTime]),
    format('Basic evaluation time: ~6f seconds~n', [BasicTime]),
    
    (EnhancedTime < BasicTime * 1.5 ->
        format('Performance: ACCEPTABLE (Enhanced is ~2fx slower)~n', [EnhancedTime/BasicTime])
    ; format('Performance: NEEDS OPTIMIZATION (Enhanced is ~2fx slower)~n', [EnhancedTime/BasicTime])),
    
    writeln('Performance comparison completed').

% =============================================================================
% COMPREHENSIVE TEST RUNNER
% =============================================================================

% Run all enhanced heuristic tests
run_enhanced_heuristic_tests :-
    writeln('Starting Enhanced Heuristic Test Suite...'),
    test_enhanced_vs_basic,
    test_game_phase_detection,
    test_pattern_recognition,
    test_threat_detection,
    test_minimax_enhanced_ai,
    test_performance_comparison,
    writeln('All enhanced heuristic tests completed!').

% Quick test for development
quick_enhanced_test :-
    setup,
    test_enhanced_vs_basic,
    writeln('Quick enhanced test completed!').

% Run all tests if this file is executed directly
:- run_enhanced_heuristic_tests, halt.