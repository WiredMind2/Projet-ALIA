% Heuristic evaluation functions for Connect Four AI
% This module provides two different evaluation strategies for the minimax algorithm
% Ported from JavaScript versions

% Position evaluation table: tableauEvaluation[Col][Row]
% Strategic position values - center columns and middle rows are more valuable
tableau_evaluation([
    [3, 4, 5, 5, 4, 3],     % Column 0 (leftmost)
    [4, 6, 8, 8, 6, 4],     % Column 1
    [5, 8, 11, 11, 8, 5],   % Column 2
    [7, 10, 13, 13, 10, 7], % Column 3 (center - most valuable)
    [5, 8, 11, 11, 8, 5],   % Column 4
    [4, 6, 8, 8, 6, 4],     % Column 5
    [3, 4, 5, 5, 4, 3]      % Column 6 (rightmost)
]).

% heuristique1(Joueur, Board, Evaluation)
% Returns evaluation based on strategic position values
% This heuristic favors moves that control the center of the board
% Joueur: player number (1 or 2) to evaluate for
% Board: current game board
% Evaluation: numerical score (higher is better for the player)
heuristique1(Joueur, Board, Evaluation) :-
    joueur_adverse(Joueur, Adversaire),          % Get opponent player number
    tableau_evaluation(Table),                   % Get position evaluation table
    evaluate_positions(Board, Table, Joueur, Adversaire, 0, 0, 0, Evaluation).

% Helper to evaluate all positions on the board
% Accumulates position values for both players and returns the difference
evaluate_positions(_, _, _, _, 7, _, Eval, Eval).  % Base case: processed all 7 columns
evaluate_positions(Board, Table, Joueur, Adversaire, Col, 6, Acc, Evaluation) :-
    NextCol is Col + 1,                          % Move to next column
    evaluate_positions(Board, Table, Joueur, Adversaire, NextCol, 0, Acc, Evaluation).
evaluate_positions(Board, Table, Joueur, Adversaire, Col, Row, Acc, Evaluation) :-
    nth0(Col, Board, Column),                    % Get current column from board
    nth0(Row, Column, Cell),                     % Get current cell value
    nth0(Col, Table, ColValues),                 % Get position values for this column
    nth0(Row, ColValues, Value),                 % Get position value for this cell
    % Add value for player's pieces, subtract for opponent's pieces
    (Cell == Joueur -> NewAcc is Acc + Value ;
     Cell == Adversaire -> NewAcc is Acc - Value ;
     NewAcc = Acc),                              % Empty cell, no change
    NextRow is Row + 1,                          % Move to next row
    evaluate_positions(Board, Table, Joueur, Adversaire, Col, NextRow, NewAcc, Evaluation).

% heuristique2(Joueur, Board, Evaluation)
% Returns evaluation based on alignment patterns (2s and 3s)
% This heuristic counts potential winning alignments and gives strategic bonuses
% Joueur: player number (1 or 2) to evaluate for
% Board: current game board
% Evaluation: numerical score (higher is better for the player)
heuristique2(Joueur, Board, Evaluation) :-
    joueur_adverse(Joueur, Adversaire),          % Get opponent player number
    % Count 3-piece alignments (more valuable)
    nombre_alignement(3, Joueur, Board, Align3J), % Player's 3-in-a-row opportunities
    nombre_alignement(3, Adversaire, Board, Align3A), % Opponent's 3-in-a-row threats
    % Count 2-piece alignments (less valuable)
    nombre_alignement(2, Joueur, Board, Align2J), % Player's 2-in-a-row opportunities  
    nombre_alignement(2, Adversaire, Board, Align2A), % Opponent's 2-in-a-row threats
    % Calculate final evaluation with strategic bias
    Evaluation is 128 + Align3J * 3 - Align3A * 3 + Align2J * 1 - Align2A * 1.

% joueur_adverse(Joueur, Adversaire)
% Maps a player to their opponent
% Joueur: player number (1 or 2)
% Adversaire: returns the opposite player number
joueur_adverse(1, 2).
joueur_adverse(2, 1).

% in_bounds(Col, Row)
% Checks if coordinates are within board boundaries
% Col: column index (0-6 for Connect Four)
% Row: row index (0-5 for Connect Four)
in_bounds(Col, Row) :- between(0, 6, Col), between(0, 5, Row).

% nombre_alignement(Align, Joueur, Board, Total)
% Counts the total number of alignments of a given length
% Align: length of alignment to count (2 or 3)
% Joueur: player number whose alignments to count
% Board: current game board
% Total: total count of found alignments
nombre_alignement(Align, Joueur, Board, Total) :-
    % Vertical alignments from each column top
    findall(C, (between(0, 6, Col), 
                nombre_cases_alignes(Col, 0, 0, 1, Joueur, Align, Board, C)), VC),
    sum_list(VC, VSum),
    % Diagonal left (\) from each column top
    findall(C, (between(0, 6, Col), 
                nombre_cases_alignes(Col, 0, 1, 1, Joueur, Align, Board, C)), DGC),
    sum_list(DGC, DGSum),
    % Diagonal right (/) from each column top
    findall(C, (between(0, 6, Col), 
                nombre_cases_alignes(Col, 0, -1, 1, Joueur, Align, Board, C)), DDC),
    sum_list(DDC, DDSum),
    % Horizontal alignments from each row left
    findall(C, (between(0, 5, Row), 
                nombre_cases_alignes(0, Row, 1, 0, Joueur, Align, Board, C)), HC),
    sum_list(HC, HSum),
    % Horizontal diagonal left from each row left
    findall(C, (between(0, 5, Row), 
                nombre_cases_alignes(0, Row, 1, 1, Joueur, Align, Board, C)), HDGC),
    sum_list(HDGC, HDGSum),
    % Horizontal diagonal right from each row right
    findall(C, (between(0, 5, Row), 
                nombre_cases_alignes(6, Row, -1, 1, Joueur, Align, Board, C)), HDDC),
    sum_list(HDDC, HDDSum),
    % Sum all alignment directions
    Total is VSum + DGSum + DDSum + HSum + HDGSum + HDDSum.

% nombre_cases_alignes(StartCol, StartRow, DCol, DRow, Joueur, Align, Board, Count)
% Counts aligned cases starting from a given position in a specific direction
% StartCol, StartRow: starting position
% DCol, DRow: direction vector (e.g., 1,1 for diagonal down-right)
% Joueur: player number to count alignments for
% Align: desired alignment length (2 or 3)
% Board: current game board
% Count: number of valid alignments found
nombre_cases_alignes(StartCol, StartRow, DCol, DRow, Joueur, Align, Board, Count) :-
    traverse(StartCol, StartRow, DCol, DRow, Joueur, Align, Board, -1, -1, 0, 0, Count).

% traverse(Col, Row, DCol, DRow, Joueur, Align, Board, DebutCol, DebutRow, NombreCases, Acc, Final)
% Traverses the board in a given direction, counting alignments
% Col, Row: current position
% DCol, DRow: direction to move
% Joueur: player to count
% Align: desired alignment length
% Board: game board
% DebutCol, DebutRow: start position of current alignment
% NombreCases: number of consecutive pieces found so far
% Acc: accumulated count of valid alignments
% Final: final result
traverse(Col, Row, _, _, _, _, _, _, _, _, Acc, Acc) :- 
    \+ in_bounds(Col, Row).                      % Stop if out of bounds
traverse(Col, Row, DCol, DRow, Joueur, Align, Board, DebutCol, DebutRow, NombreCases, Acc, Final) :-
    nth0(Col, Board, Column),                    % Get current column
    nth0(Row, Column, Cell),                     % Get current cell
    (Cell == Joueur ->
        % Found player's piece - continue or complete alignment
        (NombreCases == 0 -> 
            NewDebutCol = Col, NewDebutRow = Row ; % Start new alignment
            NewDebutCol = DebutCol, NewDebutRow = DebutRow), % Continue existing
        NewNombreCases is NombreCases + 1,       % Increment counter
        (NewNombreCases == Align ->
            % Found required alignment - check if extendable (not blocked)
            BeforeCol is NewDebutCol - DCol, BeforeRow is NewDebutRow - DRow,
            AfterCol is Col + DCol, AfterRow is Row + DRow,
            (check_extendable(BeforeCol, BeforeRow, AfterCol, AfterRow, Joueur, Board) -> 
                NewAcc is Acc + 1 ;              % Valid alignment found
                NewAcc = Acc)                    % Alignment blocked, don't count
        ; NewAcc = Acc)                         % Not yet enough pieces
    ; 
        % Not player's piece - reset alignment counter
        NewNombreCases = 0, NewAcc = Acc, 
        NewDebutCol = -1, NewDebutRow = -1),
    NextCol is Col + DCol, NextRow is Row + DRow, % Move to next position
    traverse(NextCol, NextRow, DCol, DRow, Joueur, Align, Board, 
             NewDebutCol, NewDebutRow, NewNombreCases, NewAcc, Final).

% check_extendable(BeforeCol, BeforeRow, AfterCol, AfterRow, Joueur, Board)
% Checks if an alignment can be extended (not blocked by opponent)
% BeforeCol, BeforeRow: position before the alignment
% AfterCol, AfterRow: position after the alignment  
% Joueur: player who owns the alignment
% Board: current game board
% Returns true if alignment can be extended (has space to grow)
check_extendable(BeforeCol, BeforeRow, AfterCol, AfterRow, Joueur, Board) :-
    % Check if at least one end is open and not blocked by opponent
    ((in_bounds(BeforeCol, BeforeRow), 
      nth0(BeforeCol, Board, BCol), 
      nth0(BeforeRow, BCol, BCell), 
      BCell == 0) ;                              % Before end is empty
     (in_bounds(AfterCol, AfterRow), 
      nth0(AfterCol, Board, ACol), 
      nth0(AfterRow, ACol, ACell), 
      ACell == 0)),                              % After end is empty
    % Ensure neither end is blocked by opponent
    \+ (in_bounds(BeforeCol, BeforeRow), 
        nth0(BeforeCol, Board, BCol2), 
        nth0(BeforeRow, BCol2, BCell2), 
        BCell2 == Joueur),
    \+ (in_bounds(AfterCol, AfterRow), 
        nth0(AfterCol, Board, ACol2), 
        nth0(AfterRow, ACol2, ACell2), 
        ACell2 == Joueur).