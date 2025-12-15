% Tic-Tac-Toe (Morpion) game implementation
% This module implements a complete Tic-Tac-Toe game with AI functionality
% Note: This appears to be a separate game from Connect Four, using a simpler 3x3 board

:- dynamic board/1.                               % Allows assertion and retraction of board/1 facts

% play(Player)
% Main game loop for Tic-Tac-Toe
% Handles a complete turn including display, move validation, and win checking
% Player: current player ('x' or 'o')
play(Player) :-
    write('New turn for:'), writeln(Player),     % Display current player
    board(Board),                                % Get current board state
    displayBoard,                                % Display the board
    
    (game_over(Board) ->                         % Check if game ended
        ((winner(Board, Winner) ->               % Someone won
            write('Player '), write(Winner), writeln(' wins!')) ;
            writeln('It\'s a draw!'))            % It's a draw
    );
    % Game continues - get AI move
    ia(Board, Move, Player),                     % Get AI's move recommendation
    
    playMove(Board, Move, NewBoard, Player),     % Apply the move
    applyIt(Board, NewBoard),                    % Update board state
    
    changePlayer(Player, NextPlayer),            % Switch to other player
    play(NextPlayer).                            % Continue with next player

% init
% Initialize and start the Tic-Tac-Toe game
% Sets up empty board and starts the game with player 'x'
init :- 
    length(Board,9),                             % Create 9-element empty board
    reset(Board),                                % Fill board with zeros
    assert(board(Board)),                        % Save board state
    play('x').                                   % Start with player 'x'

% reset(Board)
% Recursively reset board elements to zero (empty)
% Board: list to be reset
reset([]).                                       % Base case: empty list
reset([HA|A]) :-                                 % Recursive case
    HA = 0,                                     % Set current element to 0 (empty)
    reset(A).                                   % Recursively reset rest

% displayBoard
% Displays the current Tic-Tac-Toe board in a readable format
% Shows the board as a 3x3 grid with numbered positions
displayBoard :-
    board(Board),                               % Get current board state
    % Display first row
    nth0(0,Board,A), nth0(1,Board,B), nth0(2,Board,C),
    format('~w ~w ~w~n', [A,B,C]),
    % Display second row
    nth0(3,Board,D), nth0(4,Board,E), nth0(5,Board,F),
    format('~w ~w ~w~n', [D,E,F]),
    % Display third row
    nth0(6,Board,G), nth0(7,Board,H), nth0(8,Board,I),
    format('~w ~w ~w~n', [G,H,I]).

% playMove(Board, Move, NewBoard, Player)
% Applies a move to the Tic-Tac-Toe board
% Board: current board state
% Move: position index (0-8) where to place the piece
% NewBoard: resulting board after the move
% Player: player symbol ('x' or 'o') making the move
playMove(Board, Move, NewBoard, Player) :-
    nth0(Move, Board, 0, Rest),                % Check position is empty (0) and get rest of board
    length(Prefix, Move),                      % Prefix is first Move elements
    append(Prefix, Suffix, Rest),              % Split rest into prefix and suffix
    append(Prefix, [Player|Suffix], NewBoard). % Insert Player at position Move

% ia(Board, Move, Player)
% AI function that determines the move for a player
% Currently delegates to human player input (placeholder for AI implementation)
% Board: current board state
% Move: returns recommended move position
% Player: player symbol ('x' or 'o') to make move for
ia(Board, Move, Player) :-
    human(Board, Player, Move).                % Currently uses human input - could be replaced with AI

% human(Board, Player, Move)
% Handles human player input for Tic-Tac-Toe
% Provides interactive input for position selection with validation
% Board: current board state
% Player: player symbol ('x' or 'o')
% Move: returns selected position (1-9)
human(Board, Player, Move) :-
    repeat,                                     % Loop until valid input received
    write('Your turn: '),                      % Prompt for input
    read_string(user_input, "\n", "", _, Str), % Read input as string
    number_string(Input, Str),                 % Convert to number
    Pos is Input - 1,                          % Convert to 0-based index
    write('You played '),
    writeln(Input),                            % Echo the move
    % Validate move: position must be in bounds and empty
    (Pos < 0 ; Pos > 8 ; \+ nth0(Pos, Board, 0)) ->
        writeln('Invalid move'), fail ;        % Invalid move, try again
        Move = Pos,                            % Valid move found
        true.                                  % Success

% applyIt(Board, NewBoard)
% Updates the board state in the global database
% OldBoard: current board (ignored, just for pattern matching)
% NewBoard: new board state to save
applyIt(Board, NewBoard) :-
    retract(board(Board)),                     % Remove old board state
    assert(board(NewBoard)).                   % Save new board state

% changePlayer(Current, Next)
% Switches between players in Tic-Tac-Toe
% Current: current player symbol ('x' or 'o')
% Next: returns the opposite player symbol
changePlayer('x', 'o').                         % Player 'x' becomes player 'o'
changePlayer('o', 'x').                         % Player 'o' becomes player 'x'

% winner(Board, Player)
% Checks if the specified player has won the game
% Returns true if the player has 3 in a row (horizontal, vertical, or diagonal)
% Board: current board state
% Player: player symbol ('x' or 'o') to check for victory
winner(Board, Player) :-
    % Check all possible winning lines:
    % Horizontal rows
    (nth0(0,Board,Player), nth0(1,Board,Player), nth0(2,Board,Player));
    (nth0(3,Board,Player), nth0(4,Board,Player), nth0(5,Board,Player));
    (nth0(6,Board,Player), nth0(7,Board,Player), nth0(8,Board,Player));
    % Vertical columns
    (nth0(0,Board,Player), nth0(3,Board,Player), nth0(6,Board,Player));
    (nth0(1,Board,Player), nth0(4,Board,Player), nth0(7,Board,Player));
    (nth0(2,Board,Player), nth0(5,Board,Player), nth0(8,Board,Player));
    % Diagonals
    (nth0(0,Board,Player), nth0(4,Board,Player), nth0(8,Board,Player));
    (nth0(2,Board,Player), nth0(4,Board,Player), nth0(6,Board,Player)),
    
    % Ensure player is not empty (0)
    Player \= 0.

% full(Board)
% Checks if the board is completely full (no empty cells)
% Board: current board state
% Returns true if no cell contains 0 (empty)
full(Board) :- \+ member(0, Board).

% game_over(Board)
% Checks if the game has ended (someone won or it's a draw)
% Board: current board state
% Returns true if the game is over
game_over(Board) :- 
    winner(Board, Player), Player \= 0 ;        % Someone won
    full(Board).                                 % Board is full (draw)