% Connect Four (Puissance 4) main game file
% This is the primary game file that handles the game loop, menu system, and player interactions

:- dynamic board/1.                               % Dynamic predicate to store current game board
:- dynamic last_index/1.                          % Dynamic predicate to store column heights (for gravity simulation)

:- consult('print.pro').                          % Load board printing functions
:- consult('matrix.pro').                         % Load matrix utility functions
:- consult('win.pro').                            % Load win detection functions
:- ensure_loaded('ai_selector.pro').              % Load AI selection system
:- consult('ai/game_utils.pro').                  % Load shared game utilities

% iaMinimax(Board, NewBoard, Player)
% Backward compatibility wrapper for iaMinimax in main game
% This function provides a fallback mechanism if minimax fails
% Board: current game board
% NewBoard: resulting board after AI move
% Player: player number (1 or 2) making the move
iaMinimax(Board, NewBoard, Player) :-
    (   catch(minimax_ai(Board, NewBoard, Player), _, fail) % Try minimax AI
    ->  true                                      % Minimax succeeded
    ;   % Fallback to random if minimax fails
        random_ai(Board, NewBoard, Player)       % Use random AI as backup
    ).

% play(Player)
% Main game loop for player vs player mode
% Handles a complete turn including move validation, board update, and win checking
% Player: current player number (1 or 2)
play(Player) :-  
    write('New turn for: '),                     % Display current player
    writeln(Player),
    board(Board),                                % Get current board state
    print_board(Board),                          % Display the board
    playHumanMove(Board, NewBoard, Player),      % Get player's move
    applyBoard(Board, NewBoard),                 % Update board state
    game_over(NewBoard, Result),                 % Check if game ended
    ( Result \= 'no' ->                          % Game is over
        ( Result = 'draw' -> 
            writeln('It''s a draw!')             % Handle draw
        ; 
            format('Player ~w wins!~n', [Result]) % Handle win
        ),
        !                                         % Cut - don't continue game
    ;
        % Game continues
        changePlayer(Player, NextPlayer),        % Switch to other player
        play(NextPlayer)                         % Continue with next player
    ).

% start
% Initialize and start the game
% Sets up the game and displays the main menu
start :-
    setup,                                       % Initialize game state
    main_menu.                                   % Display main menu

% main_menu
% Displays the main menu and handles user choice
% Presents different game modes to the user
main_menu :-
    writeln('Choose game mode:'),                % Display menu prompt
    writeln('1 - Player vs Player'),             % Option 1: Human vs Human
    writeln('2 - Player vs IA'),                 % Option 2: Human vs AI
    writeln('3 - IA vs IA'),                     % Option 3: AI vs AI
    writeln('q - Quit'),                         % Option 4: Exit game
    read(Choice),                                % Read user input
    handle_choice(Choice).                       % Process the choice

% handle_choice(Choice)
% Handles the user's menu selection
% Choice: user's menu selection (1, 2, 3, or q)
handle_choice(1) :-                             % Player vs Player
    write('Starting Player vs Player...'), nl,
    play(1).                                     % Start with player 1
handle_choice(2) :-                             % Player vs AI
    write('Starting Player vs IA...'), nl,
    get_ai_for_player(2, AI),                   % Let player 2 choose AI
    play_pvai(AI, 1).                           % Start with human player 1
handle_choice(3) :-                             % AI vs AI
    write('Starting IA vs IA...'), nl,
    get_ai_for_player(1, AI1),                  % Choose AI for player 1
    get_ai_for_player(2, AI2),                  % Choose AI for player 2
    play_aivai(AI1, AI2, 1).                    % Start with AI vs AI match
handle_choice(q) :-                             % Quit
    writeln('Goodbye.').
handle_choice(_) :-                             % Invalid choice
    writeln('Invalid choice, try again.'),
    main_menu.

% play_pvai(AI, Player)
% Game loop for Player vs AI mode
% AI: AI predicate to use for the computer player
% Player: current player (1=human, 2=AI)
play_pvai(AI, Player) :-
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 1 ->                             % Human player's turn
        playHumanMove(Board, NewBoard, Player)
    ;                                           % AI player's turn
        ( call(AI, Board, NewBoard, Player) -> true % Try to call AI
        ; (writeln('AI failed, using random move'), % AI failed, use fallback
           iaRandom(Board, NewBoard, Player)) 
        )
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->                         % Game is over
        ( Result = 'draw' -> 
            writeln('It''s a draw!')
        ; 
            format('Player ~w wins!~n', [Result])
        )
    ;
        % Game continues
        changePlayer(Player, NextPlayer),
        play_pvai(AI, NextPlayer)
    ).

% play_aivai(AI1, AI2, Player)
% Game loop for AI vs AI mode
% AI1: AI predicate for player 1
% AI2: AI predicate for player 2
% Player: current player (1 or 2)
play_aivai(AI1, AI2, Player) :-
    write('New turn for: '), writeln(Player),
    board(Board),
    % Print board with error handling
    (catch(print_board(Board), E, (write('Error printing board: '), writeln(E), fail)) -> true 
     ; writeln('Failed to print board')),
    ( Player = 1 ->                             % Player 1's turn (AI1)
        ( catch(call(AI1, Board, NewBoard, Player), E, % Try AI1 with error handling
                (format('AI1 error: ~w~n', [E]), fail)) -> true 
        ; (writeln('AI1 failed, using random move'),    % AI1 failed, use fallback
           iaRandom(Board, NewBoard, Player)) 
        )
    ;
        % Player 2's turn (AI2)
        ( catch(call(AI2, Board, NewBoard, Player), E, % Try AI2 with error handling
                (format('AI2 error: ~w~n', [E]), fail)) -> true 
        ; (writeln('AI2 failed, using random move'),    % AI2 failed, use fallback
           iaRandom(Board, NewBoard, Player)) 
        )
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->                         % Game is over
        ( Result = 'draw' -> 
            writeln('It''s a draw!')
        ; 
            format('Player ~w wins!~n', [Result])
        )
    ;
        % Game continues
        changePlayer(Player, NextPlayer),
        play_aivai(AI1, AI2, NextPlayer)
    ).

% play_pvai_minimax(Player)
% Legacy function for Player vs Minimax AI
% This function is kept for backward compatibility
% Player: current player ('x'=human, 'o'=AI)
play_pvai_minimax(Player) :-
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 'x' ->                           % Human player
        playHumanMove(Board, NewBoard, Player)
    ;
        iaMinimax(Board, NewBoard, Player)     % Minimax AI
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->                         % Game is over
        ( Result = 'draw' -> 
            writeln('It''s a draw!')
        ; 
            format('Player ~w wins!~n', [Result])
        )
    ;
        % Game continues
        changePlayer(Player, NextPlayer),
        play_pvai_minimax(NextPlayer)
    ).

% playHumanMove(Board, NewBoard, Player)
% Handles human player input and move validation
% Provides interactive input for column selection with validation
% Board: current game board
% NewBoard: resulting board after valid move
% Player: player number (1 or 2) making the move
playHumanMove(Board, NewBoard, Player) :-
    read(Col),                                   % Read column input from user
    (   integer(Col), Col >= 1, Col =< 7        % Validate input is integer 1-7
    ->  ColIndex is Col - 1,                     % Convert to 0-based index
        (   validMove(ColIndex)                  % Check if column has space
        ->  playMove(Board, ColIndex, TmpBoard, Player), % Apply the move
            NewBoard = TmpBoard,
            write('Dropping in column '), write(Col), nl,
            last_index(UpdatedIdx),              % Show updated column heights
            write('Updated Indices: '), writeln(UpdatedIdx),
            true
        ;   
            writeln('Column is full, pick another.'), % Column full, ask again
            playHumanMove(Board, NewBoard, Player)
        )
    ;   
        writeln('Invalid input, enter a number between 1 and 7.'), % Invalid input
        playHumanMove(Board, NewBoard, Player)
    ).