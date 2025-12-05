%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main module for Connect 4 game
%%% Adapted from morpion.prolog for Connect 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% File: main.prolog
% Purpose: This is the main entry point for the Connect 4 game, managing the overall game flow, player configuration, user input/output, and coordination between board, game logic, and AI modules.
% It handles initialization, turn-based play, win detection, and game restart logic.
% Adapted from morpion.prolog: The structure (run, hello, play, etc.) is similar, but adapted for Connect 4: supports 0-2 human players, column-based moves instead of square selection, uses 6x7 board and gravity rules, and integrates Connect 4 win checks and AI.

:- dynamic player/2.
:- dynamic board/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% next_player(Current, Next): defines turn order
next_player(1, 2).
next_player(2, 1).

% player_mark(Player, Mark): assigns marks to players
player_mark(1, 'x').
player_mark(2, 'o').

% blank_mark(Mark): the empty cell marker
blank_mark('e').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% run
% Starts the game by calling hello, then play, then goodbye.
% Adapted from morpion.prolog: identical structure.
run :-
    hello,
    play(1),
    goodbye.

% run (alternative for restart)
% Handles game restart after completion.
run :-
    goodbye.

% hello
% Initializes the board, prints welcome, reads player setup, outputs player info.
% Adapted from morpion.prolog: morpion's hello initializes flat board; this calls init_board for 2D matrix.
hello :-
    init_board,
    nl, nl, nl,
    write('Welcome to Connect 4.'),
    read_players,
    output_players.

% goodbye
% Prints game result, cleans up dynamic facts, prompts for replay.
% Adapted from morpion.prolog: similar, but adapted for Connect 4 board and win output.
goodbye :-
    board(B),
    nl, nl,
    write('Game over: '),
    output_winner(B),
    retractall(board(_)),
    retractall(player(_, _)),
    read_play_again(V), !,
    (V == 'Y' ; V == 'y'), !,
    run.

% read_play_again(V)
% Prompts user for Y/N to play again, validates input.
% V: atom, user's response
% Adapted from morpion.prolog: identical.
read_play_again(V) :-
    nl, nl,
    write('Play again (Y/N)? '),
    read(V),
    (V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !
    .

% read_play_again(V) (error handling)
% Reprompts if invalid input.
read_play_again(V) :-
    nl, nl,
    write('Please enter Y or N.'),
    read_play_again(V).

% read_players
% Prompts for game mode and sets up players accordingly.
read_players :-
    nl, nl,
    write('Choose game mode: 1. Human vs Human, 2. Human vs AI, 3. AI vs AI'),
    read(Choice),
    (Choice == 1 ; Choice == 2 ; Choice == 3), !,
    set_players(Choice).

% read_players (error handling)
% Reprompts if invalid choice.
read_players :-
    nl, nl,
    write('Please enter 1, 2, or 3.'),
    read_players.

% set_players(1)
% Sets both players to human.
set_players(1) :-
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

% set_players(2)
% Sets player 1 to human, player 2 to computer.
set_players(2) :-
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

% set_players(3)
% Sets both players to computer.
set_players(3) :-
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !
    .

% output_players
% Prints the player types.
output_players :-
    nl,
    player(1, V1),
    write('Player 1 is '),
    write(V1),
    nl,
    player(2, V2),
    write('Player 2 is '),
    write(V2), !
    .

% play(P)
% Main game loop: prints board, checks game over, makes move, recurses.
% P: integer (1 or 2), current player
% Adapted from morpion.prolog: similar loop, but uses game_over from game_logic, and print_board from board.
play(P) :-
    board(B), !,
    print_board, !,
    game_over(B, Result), !,
    (Result == 'no' ->
        make_move(P, B), !,
        next_player(P, P2), !,
        play(P2)
    ;
        true
    ).

% make_move(P, B)
% Applies the move for player P on board B, updates the dynamic board.
% P: integer, player number
% B: list of lists, current board
make_move(P, B) :-
    player(P, Type),
    make_move2(Type, P, B, B2),
    retract( board(_) ),
    asserta( board(B2) ).

% make_move2(human, P, B, B2)
% Handles human move: prompts for column, validates, applies drop_piece.
% Adapted from morpion.prolog: morpion prompts for square; this prompts for column and uses available_moves.
make_move2(human, P, B, B2) :-
    nl, nl,
    write('Player '),
    write(P),
    write(' choose a column (1-7): '),
    read(Col),
    available_moves(B, Moves),
    member(Col, Moves),
    player_mark(P, M),
    drop_piece(B, Col, M, B2), !
    .

% make_move2(human, P, B, B2) (error)
% Reprompts on invalid column.
make_move2(human, P, B, B2) :-
    nl, nl,
    write('Invalid column. Please choose a valid column.'),
    make_move2(human, P, B, B2).

% make_move2(computer, P, B, B2)
% Handles computer move: uses minimax to choose column, applies move.
% Adapted from morpion.prolog: morpion calls minimax directly; this uses the adapted minimax from ai.prolog.
make_move2(computer, P, B, B2) :-
    nl, nl,
    write('Computer is thinking...'),
    player_mark(P, M),
    minimax(3, B, M, Col),
    drop_piece(B, Col, M, B2),
    nl, nl,
    write('Computer places '),
    write(M),
    write(' in column '),
    write(Col).

% output_winner(B)
% Prints the winner or draw based on game_over.
% B: list of lists, final board
% Adapted from morpion.prolog: similar, but adapted for 'draw' result.
output_winner(B) :-
    game_over(B, Result),
    (Result == 'x' -> write('X wins.')
    ; Result == 'o' -> write('O wins.')
    ; Result == 'draw' -> write('Draw.')
    ; write('No winner.')
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test predicates for debugging
test_win :-
    init_board,
    Empty = [['e','e','e','e','e','e','e'],['e','e','e','e','e','e','e'],['e','e','e','e','e','e','e'],['e','e','e','e','e','e','e'],['e','e','e','e','e','e','e'],['e','e','e','e','e','e','e']],
    drop_piece(Empty, 1, 'x', B1),
    drop_piece(B1, 1, 'x', B2),
    drop_piece(B2, 1, 'x', B3),
    drop_piece(B3, 1, 'x', B4),
    get_column(B4, 1, L),
    write('Column 1: '), write(L), nl,
    retractall(board(_)),
    asserta(board(B4)),
    print_board,
    (win(B4, 'x') -> write('Win detected for x') ; write('No win for x')),
    nl,
    (has_four_consecutive([e,e,x,x,x,x], 'x') -> write('Has four') ; write('No four')),
    nl,
    (check_vertical(B4, 'x') -> write('Vertical win') ; write('No vertical')),
    nl,
    (win(B4, M) -> write('Win with M='), write(M) ; write('No win')),
    nl,
    (check_vertical(B4, M2) -> write('Vertical with M2='), write(M2) ; write('No vertical')),
    nl,
    game_over(B4, Result),
    write('Result: '), write(Result), nl.

test_draw :-
    % Create a full board without 4 in a row
    % For simplicity, alternate x and o in a pattern that avoids 4 in a row
    B = [
        ['x','o','x','o','x','o','x'],
        ['o','x','o','x','o','x','o'],
        ['x','o','x','o','x','o','x'],
        ['o','x','o','x','o','x','o'],
        ['x','o','x','o','x','o','x'],
        ['o','x','o','x','o','x','o']
    ],
    game_over(B, Result),
    write('Result: '), write(Result), nl.
%%% End of main.prolog
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%