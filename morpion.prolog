:- dynamic board/1. % permet l'assertion et le retrait de faits board/1

play(Player):-
    write('New turn for:'), writeln(Player),
    board(Board),
    displayBoard,
    
    (game_over(Board) ->
        ((winner(Board, Winner) ->
            write('Player '), write(Winner), writeln(' wins!')) ;
            writeln('It\'s a draw!'))
    );

    ia(Board, Move, Player),

    playMove(Board, Move, NewBoard, Player),
    applyIt(Board, NewBoard),

    changePlayer(Player, NextPlayer),
    play(NextPlayer).

%%%%% Start the game! 
init :- length(Board,9), reset(Board), assert(board(Board)), play('x').


reset([]).
reset([HA|A]) :-
    HA = 0,
    reset(A).

displayBoard :-
    board(Board),
    nth0(0,Board,A), nth0(1,Board,B), nth0(2,Board,C),
    format('~w ~w ~w~n', [A,B,C]),
    nth0(3,Board,D), nth0(4,Board,E), nth0(5,Board,F),
    format('~w ~w ~w~n', [D,E,F]),
    nth0(6,Board,G), nth0(7,Board,H), nth0(8,Board,I),
    format('~w ~w ~w~n', [G,H,I]).

playMove(Board, Move, NewBoard, Player) :-
    nth0(Move, Board, 0, Rest),                % check value is 0 and get Rest (board without that position)
    length(Prefix, Move),                      % Prefix is first Move elements
    append(Prefix, Suffix, Rest),
    append(Prefix, [Player|Suffix], NewBoard).   % insert Player at position Move

ia(Board, Move, Player) :-
    human(Board, Player, Move).

human(Board, Player, Move) :-
    repeat,
    write('Your turn: '),
    read_string(user_input, "\n", "", _, Str),
    number_string(Input, Str),
    Pos is Input - 1,
    write('You played '),
    writeln(Input),
    (Pos < 0 ; Pos > 8 ; \+ nth0(Pos, Board, 0)) ->
        writeln('Invalid move'), fail ;
        Move = Pos,
        true
    .

applyIt(Board, NewBoard) :-
    retract(board(Board)),
    assert(board(NewBoard)).

changePlayer('x', 'o').
changePlayer('o', 'x').

winner(Board, Player) :-
    (
    (nth0(0,Board,Player), nth0(1,Board,Player), nth0(2,Board,Player));
    (nth0(3,Board,Player), nth0(4,Board,Player), nth0(5,Board,Player));
    (nth0(6,Board,Player), nth0(7,Board,Player), nth0(8,Board,Player));
    (nth0(0,Board,Player), nth0(3,Board,Player), nth0(6,Board,Player));
    (nth0(1,Board,Player), nth0(4,Board,Player), nth0(7,Board,Player));
    (nth0(2,Board,Player), nth0(5,Board,Player), nth0(8,Board,Player));
    (nth0(0,Board,Player), nth0(4,Board,Player), nth0(8,Board,Player));
    (nth0(2,Board,Player), nth0(4,Board,Player), nth0(6,Board,Player))),

    Player \= 0.

full(Board) :- \+ member(0, Board).

game_over(Board) :- winner(Board, Player), Player \= 0; full(Board).

