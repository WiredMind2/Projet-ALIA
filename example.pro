:- dynamic board/1. % permet l'assertion et le retrait de faits board/1

play(Player):-  
    write('New turn for:'),
    writeln(Player),
    board(Board),
    displayBoard,
    ia(Board, Move,Player),
    playMove(Board,Move,NewBoard,Player),
    applyIt(Board, NewBoard),

    (   isWinner(NewBoard, Player) ->
        ( write('Player '), write(Player), writeln(' wins!'), displayBoard, ! )

    ;   isDraw(NewBoard) ->
        ( writeln('Game over, it''s a draw!'), displayBoard, ! )

    ;   % otherwise, continue
        (   changePlayer(Player,NextPlayer),
            play(NextPlayer)
        )
    ).


changePlayer('x', 'o').
changePlayer('o', 'x').

applyIt(OldBoard, NewBoard):- 
    retract(board(OldBoard)), 
    assert(board(NewBoard)).

displayBoard :-
    board(Board),
    displayRow(Board, 0),
    displayRow(Board, 3),
    displayRow(Board, 6).    

displayRow(Board, Start) :-
    I1 is Start + 1, I2 is Start + 2,
    nth0(Start, Board, C1), nth0(I1, Board, C2), nth0(I2, Board, C3),
    ( nonvar(C1) -> write(C1) ; write('.') ), write(' | '),
    ( nonvar(C2) -> write(C2) ; write('.') ), write(' | '),
    ( nonvar(C3) -> write(C3) ; write('.') ), nl.

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

winning_combination([0,1,2]).
winning_combination([3,4,5]).
winning_combination([6,7,8]).
winning_combination([0,3,6]).
winning_combination([1,4,7]).
winning_combination([2,5,8]).
winning_combination([0,4,8]).
winning_combination([2,4,6]).

isWinner(Board, Player) :-
    winning_combination([A,B,C]),
    nth0(A, Board, CellA),
    nth0(B, Board, CellB),
    nth0(C, Board, CellC),
    CellA == Player,
    CellB == Player,
    CellC == Player.

isDraw(Board) :-
    \+ ( member(Cell, Board), var(Cell) ).

playMove(Board, Move, NewBoard, Player) :-
    nth0(Move, Board, Cell),
    replace(Board, Move, Player, NewBoard).

ia(Board, Move, Player) :-
    repeat,
    N is random(9),
    nth0(N, Board, Cell),
    var(Cell),
    !,
    Move is N.

%%%%% Start the game! 
init :- 
    retractall(board(_)),
    length(Board,9), 
    assert(board(Board)), 
    play('x').