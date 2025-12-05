:- dynamic board/1.
:- dynamic last_index/1.

:- consult('print.pro').
:- consult('matrix.pro').

play(Player):-  
    write('New turn for: '),
    writeln(Player),
    board(Board),
    print_board(Board),
    playMove(Board,0,NewBoard,Player),
    applyBoard(Board, NewBoard),
    print_board(NewBoard).

playMove(Board,Col,NewBoard,Player):-
    last_index(LastIndex),
    nth0(Col,LastIndex,Row),
    Row < 6,                         
    replaceMatrix(Board,Row,Col, Player,NewBoard),
    NewRow is Row + 1,
    replace(LastIndex,Col,NewRow,NewLastIndex),
    applyLastIndex(LastIndex,NewLastIndex).


applyBoard(_OldBoard,NewBoard):-
    retractall(board(_)),
    assert(board(NewBoard)).

applyLastIndex(_OldLastIndex,NewLastIndex):-
    retractall(last_index(_)),
    assert(last_index(NewLastIndex)).

changePlayer('x', 'o').
changePlayer('o', 'x').

validMove(Col) :-
    Col >= 0,
    Col < 7,
    last_index(Indices),
    nth0(Col, Indices, Row),
    Row < 6.

applyIt(OldBoard, NewBoard):- 
    retract(board(OldBoard)), 
    assert(board(NewBoard)).

init :- 
    retractall(board(_)),
    retractall(last_index(_)),
    generate_matrix(7,6,Board),
    length_list(7, Indices),
    maplist(=(0), Indices),
    assert(last_index(Indices)),
    assert(board(Board)),
    play('x').

get_item_2d(Matrix, Row, Col, Value) :- nth0(Row, Matrix, TheRow), nth0(Col, TheRow, Value).

horizontal_win(Board, M) :-
    between(0,5,Row),
    between(0,3,StartCol),
    get_item_2d(Board, Row, StartCol, M),
    C2 is StartCol + 1, get_item_2d(Board, Row, C2, M),
    C3 is StartCol + 2, get_item_2d(Board, Row, C3, M),
    C4 is StartCol + 3, get_item_2d(Board, Row, C4, M).

vertical_win(Board, M) :-
    between(0,6,Col),
    between(0,2,StartRow),
    get_item_2d(Board, StartRow, Col, M),
    R2 is StartRow + 1, get_item_2d(Board, R2, Col, M),
    R3 is StartRow + 2, get_item_2d(Board, R3, Col, M),
    R4 is StartRow + 3, get_item_2d(Board, R4, Col, M).

diagonal1_win(Board, M) :-  % / diagonal
    between(0,2,StartRow),
    between(3,6,StartCol),
    get_item_2d(Board, StartRow, StartCol, M),
    R2 is StartRow + 1, C2 is StartCol - 1, get_item_2d(Board, R2, C2, M),
    R3 is StartRow + 2, C3 is StartCol - 2, get_item_2d(Board, R3, C3, M),
    R4 is StartRow + 3, C4 is StartCol - 3, get_item_2d(Board, R4, C4, M).

diagonal2_win(Board, M) :-  % \ diagonal
    between(0,2,StartRow),
    between(0,3,StartCol),
    get_item_2d(Board, StartRow, StartCol, M),
    R2 is StartRow + 1, C2 is StartCol + 1, get_item_2d(Board, R2, C2, M),
    R3 is StartRow + 2, C3 is StartCol + 2, get_item_2d(Board, R3, C3, M),
    R4 is StartRow + 3, C4 is StartCol + 3, get_item_2d(Board, R4, C4, M).

win(Board, M) :-
    (horizontal_win(Board, M) ;
     vertical_win(Board, M) ;
     diagonal1_win(Board, M) ;
     diagonal2_win(Board, M)),
    M \= '.'.

game_over(Board, Result) :-
    (win(Board, M) -> Result = M ;
     (\+ (member(Row, Board), member('.', Row)) -> Result = 'draw' ;
      Result = 'no')).

available_moves(Board, Moves) :-
    findall(Col, (between(0,6,Col), get_item_2d(Board, 0, Col, '.')), Moves).

play_human_move(Board,NewBoard,Player) :-
    repeat,
    write('Player '), write(Player), writeln(', choose a column (1-7): '),
    read(Col),
    (   integer(Col), Col >= 1, Col =< 7
    ->  ColIndex is Col - 1,
        (   playMove(Board, ColIndex, TmpBoard, Player)
        ->  NewBoard = TmpBoard,
            write('Dropping in column '), write(Col), nl,
            last_index(UpdatedIdx),
            write('Updated Indices: '), writeln(UpdatedIdx),
            game_over(NewBoard, Result),
            (Result \= 'no' ->
                (Result = 'draw' -> writeln('It\'s a draw!') ;
                 format('Player ~w wins!~n', [Result])),
                fail ;
                true)
        ;   writeln('Column is full, pick another.'),
            fail
        )
    ;   writeln('Invalid input, enter a number between 1 and 7.'),
        fail
    ).


test_play_human_move :-
    init,
    board(Board),
    print_board(Board),
    play_human_move(Board, NewBoard, x),
    applyIt(Board, NewBoard),
    print_board(NewBoard),
    board(UpdatedBoard), 
    play_human_move(UpdatedBoard, NewBoard2, o),
    applyIt(UpdatedBoard, NewBoard2),
    print_board(NewBoard2).