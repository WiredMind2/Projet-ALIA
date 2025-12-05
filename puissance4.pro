:- dynamic board/1.
:- dynamic last_index/1.

% Puissance 4

length_list(N, List) :- length(List, N).

generate_matrix(Cols, Rows, Matrix) :-
    length_list(Rows, Matrix),
    maplist(length_list(Cols), Matrix).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

replaceMatrix(Matrix, RowIndex, ColIndex, Player, NewMatrix) :-
    nth0(RowIndex, Matrix, OldRow),
    replace(OldRow, ColIndex, Player, NewRow),
    replace(Matrix, RowIndex, NewRow, NewMatrix).

testReplace :-
    generate_matrix(7,6,Board),
    replaceMatrix(Board, 0, 0, 'A', NewBoard),
    replaceMatrix(NewBoard, 1, 0, 'B', NewBoard2),
    print_matrix(NewBoard2).

init :- 
    retractall(board(_)),
    retractall(last_index(_)),
    generate_matrix(7,6,Board),
    length_list(7, Indices),
    assert(last_index(Indices)),
    assert(board(Board)).

changePlayer('A', 'B').
changePlayer('B', 'A').

applyIt(OldBoard, NewBoard):- 
    retract(board(OldBoard)), 
    assert(board(NewBoard)).


% ---- Board checking utilities ----

print_cell(.) :-
    write('. ').

print_cell(x) :- 
    ansi_format([fg(red)], 'O', []).
    
print_cell(o) :- 
    ansi_format([fg(yellow)], 'O', []).

print_row([], N) :-
    write(' '),
    write(N),
    writeln(' ').
print_row([Cell|Rest], N) :-
    print_cell(Cell), write(' '),
    print_row(Rest, N).

print_matrix([], 0).
print_matrix([Row|Rest], N) :-
    print_row(Row, N),
    O is  N - 1,
    print_matrix(Rest, O).

print_column_number(N, N) :- write(N), !.
print_column_number(X, N) :-
    X < N,
    write(X),
    write(' '),
    X1 is X + 1,
    print_column_number(X1, N).

print_board :-
    board(Board),
    print_matrix(Board, 6),
    print_column_number(1, 7).

