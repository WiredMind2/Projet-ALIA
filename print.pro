print_cell(.) :-
    write('.').

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
    O is  N - 1,
    print_matrix(Rest, O),
    print_row(Row, N).
    
print_column_number(N, N) :- write(N), !.
print_column_number(X, N) :-
    X < N,
    write(X),
    write(' '),
    X1 is X + 1,
    print_column_number(X1, N).

print_board(Board) :-
    print_matrix(Board, 6),
    print_column_number(1, 7),
    nl.