:- dynamic board/1.

% Puissance 4

length_list(N, List) :- length(List, N).

generate_matrix(Cols, Rows, Matrix) :-
    length_list(Rows, Matrix),
    maplist(length_list(Cols), Matrix).

init :- 
    retractall(board(_)),
    generate_matrix(7,6,Board),
    assert(board(Board)).

% ---- Board checking utilities ----

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

print_board :-
    board(Board),
    print_matrix(Board, 6).