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

print_matrix([]).
print_matrix([Row|Rest]) :-
    writeln(Row),
    print_matrix(Rest).

print_board :-
    board(Board),
    print_matrix(Board).