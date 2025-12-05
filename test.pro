:- consult('puissance4.pro').

% Utilitaire pour lancer un test et afficher le résultat
run_test(TestName, Goal) :-
    write('Test: '), write(TestName), write(' ... '),
    (   catch(Goal, _, fail)
    ->  ansi_format([fg(green)], 'REUSSI', []), nl
    ;   ansi_format([fg(red)], 'ECHOUE', []), nl
    ).

% Test de generate_matrix
test_generate_matrix :-
    generate_matrix(7, 6, Board),
    length(Board, 6),
    Board = [FirstRow|_],
    length(FirstRow, 7).

% Test de replace
test_replace :-
    List = [a, b, c, d, e],
    replace(List, 2, 'X', [a, b, 'X', d, e]).

% Test de replaceMatrix
test_replace_matrix :-
    generate_matrix(3, 3, Board),
    replaceMatrix(Board, 1, 1, 'X', NewBoard),
    nth0(1, NewBoard, Row),
    nth0(1, Row, 'X').

% Test de print_board (vérifie juste que ça ne plante pas)
test_print_board :-
    generate_matrix(7, 6, Board),
    replaceMatrix(Board, 0, 0, x, Board1),
    replaceMatrix(Board1, 0, 1, o, Board2),
    replaceMatrix(Board2, 1, 0, o, Board3),
    replaceMatrix(Board3, 1, 1, x, Board4),
    print_board(Board4).

% Test de play_human_move
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

% Test de init
test_init :-
    init,
    board(Board),
    length(Board, 6),
    Board = [FirstRow|_],
    length(FirstRow, 7).

% Test complet
run_all_tests :-
    nl,
    writeln('========================================'),
    writeln('     EXECUTION DES TESTS'),
    writeln('========================================'),
    nl,
    run_test('generate_matrix', test_generate_matrix),
    run_test('replace', test_replace),
    run_test('replaceMatrix', test_replace_matrix),
    run_test('print_board', test_print_board),
    run_test('init', test_init),
    run_test('play_human_move', test_play_human_move),
    nl,
    writeln('========================================'),
    writeln('     TESTS TERMINES'),
    writeln('========================================'),
    nl.