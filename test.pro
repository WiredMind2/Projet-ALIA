:- consult('puissance4.pro').
:- use_module(library(lists)).

% ========================================
% UTILES POUR LES TESTS
% ========================================

% Compte le nombre de tests réussis
:- dynamic test_passed/1.
:- dynamic test_failed/1.

reset_counters :-
    retractall(test_passed(_)),
    retractall(test_failed(_)),
    assert(test_passed(0)),
    assert(test_failed(0)).

increment_passed :-
    retract(test_passed(N)),
    N1 is N + 1,
    assert(test_passed(N1)).

increment_failed :-
    retract(test_failed(N)),
    N1 is N + 1,
    assert(test_failed(N1)).

% Macro de test
test(Name, Goal) :-
    write('Test: '), write(Name), write(' ... '),
    (   call(Goal)
    ->  writeln('OK'), increment_passed, !
    ;   writeln('ECHEC'), increment_failed, !
    ).

% ========================================
% TESTS DES FONCTIONS MATRICIELLES
% ========================================

test_matrix :-
    nl,
    writeln('--- Tests des fonctions matricielles ---'),
    
    % Test génération matrice
    test('generate_matrix crée une matrice 7x6',
        (generate_matrix(7, 6, Board1),
         length(Board1, 6),
         Board1 = [Row1|_],
         length(Row1, 7))),
    
    % Test replace
    test('replace un élément dans une liste',
        (replace([a, b, c], 1, x, Result),
         Result == [a, x, c])),
    
    % Test replaceMatrix
    test('replaceMatrix remplace un élément dans une matrice',
        (generate_matrix(3, 3, Board3),
         replaceMatrix(Board3, 0, 0, x, NewBoard3),
         get_item_2d(NewBoard3, 0, 0, x))),

    % Test get_item_2d
    test('get_item_2d récupère un élément',
        (Board4 = [[a, b], [c, d]],
         get_item_2d(Board4, 1, 0, c))),
    
    nl.

% ========================================
% TESTS DES MECANISMES DE JEU
% ========================================

test_game_mechanics :-
    nl,
    writeln('--- Tests des mécanismes de jeu ---'),
    
    % Test setup
    test('setup initialise le plateau',
        (setup,
         board(B),
         last_index(LI),
         length(B, 6),
         B = [Row|_],
         length(Row, 7),
         length(LI, 7))),
    
    % Test changePlayer
    test('changePlayer change x en o',
        changePlayer(x, o)),
    
    test('changePlayer change o en x',
        changePlayer(o, x)),
    
    % Test validMove sur plateau vide
    test('validMove accepte colonne 0 sur plateau vide',
        (setup, validMove(0))),
    
    test('validMove accepte colonne 6 sur plateau vide',
        (setup, validMove(6))),
    
    test('validMove refuse colonne 7',
        (setup, \+ validMove(7))),
    
    test('validMove refuse colonne -1',
        (setup, \+ validMove(-1))),

    test('validMove refuse une colonne pleine',
        (setup,
         last_index(LastIndex),
         replace(LastIndex, 3, 6, FullLastIndex),
         applyLastIndex(LastIndex, FullLastIndex),
         \+ validMove(3))),
    
    % Test playMove
    test('playMove place un pion dans une colonne vide',
        (setup,
         board(Board),
         last_index(LastIndex),
         playMove(Board, 3, NewBoard, x, LastIndex, NewLastIndex),
         get_item_2d(NewBoard, 0, 3, x),
         nth0(3, NewLastIndex, 1))),
    
    % Test empilage de pions
    test('playMove empile correctement les pions',
        (setup,
         board(Board),
         last_index(LastIndex),
         playMove(Board, 3, B1, x, LastIndex, LI1),
         playMove(B1, 3, B2, o, LI1, LI2),
         get_item_2d(B2, 0, 3, x),
         get_item_2d(B2, 1, 3, o),
         nth0(3, LI2, 2))),
    
    nl.

% ========================================
% TESTS DES CONDITIONS DE VICTOIRE
% ========================================

test_win_conditions :-
    nl,
    writeln('--- Tests des conditions de victoire ---'),
    
    % Test horizontal_win
    test('horizontal_win détecte 4 alignés horizontalement',
        (Board1 = [
            [x, x, x, x, '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        horizontal_win(Board1, x))),
    
    % Test vertical_win
    test('vertical_win détecte 4 alignés verticalement',
        (Board2 = [
            [o, '.', '.', '.', '.', '.', '.'],
            [o, '.', '.', '.', '.', '.', '.'],
            [o, '.', '.', '.', '.', '.', '.'],
            [o, '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        vertical_win(Board2, o))),
    
    % Test diagonal1_win (/)
    test('diagonal1_win détecte diagonale montante',
        (Board3 = [
            ['.', '.', '.', x, '.', '.', '.'],
            ['.', '.', x, '.', '.', '.', '.'],
            ['.', x, '.', '.', '.', '.', '.'],
            [x, '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        diagonal1_win(Board3, x))),
    
    % Test diagonal2_win (\)
    test('diagonal2_win détecte diagonale descendante',
        (Board4 = [
            [o, '.', '.', '.', '.', '.', '.'],
            ['.', o, '.', '.', '.', '.', '.'],
            ['.', '.', o, '.', '.', '.', '.'],
            ['.', '.', '.', o, '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        diagonal2_win(Board4, o))),
    
    % Test win
    test('win détecte une victoire',
        (Board5 = [
            [x, x, x, x, '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        win(Board5, x))),
    
    % Test game_over avec victoire
    test('game_over retourne le gagnant',
        (Board6 = [
            [o, o, o, o, '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        game_over(Board6, o))),
    
    % Test game_over avec match nul
    test('game_over détecte un match nul',
        (Board7 = [
            [x, o, x, o, x, o, x],
            [o, x, o, x, o, x, o],
            [o, x, o, x, o, x, o],
            [o, x, o, x, o, x, o],
            [x, o, x, o, x, o, x],
            [x, o, x, o, x, o, x]
        ],
        game_over(Board7, draw))),
    
    % Test game_over sans fin de jeu
    test('game_over retourne no quand partie continue',
        (Board8 = [
            [x, '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        game_over(Board8, no))),
    
    nl.

% ========================================
% TESTS DES IA
% ========================================

test_ia_random :-
    nl,
    writeln('--- Tests de l\'IA Random ---'),
    
    % Test que iaRandom joue un coup valide
    test('iaRandom joue un coup valide sur plateau vide',
        (setup,
         board(Board1),
         iaRandom(Board1, NewBoard1, x),
         Board1 \= NewBoard1,
         game_over(NewBoard1, no))),
    
    % Test sur plateau avec situation spécifique
    test('iaRandom joue un coup valide sur plateau avec pions',
        (setup,
         Board2 = [
            [x, o, x, '.', '.', '.', '.'],
            [o, x, o, '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        LastIndex2 = [2, 2, 2, 0, 0, 0, 0],
        applyBoard(_, Board2),
        applyLastIndex(_, LastIndex2),
        iaRandom(Board2, NewBoard2, x),
        Board2 \= NewBoard2)),
    
    nl.

test_ia_presque_random :-
    nl,
    writeln('--- Tests de l\'IA Presque Random ---'),
    
    % Test victoire immédiate
    test('iaPresqueRandom joue le coup gagnant',
        (setup,
         Board1 = [
            [x, x, x, '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        LastIndex1 = [1, 1, 1, 0, 0, 0, 0],
        applyBoard(_, Board1),
        applyLastIndex(_, LastIndex1),
        iaPresqueRandom(Board1, NewBoard1, x),
        get_item_2d(NewBoard1, 0, 3, x),
        game_over(NewBoard1, x))),
    
    % Test blocage adversaire
    test('iaPresqueRandom bloque l\'adversaire',
        (setup,
         Board2 = [
            [o, o, o, '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        LastIndex2 = [1, 1, 1, 0, 0, 0, 0],
        applyBoard(_, Board2),
        applyLastIndex(_, LastIndex2),
        iaPresqueRandom(Board2, NewBoard2, x),
        get_item_2d(NewBoard2, 0, 3, x))),
    
    nl.

test_ia_minimax :-
    nl,
    writeln('--- Tests de l\'IA Minimax ---'),
    
    % Test que minimax joue un coup valide
    test('iaMinimax avec alpha-beta joue un coup valide',
        (setup,
         board(Board1),
         iaMinimax(Board1, NewBoard1, x, 2, true),
         Board1 \= NewBoard1,
         game_over(NewBoard1, no))),
    
    % Test minimax sans alpha-beta
    test('iaMinimax sans alpha-beta joue un coup valide',
        (setup,
         board(Board2),
         iaMinimax(Board2, NewBoard2, x, 2, false),
         Board2 \= NewBoard2,
         game_over(NewBoard2, no))),
    
    % Test victoire immédiate avec minimax
    test('iaMinimax détecte et joue le coup gagnant',
        (setup,
         Board3 = [
            [x, x, x, '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        LastIndex3 = [1, 1, 1, 0, 0, 0, 0],
        applyBoard(_, Board3),
        applyLastIndex(_, LastIndex3),
        iaMinimax(Board3, NewBoard3, x, 3, true),
        get_item_2d(NewBoard3, 0, 3, x),
        game_over(NewBoard3, x))),
    
    % Test blocage avec minimax
    test('iaMinimax bloque l\'adversaire',
        (setup,
         Board4 = [
            [o, o, o, '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        LastIndex4 = [1, 1, 1, 0, 0, 0, 0],
        applyBoard(_, Board4),
        applyLastIndex(_, LastIndex4),
        iaMinimax(Board4, NewBoard4, x, 4, true),
        get_item_2d(NewBoard4, 0, 3, x))),
    
    % Test evaluation sur plateau complexe
    test('iaMinimax joue un coup raisonnable sur plateau complexe',
        (setup,
         Board5 = [
            [x, o, x, '.', '.', '.', '.'],
            [o, x, o, '.', '.', '.', '.'],
            [x, o, x, '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.']
        ],
        LastIndex5 = [3, 3, 3, 0, 0, 0, 0],
        applyBoard(_, Board5),
        applyLastIndex(_, LastIndex5),
        iaMinimax(Board5, NewBoard5, o, 3, true),
        Board5 \= NewBoard5,
        game_over(NewBoard5, no))),
    
    nl.

% ========================================
% TEST COMPLET
% ========================================

run_all_tests :-
    reset_counters,
    nl,
    writeln('========================================'),
    writeln('     EXECUTION DES TESTS'),
    writeln('========================================'),
    nl,
    
    test_matrix,
    test_game_mechanics,
    test_win_conditions,
    test_ia_random,
    test_ia_presque_random,
    test_ia_minimax,
    
    nl,
    writeln('========================================'),
    writeln('     TESTS TERMINES'),
    writeln('========================================'),
    test_passed(Passed),
    test_failed(Failed),
    Total is Passed + Failed,
    format('Total: ~w tests~n', [Total]),
    format('Réussis: ~w~n', [Passed]),
    format('Échoués: ~w~n', [Failed]),
    (   Failed = 0
    ->  writeln('Tous les tests sont passés avec succès!')
    ;   writeln('Certains tests ont échoué.')
    ),
    nl.