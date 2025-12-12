:- dynamic board/1.
:- dynamic last_index/1.

:- consult('print.pro').
:- consult('matrix.pro').
:- consult('win.pro').
:- consult('ai_selector.pro').

% Minimax AI implementation
iaMinimax(Board, NewBoard, Player) :-
    minimax(Board, 1, Player, BestCol, _),
    playMove(Board, BestCol, NewBoard, Player).

minimax(Board, Depth, Player, BestCol, Score) :-
    (Depth =< 0 ; (game_over(Board, Result), Result \= 'no')) ->
    evaluate(Board, Player, Score), BestCol = -1.

minimax(Board, Depth, Player, BestCol, Score) :-
    findall(Col, validMove(Col), ValidMoves),
    ValidMoves \= [] ->
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    findall(Score-Col, (member(Col, ValidMoves), simulateMove(Board, Col, NewBoard, Player), minimax(NewBoard, NewDepth, Opponent, _, OppScore), Score is -OppScore), ScoresCols),
    sort(ScoresCols, Sorted), last(Sorted, Score-BestCol).

evaluate(Board, Player, Score) :-
    game_over(Board, Result),
    changePlayer(Player, Opponent),
    ( Result == Player -> Score = 10000
    ; Result == Opponent -> Score = -10000
    ; Result == 'draw' -> Score = 0
    ; Score = 0
    ).

simulateMove(Board, Col, NewBoard, Player) :-
    last_index(LastIndex),
    nth0(Col, LastIndex, Row),
    Row < 6,
    replaceMatrix(Board, Row, Col, Player, NewBoard).

is_open3([P,P,P,0], P).
is_open3([0,P,P,P], P).

is_open2([P,P,0,0], P).
is_open2([0,0,P,P], P).

get_cell(Board, R, C, V) :- nth0(R, Board, Row), nth0(C, Row, V).

window_row(Board, R, C, List) :- between(0,5,R), between(0,3,C), findall(V, (between(0,3,I), C2 is C+I, get_cell(Board,R,C2,V)), List).
window_col(Board, R, C, List) :- between(0,2,R), between(0,6,C), findall(V, (between(0,3,I), R2 is R+I, get_cell(Board,R2,C,V)), List).
window_diag1(Board, R, C, List) :- between(0,2,R), between(0,3,C), findall(V, (between(0,3,I), R2 is R+I, C2 is C+I, get_cell(Board,R2,C2,V)), List).
window_diag2(Board, R, C, List) :- between(0,2,R), between(3,6,C), findall(V, (between(0,3,I), R2 is R+I, C2 is C-I, get_cell(Board,R2,C2,V)), List).

window(Board, List) :- window_row(Board,_,_,List) ; window_col(Board,_,_,List) ; window_diag1(Board,_,_,List) ; window_diag2(Board,_,_,List).

count_open3(Board, Player, Count) :- findall(1, (window(Board, List), is_open3(List, Player)), L), length(L, Count).
count_open2(Board, Player, Count) :- findall(1, (window(Board, List), is_open2(List, Player)), L), length(L, Count).

center_score(Board, Player, Score) :- findall(S, (between(0,5,R), between(0,6,C), get_cell(Board,R,C,Player), S is 4 - abs(C-3)), L), sum_list(L, Score).

playable_score(Board, Score) :- count_empty(Board, Count), Perc is Count / 42.0 * 100, (Perc >=60 -> Score=1000 ; Perc>=50 -> Score=5 ; Perc>=40 -> Score=4 ; Score=0).

count_empty(Board, Count) :- findall(1, (member(Row,Board), member(0,Row)), L), length(L, Count).

play(Player):-  
    write('New turn for: '),
    writeln(Player),
    board(Board),
    print_board(Board),
    playHumanMove(Board, NewBoard, Player),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) ),
        !
    ;
        changePlayer(Player,NextPlayer),
        play(NextPlayer)
    ).

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
    between(0,6,Col),
    last_index(Indices),
    nth0(Col, Indices, Row),
    Row < 6.

setup :- 
    retractall(board(_)),
    retractall(last_index(_)),
    generate_matrix(7,6,Board),
    length_list(7, Indices),
    maplist(=(0), Indices),
    assert(last_index(Indices)),
    assert(board(Board)).

start :-
    setup,
    main_menu.

main_menu :-
    writeln('Choose game mode:'),
    writeln('1 - Player vs Player'),
    writeln('2 - Player vs IA'),
    writeln('3 - IA vs IA'),
    writeln('q - Quit'),
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :-
    write('Starting Player vs Player...'), nl,
    play('x').
handle_choice(2) :-
    write('Starting Player vs IA...'), nl,
    get_ai_for_player('o', AI),
    play_pvai(AI, 'x').
handle_choice(3) :-
    write('Starting IA vs IA...'), nl,
    get_ai_for_player('x', AI1),
    get_ai_for_player('o', AI2),
    play_aivai(AI1, AI2, 'x').
handle_choice(q) :-
    writeln('Goodbye.').
handle_choice(_) :-
    writeln('Invalid choice, try again.'),
    main_menu.

play_pvai(AI, Player) :-  % Player 'x' = human, 'o' = IA
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 'x' ->
        playHumanMove(Board, NewBoard, Player)
    ;
        ( call(AI, Board, NewBoard, Player) -> true ; (writeln('AI failed, using random move'), iaRandom(Board, NewBoard, Player)) )
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) )
    ;
        changePlayer(Player,NextPlayer),
        play_pvai(AI, NextPlayer)
    ).

play_aivai(AI1, AI2, Player) :-  % Player 'x' uses AI1, 'o' uses AI2
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 'x' ->
        ( call(AI1, Board, NewBoard, Player) -> true ; (writeln('AI1 failed, using random move'), iaRandom(Board, NewBoard, Player)) )
    ;
        ( call(AI2, Board, NewBoard, Player) -> true ; (writeln('AI2 failed, using random move'), iaRandom(Board, NewBoard, Player)) )
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) )
    ;
        changePlayer(Player,NextPlayer),
        play_aivai(AI1, AI2, NextPlayer)
    ).

play_pvai_minimax(Player) :-  % Player 'x' = human, 'o' = IA Minimax
    write('New turn for: '), writeln(Player),
    board(Board),
    print_board(Board),
    ( Player = 'x' ->
        playHumanMove(Board, NewBoard, Player)
    ;
        iaMinimax(Board, NewBoard, Player)
    ),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, Result),
    ( Result \= 'no' ->
        ( Result = 'draw' -> writeln('It''s a draw!') ; format('Player ~w wins!~n', [Result]) )
    ;
        changePlayer(Player,NextPlayer),
        play_pvai_minimax(NextPlayer)
    ).

playHumanMove(Board,NewBoard,Player) :-
    read(Col),
    (   integer(Col), Col >= 1, Col =< 7
    ->  ColIndex is Col - 1,
        (   validMove(ColIndex)
        ->  playMove(Board, ColIndex, TmpBoard, Player),
            NewBoard = TmpBoard,
            write('Dropping in column '), write(Col), nl,
            last_index(UpdatedIdx),
            write('Updated Indices: '), writeln(UpdatedIdx),
            true
        ;   writeln('Column is full, pick another.'),
            playHumanMove(Board,NewBoard,Player)
        )
    ;   writeln('Invalid input, enter a number between 1 and 7.'),
        playHumanMove(Board,NewBoard,Player)
    ).