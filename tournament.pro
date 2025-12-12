% Tournament Engine - Simplified
:- consult('game.pro').
:- consult('ia.pro').

:- dynamic ai_config/3.
:- dynamic tournament_stat/4.

% Run a full tournament
run_tournament(ConfigFile) :-
    writeln('=== TOURNAMENT ==='),
    retractall(ai_config(_,_,_)),
    retractall(tournament_stat(_,_,_,_)),
    consult(ConfigFile),
    findall(Id, ai_config(Id, _, _), IDs),
    forall(ai_config(Id, _, _), assert(tournament_stat(Id, 0, 0, 0))),
    run_matches(IDs),
    display_rankings.

% Run all matches (round-robin)
run_matches(IDs) :-
    forall(
        (member(AI1, IDs), member(AI2, IDs), AI1 @< AI2),
        (run_match(AI1, AI2, 'x', R1),
         run_match(AI2, AI1, 'x', R2),
         record_result(AI1, AI2, R1),
         record_result(AI2, AI1, R2))
    ).

% Run a single match
run_match(AI1, AI2, Player, Result) :-
    setup,
    ai_config(AI1, T1, P1),
    ai_config(AI2, T2, P2),
    play_game(Player, AI1, T1, P1, AI2, T2, P2, Result).

% Play one game
play_game(Player, AI1, T1, P1, AI2, T2, P2, Result) :-
    board(Board),
    (Player = 'x' -> Type = T1, Params = P1 ; Type = T2, Params = P2),
    make_move(Board, NewBoard, Player, Type, Params),
    applyBoard(Board, NewBoard),
    game_over(NewBoard, R),
    (R \= 'no' ->
        (R = 'draw' -> Result = 'draw' ; R = 'x' -> Result = AI1 ; Result = AI2)
    ;
        changePlayer(Player, Next),
        play_game(Next, AI1, T1, P1, AI2, T2, P2, Result)
    ).

% Make AI move
make_move(Board, NewBoard, Player, random, _) :- iaRandom(Board, NewBoard, Player).
make_move(Board, NewBoard, Player, _, _) :- iaRandom(Board, NewBoard, Player).

% Record result
record_result(AI1, AI2, Result) :-
    (Result = AI1 -> update_stat(AI1, 1, 0, 0), update_stat(AI2, 0, 0, 1)
    ; Result = AI2 -> update_stat(AI2, 1, 0, 0), update_stat(AI1, 0, 0, 1)
    ; Result = 'draw' -> update_stat(AI1, 0, 1, 0), update_stat(AI2, 0, 1, 0)
    ; true).

% Update stats
update_stat(AI, W, D, L) :-
    retract(tournament_stat(AI, Wins, Draws, Losses)),
    NewW is Wins + W, NewD is Draws + D, NewL is Losses + L,
    assert(tournament_stat(AI, NewW, NewD, NewL)).

% Display rankings
display_rankings :-
    writeln(''),
    writeln('=== RESULTS ==='),
    findall([S,AI,W,D,L], (tournament_stat(AI,W,D,L), S is W*3+D), Stats),
    sort(0, @>=, Stats, Sorted),
    writeln('AI            | Wins | Draws | Losses | Score'),
    writeln('--------------|------|-------|--------|------'),
    forall(member([S,AI,W,D,L], Sorted),
           format('~w~t~13+ |  ~d   |   ~d   |   ~d    |  ~d~n', [AI,W,D,L,S])).
