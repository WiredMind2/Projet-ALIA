# Puissance 4

Console Connect Four in SWI-Prolog, built for the **ALIA** course at **INSA Lyon** (Hexanome 13).

The game is a standard 7×6 board with gravity and four-in-a-row wins. You can play human vs human, human vs AI, or AI vs AI, run a round-robin tournament between configured strategies, and execute a Prolog test suite.

## Features

- 7×6 board with gravity (tokens drop to the lowest empty cell)
- Win detection: horizontal, vertical, and both diagonals
- Draw detection when the board is full
- ANSI-colored console board (`x` red, `o` yellow)
- Four modes: Player vs Player, Player vs AI, AI vs AI, and tournament
- AI strategies: random, almost-random (win/block), minimax, and minimax with alpha-beta pruning

## Requirements

- [SWI-Prolog](https://www.swi-prolog.org/) (uses `library(random)`, `library(lists)`, and `ansi_format/3`)
- No extra packs or dependencies

GNU Prolog is not supported.

## Quick start

From the SWI-Prolog REPL, in the project directory:

```prolog
?- consult('puissance4.pro').
?- start.
```

From a shell:

```bash
swipl -s puissance4.pro
```

Then run `start.` at the Prolog prompt.

## How to play

Human moves are column numbers **1–7**, entered with Prolog `read/1` (type the number followed by a period, e.g. `4.`).

Players:

- `'x'` — red; the human always plays first in Player vs AI
- `'o'` — yellow

### Main menu

| Choice | Mode |
|--------|------|
| `1` | Player vs Player |
| `2` | Player vs IA (you play first) |
| `3` | IA vs IA |
| `4` | Run tournament |
| `q` | Quit |

### Player vs AI submenu

| Choice | AI |
|--------|----|
| `1` | Random |
| `2` | Better Random (takes a winning move or blocks a loss, otherwise random) |
| `3` | Minimax (depth 1–7, no pruning) |
| `4` | Minimax with alpha-beta pruning (depth 1–7) |
| `q` | Back to main menu |

AI vs AI pits `iaRandom` (`x`) against `iaPresqueRandom` (`o`).

## AI strategies

| Strategy | Predicate | Behaviour |
|----------|-----------|-----------|
| Random | `iaRandom/3` | Uniform choice among valid columns |
| Better Random | `iaPresqueRandom/3` | Win if possible, else block the opponent, else random |
| Minimax | `iaMinimax/5` | Depth-limited search; optional alpha-beta pruning |

Minimax leaf evaluation uses win / loss / draw scores plus a **center-weighted position table** (`evaluation_board/1` in `minimax.pro`). Cells nearer the centre score higher because they participate in more potential alignments. Threat detection (open threes) is implemented in the same file but is currently disabled in the live score.

Call shape:

```prolog
iaMinimax(Board, NewBoard, Player, Depth, UseAlphaBeta).
```

`UseAlphaBeta` is `true` or `false`. Interactive play limits depth to 1–7.

## Tournament

Menu option `4` runs `run_tournament('tournament_config.pl')`: a round-robin where every pair of AIs plays twice (each colour once). Ranking uses 3 / 1 / 0 points for win / draw / loss.

Edit [`tournament_config.pl`](tournament_config.pl) to change the field:

```prolog
% ai_config(Id, Type, Parameters).
% Type: random | presque_random | minimax
% Minimax parameters: depth(N), alphabeta(true/false)

ai_config(random1, random, []).
ai_config(presque_random1, presque_random, []).
ai_config(mm_d2_noab, minimax, [depth(2), alphabeta(false)]).
ai_config(mm_d4_ab, minimax, [depth(4), alphabeta(true)]).
```

The default config already includes random, presque-random, minimax without pruning at depths 2 and 3, and minimax with alpha-beta at depths 3, 4, and 5.

## Tests

```prolog
?- consult('test.pro').
?- run_all_tests.
```

Suites covered:

- Matrix helpers (`generate_matrix`, `replace`, `replaceMatrix`, `get_item_2d`)
- Game mechanics (`setup`, `validMove`, `playMove`, `changePlayer`)
- Win and draw conditions
- Random, presque-random, and minimax AIs (legal moves, forced wins, blocks)

## Project structure

| File | Role |
|------|------|
| [`puissance4.pro`](puissance4.pro) | Entry point, main menu, AI picker |
| [`game.pro`](game.pro) | Board state, moves, `setup/0` |
| [`modes.pro`](modes.pro) | Player vs Player / AI / AI vs AI loops |
| [`ia.pro`](ia.pro) | Random and presque-random AIs |
| [`minimax.pro`](minimax.pro) | Minimax, alpha-beta, position evaluation |
| [`win.pro`](win.pro) | Four-in-a-row and `game_over/2` |
| [`matrix.pro`](matrix.pro) | 2D list helpers |
| [`print.pro`](print.pro) | ANSI console rendering |
| [`tournament.pro`](tournament.pro) | Round-robin engine and rankings |
| [`tournament_config.pl`](tournament_config.pl) | Tournament AI roster |
| [`test.pro`](test.pro) | Test harness and suites |

## Team

Hexanome 13:

- Andy Gonzales
- Jason Laval
- Elise Bachet
- Louis Labory
- Lou Reina--Kuntziger
- William Michaud
