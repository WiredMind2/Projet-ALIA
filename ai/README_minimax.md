# Minimax with Alpha-Beta Pruning Implementation

This document describes the Prolog implementation of the minimax algorithm with alpha-beta pruning, ported from the JavaScript implementation in `js/alphaBeta.js`.

## Overview

The implementation provides a complete minimax algorithm with alpha-beta pruning for Connect Four AI players. It includes:

- **Recursive minimax evaluation** with alpha-beta pruning optimization
- **Per-player configuration** for depth and heuristic selection
- **Terminal state handling** (wins, draws, depth limits)
- **Proper board representation** using the existing game utilities
- **Alpha-beta pruning cuts** for performance optimization

## Core Predicates

### `coup_alpha_beta/3`
```prolog
coup_alpha_beta(Board, CurrentPlayer, Column)
```
Main function that returns the optimal column for the current player.

**Parameters:**
- `Board`: Current game board (7x6 matrix)
- `CurrentPlayer`: Player number (1 or 2)
- `Column`: Output - chosen column (0-6)

**Example:**
```prolog
% Get optimal move for player 1
coup_alpha_beta(Board, 1, Column).
```

### `init_alpha_beta/4`
```prolog
init_alpha_beta(Player1Depth, Player1Heuristic, Player2Depth, Player2Heuristic)
```
Initialize algorithm parameters for both players.

**Parameters:**
- `Player1Depth`: Search depth for player 1
- `Player1Heuristic`: Heuristic (1 or 2) for player 1
- `Player2Depth`: Search depth for player 2
- `Player2Heuristic`: Heuristic (1 or 2) for player 2

**Example:**
```prolog
% Player 1: depth 4, heuristic 1
% Player 2: depth 4, heuristic 2
init_alpha_beta(4, 1, 4, 2).
```

### `valeur_max_ab/7`
```prolog
valeur_max_ab(Board, Depth, Alpha, Beta, Player, Value, NewAlpha)
```
Recursive max function with alpha-beta pruning for the maximizing player.

### `valeur_min_ab/7`
```prolog
valeur_min_ab(Board, Depth, Alpha, Beta, Player, Value, NewBeta)
```
Recursive min function with alpha-beta pruning for the minimizing player.

## Algorithm Details

### Board Representation
- **Format**: `grille[colonne][ligne]` where columns are 0-6 and rows are 0-5 (bottom to top)
- **Empty cells**: 0
- **Player 1**: 1
- **Player 2**: 2

### Terminal States
1. **Victory Check**: Uses existing `win/2` predicate to detect 4-in-a-row
2. **Depth Limits**: Configurable per player via `init_alpha_beta/4`
3. **Draw Detection**: Uses `board_full/1` to detect full board

### Alpha-Beta Pruning
- **Max player**: Prunes when `alpha >= beta`
- **Min player**: Prunes when `beta <= alpha`
- **Initial values**: Alpha = -10000, Beta = 10000

### Heuristic Evaluation
- **Heuristic 1**: Position-based evaluation using `tableau_evaluation`
- **Heuristic 2**: Alignment-based evaluation counting 2 and 3-in-a-row patterns

### Victory Scoring
- **Winning player**: `1000 - depth` (faster wins get higher scores)
- **Losing player**: `-1000 + depth` (delayed losses are better)

## Global State Variables

The implementation uses dynamic predicates to maintain state:

- `colonne_max/1`: Best column found
- `joueur_courant/1`: Current player
- `joueur1_profondeur/1`: Player 1's search depth
- `joueur2_profondeur/1`: Player 2's search depth
- `joueur1_heuristique/1`: Player 1's heuristic type
- `joueur2_heuristique/1`: Player 2's heuristic type

## Usage Example

```prolog
% Load the module
:- consult('ai/minimax.pro').

% Initialize game
setup.

% Set up alpha-beta parameters
init_alpha_beta(3, 1, 3, 2).

% Get the current board state
board(Board).

% Get optimal move for current player
coup_alpha_beta(Board, 1, Column).

% Column contains the recommended column (0-6)
```

## Testing

Use the test file to verify functionality:

```prolog
% Load test file
:- consult('ai/test_minimax.pro').

% Run basic tests
test_all.
```

## Performance Considerations

1. **Search Depth**: Higher depths provide better moves but increase computation time exponentially
2. **Alpha-Beta Pruning**: Significantly reduces the search space while maintaining optimality
3. **Heuristic Selection**: Different heuristics may perform better in different game situations
4. **Board State**: The algorithm works with any valid Connect Four board state

## Integration with Existing Code

The implementation integrates with:
- `ai/game.pro`: Board manipulation and validation
- `ai/evaluation.pro`: Heuristic evaluation functions
- `win.pro`: Victory detection
- `matrix.pro`: Matrix operations

## Comparison with JavaScript Version

The Prolog implementation maintains functional equivalence with `js/alphaBeta.js`:

- **Same algorithm structure**: Max-min alternation with alpha-beta pruning
- **Same terminal conditions**: Victory, depth limits, and draw detection
- **Same scoring system**: 1000-point victory scale with depth adjustment
- **Same per-player configuration**: Independent depth and heuristic settings
- **Same board representation**: Column-row indexing (0-6, 0-5)

The main differences are:
- **Language syntax**: Prolog vs JavaScript
- **State management**: Dynamic predicates vs global variables
- **Recursion style**: Prolog backtracking vs explicit loops