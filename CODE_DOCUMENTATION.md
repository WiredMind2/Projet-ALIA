# Connect Four AI Project - Code Documentation

## Overview

This Prolog project implements a complete Connect Four (Puissance 4) game with multiple AI implementations. The system features a sophisticated minimax algorithm with alpha-beta pruning, as well as simpler random-based AIs for comparison. Additionally, it includes a basic Tic-Tac-Toe (Morpion) implementation.

## Project Structure

```
├── puissance4.pro           # Main Connect Four game logic and menu system
├── morpion.prolog          # Tic-Tac-Toe game implementation
├── matrix.pro              # Matrix utility functions for board manipulation
├── print.pro               # Board display and visualization functions
├── win.pro                 # Win detection algorithms for Connect Four
├── ai_selector.pro         # AI selection system and configuration
├── ia_random.pro           # Legacy random AI (for backward compatibility)
├── ai/                     # AI implementation modules
│   ├── minimax.pro         # Minimax algorithm with alpha-beta pruning
│   ├── evaluation.pro      # Heuristic evaluation functions
│   ├── game.pro      # Shared game utilities for all AIs
│   ├── random/             # Random AI implementations
│   │   ├── random.pro      # Pure random AI
│   │   └── almost_random.pro # Smart random AI with win detection
│   ├── selector.pro        # Additional AI selection utilities
│   ├── test_minimax.pro    # Testing utilities for minimax
│   └── README_minimax.md   # Minimax algorithm documentation
└── js/                     # JavaScript reference implementations
    ├── alphaBeta.js        # JavaScript minimax reference
    ├── heuristique1.js     # JavaScript heuristic 1 reference
    ├── heuristique2.js     # JavaScript heuristic 2 reference
    ├── ia.js              # JavaScript AI controller
    ├── jeu.js             # JavaScript game logic
    └── puissance4.js      # JavaScript Connect Four reference
```

## Core Game Components

### 1. Board Representation

The game uses a 7×6 matrix (7 columns, 6 rows) represented as a list of lists:
- `0` represents empty cells
- `1` represents player 1 pieces (displayed as 'x')
- `2` represents player 2 pieces (displayed as 'o')

### 2. Game Flow

The main game flow follows this pattern:
1. **Initialization**: `setup/0` creates empty board and initializes state
2. **Menu Selection**: User chooses game mode (PvP, PvAI, AIvAI)
3. **Turn Loop**: Each player takes turns making moves
4. **Move Validation**: Validate column selection and gravity simulation
5. **Win Detection**: Check for horizontal, vertical, or diagonal wins
6. **Game Continuation**: Switch players or end game

### 3. Key Modules

#### `matrix.pro` - Board Operations
- `generate_matrix/3`: Creates empty board matrix
- `replace/4`: Replaces elements in lists
- `replaceMatrix/5`: Replaces cells in 2D matrix
- `sublist/2`: Checks for contiguous sublists

#### `print.pro` - Visual Display
- `print_board/1`: Main display function showing board state
- `print_cell/1`: Displays individual cells with appropriate symbols
- `print_row/1`: Prints rows with space separation
- `print_column_numbers/1`: Shows column numbers (0-6) below board

#### `win.pro` - Victory Detection
- `win/2`: Main win detection checking all directions
- `horizontal_win/2`: Checks horizontal 4-in-a-row
- `vertical_win/2`: Checks vertical 4-in-a-row  
- `diagonal_win/2`: Checks both diagonal directions
- `check_diagonal/6`: Generic diagonal checking with configurable direction

#### `ai/game.pro` - Shared Utilities
- `playMove/4`: Applies moves to board (stateful, for main game)
- `simulateMove/4`: Simulates moves without state changes (for AI)
- `validMove/2`: Checks if column is playable
- `get_next_open_row/3`: Finds next available row in column
- `changePlayer/2`: Switches between players
- `game_over/2`: Checks game end conditions

## AI Implementation

### 1. Random AI (`ai/random/random.pro`)
- **Strategy**: Completely random move selection
- **Behavior**: Uniformly selects any valid move
- **Use Case**: Baseline AI for testing, debugging

### 2. Smart Random AI (`ai/random/almost_random.pro`)
- **Strategy**: Priority-based decision making
- **Priority Order**:
  1. Take winning move if available
  2. Block opponent's immediate win
  3. Play randomly if no immediate threats
- **Use Case**: Better than random but still simple

### 3. Minimax AI (`ai/minimax.pro`)
- **Algorithm**: Minimax with Alpha-Beta pruning
- **Complexity**: Sophisticated tree search with evaluation
- **Components**:
  - **Search Depth**: Configurable (default: 5 moves ahead)
  - **Evaluation Functions**: Two different heuristics
  - **Alpha-Beta Pruning**: Optimization to reduce search space
  - **Dynamic Configuration**: Per-player depth and heuristic settings

### 4. Evaluation Functions (`ai/evaluation.pro`)

#### Heuristic 1: Position-Based
- Uses strategic position values favoring center columns
- `tableau_evaluation`: Position weights (center = 7-13, edges = 3-5)
- Calculates difference between player and opponent position values

#### Heuristic 2: Alignment-Based
- Counts potential winning alignments (2s and 3s)
- Weights: 3-in-a-row = 3 points, 2-in-a-row = 1 point
- Includes strategic bias (base score: 128)
- Considers extendability of alignments

### 5. Alpha-Beta Minimax Algorithm

#### Core Concepts:
- **Max Player**: Tries to maximize evaluation score
- **Min Player**: Tries to minimize evaluation score (opponent)
- **Alpha-Beta Pruning**: Eliminates branches that can't improve result
- **Terminal States**: Win/loss/draw conditions
- **Depth Limiting**: Prevents infinite recursion

#### Key Functions:
- `init_alpha_beta/4`: Initialize algorithm parameters
- `coup_alpha_beta/3`: Main entry point, returns best column
- `valeur_max_ab/7`: Maximizing player evaluation
- `valeur_min_ab/7`: Minimizing player evaluation
- `valeur_max_recursive/10`: Recursive column evaluation (max)
- `valeur_min_recursive/9`: Recursive column evaluation (min)

## Game Modes

### 1. Player vs Player
- Two human players take turns
- Interactive input for column selection
- Full move validation and feedback

### 2. Player vs AI
- Human player vs computer opponent
- AI selection menu for computer player
- Supports all AI types (random, smart random, minimax)

### 3. AI vs AI
- Computer vs computer gameplay
- Allows testing and comparing different AI strategies
- Educational for understanding AI behavior

## Input/Output System

### Input Validation:
- Column numbers: 1-7 (converted to 0-6 internally)
- Error handling for invalid inputs
- Retry prompts for invalid moves
- Full column detection and feedback

### Output Formatting:
- Visual board display with '.' (empty), 'x' (player 1), 'o' (player 2)
- Column numbers displayed below board
- Game status messages (wins, draws, current player)
- Move confirmation and column height tracking

## Error Handling

### AI Error Recovery:
- `catch/3` blocks around AI function calls
- Fallback to random AI if primary AI fails
- Error logging for debugging AI issues
- Graceful degradation to ensure game continues

### Input Error Handling:
- Invalid input detection and retry
- Out-of-bounds move protection
- Full column detection and prevention
- Clear error messages for user guidance

## Configuration and Customization

### Minimax Settings:
- Search depth: Configurable per player
- Heuristic selection: Per player preference
- Default: Depth 5, Heuristic 1 for both players

### AI Selection:
- Interactive menu system
- Multiple AI types available
- Per-game AI assignment
- Backward compatibility maintained

## Design Patterns and Best Practices

### 1. Separation of Concerns:
- Game logic separate from AI implementations
- Utilities shared across modules
- Clear interfaces between components

### 2. State Management:
- Dynamic predicates for game state
- Stateless functions for AI calculations
- Clean state transitions

### 3. Error Handling:
- Comprehensive input validation
- Graceful AI failure handling
- User-friendly error messages

### 4. Extensibility:
- Easy to add new AI strategies
- Configurable parameters
- Modular architecture

## Performance Considerations

### Minimax Optimization:
- Alpha-beta pruning significantly reduces search space
- Depth limiting prevents exponential blowup
- Efficient board representation for fast evaluation

### Memory Usage:
- Dynamic predicates for state management
- Efficient list-based board representation
- Minimal recursion depth for most operations

## Testing and Debugging

The codebase includes several features for testing and debugging:
- Error logging for AI failures
- Move validation at all levels
- Visual feedback for all game states
- Modular design allows isolated testing

## Usage Instructions

### Starting the Game:
```prolog
?- start.
```

### Playing Connect Four:
1. Choose game mode from menu
2. If AI mode, select AI type
3. Input column numbers (1-7) when prompted
4. Game automatically detects wins/draws

### AI Comparison:
- Use AI vs AI mode to compare strategies
- Configure different AI parameters
- Observe different playing styles and effectiveness

This comprehensive implementation provides a solid foundation for Connect Four gameplay with multiple AI approaches, from simple random strategies to sophisticated minimax algorithms with alpha-beta pruning.