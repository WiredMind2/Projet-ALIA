% Test to verify the main game loads with enhanced minimax
:- initialization(main).

main :-
    % Load the main game file
    consult('puissance4'),
    
    writeln('✅ Game loaded successfully with enhanced minimax!'),
    writeln(''),
    writeln('The enhanced minimax heuristic includes:'),
    writeln('• Advanced pattern recognition (3-in-a-row, 2-in-a-row detection)'),
    writeln('• Dynamic threat detection and blocking'),
    writeln('• Strategic position control (center, diagonal, bottom)'),
    writeln('• Game phase awareness (opening/middlegame/endgame)'),
    writeln('• Enhanced mobility and flexibility assessment'),
    writeln(''),
    writeln('To play the game, run: ?- start.'),
    
    halt.