var colonneMax;

/**
 * Retourne la colonne determinée par l'algorithme Alpha Beta
 * 
 * @returns {Number} colonne à jouer
 */
function coupAlphaBeta() {
    valeurMaxAB(0, -10000, 10000);
    return colonneMax;
}

function valeurMaxAB(profondeur, alpha, beta) {
    // Verifie si victoire
    var victoire = checkVictoire();
    if (victoire !== 0) {
        if (victoire == joueurCourant) {
            return (1000 - profondeur);
        } else {
            return (-1000 + profondeur);
        }
    }

    // Verifie profondeur
    if ((joueurCourant == 1 && profondeur == joueur1Profondeur) || (joueurCourant == 2 && profondeur == joueur2Profondeur) || checkNul()) {
        if (joueurCourant == 1 && joueur1Heuristique == 1 || joueurCourant == 2 && joueur2Heuristique == 1) {
            return heuristique1(joueurCourant);
        } else {
            return heuristique2(joueurCourant);
        }
    }

    profondeur++;
    var colonne = 0;
    for (var i = 0; i < 7; i++) {
        var ligne = ligneVide(i);
        // Si la colonne est jouable
        if (ligne >= 0) {
            // On simule le coup
            grille[i][ligne] = joueurCourant;
            var valeur = valeurMinAB(profondeur, alpha, beta);
            if (valeur > alpha) {
                alpha = valeur;
                colonne = i;
            }
            // Retour à l'état antérieur
            grille[i][ligne] = 0;
            // Coupure
            if (alpha >= beta) {
                return alpha;
            }
        }
    }
    colonneMax = colonne;
    return alpha;
}

function valeurMinAB(profondeur, alpha, beta) {
    var joueurCourantAdverse = (joueurCourant === 1) ? 2 : 1;

    // Verifie si victoire
    var victoire = checkVictoire();
    if (victoire !== 0) {
        if (victoire == joueurCourant) {
            return (1000 - profondeur);
        } else {
            return (-1000 + profondeur);
        }
    }

    // Verifie profondeur
    if ((joueurCourant == 1 && profondeur == joueur1Profondeur) || (joueurCourant == 2 && profondeur == joueur2Profondeur) || checkNul()) {
        if (joueurCourant == 1 && joueur1Heuristique == 1 || joueurCourant == 2 && joueur2Heuristique == 1) {
            return heuristique1(joueurCourant);
        } else {
            return heuristique2(joueurCourant);
        }
    }

    profondeur++;
    for (var i = 0; i < 7; i++) {
        var ligne = ligneVide(i);
        // Si la colonne est jouable
        if (ligne >= 0) {
            // On simule le coup
            grille[i][ligne] = joueurCourantAdverse;
            var valeur = valeurMaxAB(profondeur, alpha, beta);
            if (valeur < beta) {
                beta = valeur;
            }
            // Retour à l'état antérieur
            grille[i][ligne] = 0;
            // Coupure
            if (beta <= alpha) {
                return beta;
            }
        }
    }
    return beta;
}
