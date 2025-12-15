/**
 * Retourne la colonne jouée en fonction du joueur et de l'algorithme choisi
 * 
 * @param {Number} joueur
 * @param {Number} algorithme
 * @returns {Number} Colonne à jouer
 */
function jouerBot(joueur, algorithme) {
    var coup;

    // On commence toujours par jouer au milieu
    if (grille[3][5] === 0) {
        return 3;
    }

    if (algorithme == 1) {
        coup = coupNaif(joueur);
    }
    if (algorithme == 2) {
        coup = coupMinMax();
    }
    if (algorithme == 3) {
        coup = coupAlphaBeta();
    }
    return coup;
}

/**
 * Algorithme qui empeche un coup gagnant de l'adversaire, joue un coup gagnant, ou joue au hasard
 * 
 * @param {type} joueur
 * @returns {Number} 
 */
function coupNaif(joueur) {
    var joueurAdverse = (joueur === 1) ? 2 : 1;
    // Defaite au prochain tour
    for (var i = 0; i < 7; i++) {
        var ligne = ligneVide(i);
        if (ligne >= 0) {
            grille[i][ligne] = joueurAdverse;
            if (checkVictoire() === joueurAdverse) {
                grille[i][ligne] = 0;
                return i;
            }
            grille[i][ligne] = 0;
        }
    }

    // Victoire au prochain tour
    for (var i = 0; i < 7; i++) {
        var ligne = ligneVide(i);
        if (ligne >= 0) {
            grille[i][ligne] = joueur;
            if (checkVictoire() === joueur) {
                grille[i][ligne] = 0;
                return i;
            }
            grille[i][ligne] = 0;
        }
    }
    return Math.floor(Math.random() * (7));
}


