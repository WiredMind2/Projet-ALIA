/**
 * Retourne une évaluation de la feuille en fonction du nombre de cases alignés par les joueurs
 * 
 * @param {type} joueur
 * @returns {Number} Evaluation
 */
function heuristique2(joueur) {
    var joueurAdverse = (joueur === 1) ? 2 : 1;
    var evaluation = 128;

    evaluation += nombreAlignement(3, joueur) * 3;
    evaluation -= nombreAlignement(3, joueurAdverse) * 3;
    evaluation += nombreAlignement(2, joueur) * 1;
    evaluation -= nombreAlignement(2, joueurAdverse) * 1;
    return evaluation;
}

/**
 * Retourne le nombre d'alignement passé en parametre d'un joueur
 * Alignement horizontal, vertical, diagonale gauche \ et diagonale droite /
 * 
 * @param {Number} alignement
 * @param {Number} joueur
 * @returns {Number} Nombre d'alignement
 */
function nombreAlignement(alignement, joueur) {
    var nombre = 0;
    var sens = {
        horizontal: {colonne: 1, ligne: 0},
        vertical: {colonne: 0, ligne: 1},
        diagonaleDroite: {colonne: -1, ligne: 1},
        diagonaleGauche: {colonne: 1, ligne: 1}
    }

    for (var colonne = 0; colonne < 7; colonne++) {
        nombre += nombreCasesAlignes(colonne, 0, sens.vertical, joueur, alignement);
        nombre += nombreCasesAlignes(colonne, 0, sens.diagonaleGauche, joueur, alignement);
        nombre += nombreCasesAlignes(colonne, 0, sens.diagonaleDroite, joueur, alignement);
    }
    for (var ligne = 0; ligne < 6; ligne++) {
        nombre += nombreCasesAlignes(0, ligne, sens.horizontal, joueur, alignement);
        nombre += nombreCasesAlignes(0, ligne, sens.diagonaleGauche, joueur, alignement);
        nombre += nombreCasesAlignes(6, ligne, sens.diagonaleDroite, joueur, alignement);
    }
    return nombre;
}

/**
 * Retourne le nombre de case alignées selon le sens et l'alignement choisi
 * 
 * @param {Number} colonne
 * @param {Number} ligne
 * @param {sens} sens
 * @param {Number} joueur
 * @param {Number} alignement
 * @returns {Number} Nombre de cases alignées
 */
function nombreCasesAlignes(colonne, ligne, sens, joueur, alignement) {
    var nombreAlignes = 0;
    var nombreCases = 0;
    var debutColonne, debutLigne;
    var caseAvant, caseApres;

    while (((colonne >= 0) && (colonne < 7)) && ((ligne >= 0) && (ligne < 6))) {
        // Si la case appartient au joueur
        if (grille[colonne][ligne] == joueur) {
            if (nombreCases === 0) { // Nouvel alignement
                debutColonne = colonne;
                debutLigne = ligne;
            }
            nombreCases++;
        } else { // Alignement rompu
            nombreCases = 0;
        }

        // Si on a atteint le nombre d'alignement,...
        if (nombreCases == alignement) {
            // ... on regarde si la case avant l'alignement ...
            if ((debutColonne - sens.colonne < 7 && debutColonne - sens.colonne >= 0) && (debutLigne - sens.ligne < 6 && debutLigne - sens.ligne >= 0)) {
                caseAvant = grille[debutColonne - sens.colonne][debutLigne - sens.ligne];
            }
            // ... ou la case après l'alignement ...
            if ((colonne + sens.colonne < 7 && colonne + sens.colonne >= 0) && (ligne + sens.ligne < 6 && ligne + sens.ligne >= 0)) {
                caseApres = grille[colonne + sens.colonne][ligne + sens.ligne];
            }
            // ... est jouable par le joueur
            if ((caseAvant == 0 || caseApres == 0) && (caseAvant != joueur && caseApres != joueur)) {
                nombreAlignes++;
                caseAvant = -1;
                caseApres = -1;
            }
        }
        colonne += sens.colonne;
        ligne += sens.ligne;
    }
    return nombreAlignes;
}