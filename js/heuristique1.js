var tableauEvaluation = [[3, 4, 5, 5, 4, 3],
    [4, 6, 8, 8, 6, 4],
    [5, 8, 11, 11, 8, 5],
    [7, 10, 13, 13, 10, 7],
    [5, 8, 11, 11, 8, 5],
    [4, 6, 8, 8, 6, 4],
    [3, 4, 5, 5, 4, 3]];

/**
 * Retourne une evaluation de la feuille en fonction du tableau d'evaluation
 * 
 * @param {type} joueur
 * @returns {Number} Evaluation
 */
function heuristique1(joueur) {
    var joueurAdverse = (joueur === 1) ? 2 : 1;
    var evaluation = 0;

    for (var col = 0; col < 7; col++) {
        for (var lig = 0; lig < 6; lig++) {
            if (grille[col][lig] === joueur) {
                evaluation += tableauEvaluation[col][lig];
            } else if (grille[col][lig] === joueurAdverse) {
                evaluation -= tableauEvaluation[col][lig];
            }
        }
    }
    return evaluation;
}