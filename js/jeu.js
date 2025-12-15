
/**
 * Créé la grille du plateau. Elle ajoute les 7 colonnes du jeu au div 'jeu', puis créé les évenements d'ajout de pion.
 * 
 * @return {undefined}
 */
function initCases() {
    for (var a = 0; a < 7; a++)
    {
        var maColonne = document.createElement('div');
        maColonne.className = 'colonne';
        maColonne.id = a + '';
        for (var b = 0; b < 6; b++)
        {
            var maCase = document.createElement('div');
            maCase.className = 'case_vide';
            maCase.id = a + '' + b;
            maColonne.appendChild(maCase);
        }
        maColonne.addEventListener('click', eventColonne, false);
        jeu.appendChild(maColonne);
    }
}

/**
 * Au clique de la case, jouer le coup à la colonne choisie
 * 
 * @param {Event} e la case cliqué
 * @returns {undefined}
 */
function eventColonne(e) {
    jouer(e.target.parentNode.id);
}

/**
 * Desactive les evenements (en cas de fin de jeu)
 * 
 * @returns {undefined}
 */
function desactiverEvents() {
    var mesColonnes = document.getElementsByClassName('colonne');
    var taille = mesColonnes.length;
    for (var i = 0; i < taille; i++) {
        mesColonnes[i].removeEventListener('click', eventColonne, false);
    }
}

/**
 * Renvoi la 1ere position libre (en partant du haut) de la colonne donnée
 * 
 * @param {Number} colonne
 * @returns {Number} ligne de la case vide
 */
function ligneVide(colonne) {
    var ligne = 5;
    while (ligne >= 0 && grille[colonne][ligne] !== 0) {
        ligne--;
    }
    return ligne;
}

/**
 * Ajoute un pion à la colonne choisie, et l'ajoute dans la grille
 * 
 * @param {Number} joueur
 * @param {Number} colonne (facultatif si bot)
 * @returns {Boolean} Ajout reussi
 */
function ajouterPion(joueur, colonne) {
    var ligne = ligneVide(colonne);
    if (ligne >= 0) {
        grille[colonne][ligne] = joueur;
        var caseVide = document.getElementById(colonne + '' + ligne);
        if (joueur === 1) {
            caseVide.className = 'case_jaune';
        } else {
            caseVide.className = 'case_rouge';
        }
        return true;
    } else {
        return false;
    }
}

/**
 * Algorithme de déroulement du jeu
 * 
 * @param {Number} colonne
 * @returns {undefined}
 */
function jouer(colonne) {

    // Joueur 1
    if (joueurCourant === 1) {
        if (joueur1Bot) {
            ajouterPion(joueurCourant, jouerBot(joueurCourant, joueur1Algo));
        } else {
            if (!ajouterPion(joueurCourant, colonne)) {
                return;
            }
        }
        if (checkVictoire() === joueurCourant) {
            changerEtat(1, true);
            desactiverEvents();
        } else if (checkNul()) {
            changerEtat(0, true);
            desactiverEvents();
            return;
        } else {
            joueurCourant = 2;
            changerEtat(2, false);
            if (joueur2Bot) {
                jouer();
            }
        }
        // Joueur 2
    } else {
        if (joueur2Bot) {
            ajouterPion(joueurCourant, jouerBot(joueurCourant, joueur2Algo));
        } else {
            if (!ajouterPion(joueurCourant, colonne)) {
                return;
            }
        }
        if (checkVictoire() === joueurCourant) {
            changerEtat(2, true);
            desactiverEvents();
        } else if (checkNul()) {
            changerEtat(0, true);
            desactiverEvents();
            return;
        } else {
            joueurCourant = 1;
            changerEtat(1, false);
            if (joueur1Bot) {
                jouer();
            }
        }
    }
}

/**
 * Reinitialise la partie (grille + plateau)
 * 
 * @returns {undefined}
 */
function reinitialiser() {
    grille = [[0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0]];
    joueurCourant = 1;
    while (jeu.hasChildNodes()) {
        jeu.removeChild(jeu.firstChild);
    }
    initCases();
}

/**
 * Verifie si la grille est pleine
 * 
 * @returns {Boolean} match nul
 */
function checkNul() {
    var estNul = true;
    for (var colonne = 0; colonne < 7; colonne++) {
        estNul = estNul && (grille[colonne][0] !== 0);
    }
    return estNul;
}

/**
 * Verifie si quelqu'un a gagné la partie
 * 
 * @returns {Number} Joueur qui a gagné, 0 sinon
 */
function checkVictoire() {
    //Balayage horizontal _
    for (var colonne = 0; colonne <= 3; colonne++) {
        for (var ligne = 5; ligne >= 0; ligne--) {
            if (grille[colonne][ligne] !== 0
                    && grille[colonne][ligne] === grille[colonne + 1][ligne]
                    && grille[colonne][ligne] === grille[colonne + 2][ligne]
                    && grille[colonne][ligne] === grille[colonne + 3][ligne]) {
                return grille[colonne][ligne];
            }
        }
    }

    // Balayage verticale |
    for (colonne = 0; colonne <= 6; colonne++) {
        for (ligne = 5; ligne >= 3; ligne--) {
            if (grille[colonne][ligne] !== 0
                    && grille[colonne][ligne] === grille[colonne][ligne - 1]
                    && grille[colonne][ligne] === grille[colonne][ligne - 2]
                    && grille[colonne][ligne] === grille[colonne][ligne - 3]) {
                return grille[colonne][ligne];
            }
        }
    }

    // Balayage diagonale /
    for (colonne = 0; colonne <= 3; colonne++) {
        for (ligne = 5; ligne >= 3; ligne--) {
            if (grille[colonne][ligne] !== 0
                    && grille[colonne][ligne] === grille[colonne + 1][ligne - 1]
                    && grille[colonne][ligne] === grille[colonne + 2][ligne - 2]
                    && grille[colonne][ligne] === grille[colonne + 3][ligne - 3]) {
                return grille[colonne][ligne];
            }
        }
    }

    // Balayage diagonale \
    for (colonne = 6; colonne >= 3; colonne--) {
        for (ligne = 5; ligne >= 3; ligne--) {
            if (grille[colonne][ligne] !== 0
                    && grille[colonne][ligne] === grille[colonne - 1][ligne - 1]
                    && grille[colonne][ligne] === grille[colonne - 2][ligne - 2]
                    && grille[colonne][ligne] === grille[colonne - 3][ligne - 3]) {
                return grille[colonne][ligne];
            }
        }
    }
    return 0;
}