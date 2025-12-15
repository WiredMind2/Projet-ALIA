var grille = new Array();
grille = [[0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0]];

var joueurCourant = 1;

// Parametres du jeu
var joueur1Nom = "Joueur";
var joueur2Nom = "Bot";

var joueur1Bot = false;
var joueur2Bot = false;

var joueur1Algo = 0;
var joueur2Algo = 0;

var joueur1Profondeur = 0;
var joueur2Profondeur = 0;

var joueur1Heuristique = 1;
var joueur2Heuristique = 1;


var jeu = document.getElementById('jeu');


/**
 * Fonction d'affichage de la grille pour debogage
 * 
 * @returns {undefined}
 */
function afficherGrille() {
    var resultat = "[";
    for (var l = 0; l < 6; l++) {
        resultat += "[";
        for (var c = 0; c < 7; c++) {
            resultat += grille[c][l] + " , ";
        }
        resultat += "]\n";
    }
    resultat += "]";
    console.log(resultat);
}

//(function main() {
//    initCases();
//    ajouterEcouteursInfos();
//    initMenusDeroulants();
//})();
