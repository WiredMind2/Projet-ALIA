# Heuristiques IA – Puissance 4

Ce document décrit les heuristiques utilisées pour l'évaluation des coups dans l'Intelligence Artificielle du jeu Puissance 4.

## 1. Coups gagnants

**Score : +10000**

- Si le coup crée un alignement de **4 jetons consécutifs** (JJJJ), c'est un coup gagnant immédiat.
- Cette heuristique a la priorité maximale.

**Exemple :**
```
J J J J  ← Alignement gagnant
```

## 2. Jeu proche du centre (stratégie positionnelle)

**Score : de +0 à +4 (proportionnel)**

- Plus le coup est joué près du centre du plateau, plus le bonus est élevé
- Le centre offre plus d'opportunités d'alignement (horizontal, vertical, diagonales)
- Formule proportionnelle à la distance au centre

## 3. Pourcentage de cases vides/jouées entre 2 jetons de l'adversaire (minimum 4)

Entre deux jetons de l'adversaire, le pourcentage de jetons du joueur par rapport aux cases libres est évalué. Entre les deux jetons de l'adversaire l'espace doit être d'au moins 4 cases. 

**Scoring selon le pourcentage de cases libres :**

| % de cases libres | Score     |
|-------------------|-----------|
| 60%               | +1000     |
| 50%               | +5        |
| 40%               | +4        |
| 30%               | +3        |
| 20%               | +2        |


