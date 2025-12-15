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

**Score : selon la grille ci-dessous**

| 3 | 4 | 5 | 7 | 5 | 4 | 3 |
| 4 | 6 | 8 | 10 | 8 | 6 | 4 |
| 5 | 8 | 11 | 14 | 11 | 8 | 5 |
| 5 | 8 | 11 | 14 | 11 | 8 | 5 |
| 4 | 6 | 8 | 10 | 8 | 6 | 4 |
| 3 | 4 | 5 | 7 | 5 | 4 | 3 |

## 3. Groupes de pièces connectées

**Score :**

- 2 pièces consécutives : +10 points
- 3 pièces consécutives : +100 points