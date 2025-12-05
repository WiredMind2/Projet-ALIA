# Heuristiques IA – Puissance 4

Ce document décrit les heuristiques utilisées pour l'évaluation des coups dans l'Intelligence Artificielle du jeu Puissance 4.

## Résumé rapide

| Heuristique | Pattern | Score |
|-------------|---------|-------|
| **Coup gagnant** | `JJJJ` | +10000 |
| **3 alignés ouverts** | `JJJ_` | +15 |
| **2 alignés ouverts** | `JJ__` | +5 |
| **Position centrale** | Centre du plateau | +40 max |
| **Cases jouables (60%)** | 60% vides | +1000 |
| **Cases jouables (50%)** | 50% vides | +5 |
| **Cases jouables (40%)** | 40% vides | +4 |

---

## 1. Coups gagnants

**Score : +10000**

- Si le coup crée un alignement de **4 jetons consécutifs** (JJJJ), c'est un coup gagnant immédiat.
- Cette heuristique a la priorité maximale.

**Exemple :**
```
J J J J  ← Alignement gagnant
```

## 2. Alignements utiles

### 2.1 Trois jetons alignés ouverts

**Score : +15 par configuration**

- Configuration : `J J J _`
- Un alignement de 3 jetons avec une case libre adjacente
- Permet de créer une menace de victoire au prochain tour

**Exemple :**
```
J J J _
```

### 2.2 Deux jetons alignés ouverts

**Score : +5 par configuration**

- Configuration : `J J _ _`
- Un alignement de 2 jetons avec des cases libres adjacentes
- Base pour construire des alignements plus longs

**Exemple :**
```
J J _ _
```

## 3. Jeu proche du centre (stratégie positionnelle)

**Score : de +0 à +4 (proportionnel)**

- Plus le coup est joué près du centre du plateau, plus le bonus est élevé
- Le centre offre plus d'opportunités d'alignement (horizontal, vertical, diagonales)
- Formule proportionnelle à la distance au centre

## 4. Pourcentage de cases jouables / vides (minimum 4)

Cette heuristique évalue la "respiration" du jeu et pénalise les situations bloquées.

**Scoring selon le pourcentage de cases libres :**

| % de cases libres | Score     |
|-------------------|-----------|
| 60%               | +1000     |
| 50%               | +5        |
| 40%               | +4        |
| 30%               | +3        |
| 20%               | +2        |


