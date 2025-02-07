# ATP500 Dashboard Project

Ce projet utilise des données des tournois ATP500 pour effectuer une analyse exploratoire et des modèles prédictifs. L'application finale sera construite avec R Shiny.

## Structure du projet
- `data/`: Contient les jeux de données bruts et nettoyés.
- `scripts/`: Scripts pour l'EDA, le nettoyage des données, et le modeling.
- `outputs/`: Résultats de l'analyse, graphiques, etc.

## Installation
Le projet utilise `renv` pour gérer les dépendances. Pour restaurer l'environnement :
```R
renv::restore()
```