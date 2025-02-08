# ATP500 Dashboard Project

Ce projet utilise des données des tournois ATP pour effectuer une analyse exploratoire et des modèles prédictifs. L'application finale sera construite avec R Shiny.

## Structure du projet
- `data/`: Contient le jeu de données brut.
- `app.R`: Responsable lancer l'application shiny et du layout global (sidebar, footer, ...)
- `modules/`: Contient les différentes briques de l'UI et des server de l'app shiny

L'application est separée en plusieurs modules qui ont chacun un fichier server et ui dédiés afin de faciliter le developpement. 

## Installation
Le projet utilise `renv` pour gérer les dépendances. 
Pour installer `renv` :
```R
install.packages("renv")
```

Pour restaurer l'environnement :
```R
renv::restore()
```

Pour lancer l'app :
```R
shiny::app()
```

## Documentation 

Le projet utilise la librairie `bs4Dash`, lien vers la doc :
[R Documentation](https://www.rdocumentation.org/packages/bs4Dash/versions/2.3.4)
[Github bs4Dash](https://github.com/RinteRface/bs4Dash)
[Une ptite video sympa qui présente bs4Dash](https://www.youtube.com/watch?v=LY6K_GD4ypc)




