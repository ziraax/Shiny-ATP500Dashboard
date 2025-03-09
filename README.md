# Visualisation de données sur le tennis

**Projet de visualisation de données**  
*Master 1 MAS Rennes 1 & Rennes 2*  
Elliot RAULT-MAISONNEUVE, Hugo WALTER, Marla DONNIO  
Date : 2025-03-09

## Description

Plongez au cœur du tennis avec notre dashboard de data visualisation !

Le tennis est un sport de passion, de stratégie et de performances, pourtant l’analyse approfondie des données du circuit ATP reste rare. C’est pourquoi nous avons décidé de créer une application interactive qui met en lumière les tendances, les performances et les dynamiques du tennis professionnel.

Vous pouvez accéder à l'application via le lien suivant :  
[**Lien vers l’application**](https://elliotrm.shinyapps.io/TennisDashboard/)

## Une base de données riche

Notre projet repose sur un jeu de données vaste, rassemblant **64 411 matchs** entre joueurs du circuit ATP sur la période **2000-2025**. Pour chaque rencontre, nous avons collecté des informations clés :

- Le tournoi, la date, la surface (terre battue, gazon, dur…)
- Les joueurs, le vainqueur, le score, le classement ATP
- Détails sur les joueurs (nationalité, main dominante, date de naissance)
- Détails sur les tournois (coordonnées GPS, catégorie)

Ces données nous ont permis d’analyser le circuit ATP sous différentes perspectives :

- Analyser les performances d’un joueur
- Analyser les rencontres entre deux joueurs
- Modèle de prédiction de duel dans l'onglet **“Battle”**
- Visualisation des tournois sur une carte temporelle
- Modèle de clustering non supervisé (k-means) pour visualiser les différences entre les classes de joueurs présupposées

## Découverte de l’application

### Vue d'ensemble

Dès l’ouverture de l’application, une page d’accueil immersive donne un premier aperçu de la base de données, incluant :

- Le nombre total de matchs enregistrés
- Le nombre de joueurs uniques
- La période couverte par les données

L’application se divise en trois grandes sections, chacune offrant une approche unique de l’analyse du tennis.

### A - Le Dashboard : une immersion dans les statistiques du tennis

1. **Vue d’ensemble : un panorama du circuit ATP**
   - Nombre total de matchs joués
   - Nombre de vainqueurs uniques
   - Nombre de tournois organisés à travers le monde

2. **Analyse joueur : zoom sur une carrière**
   - Nationalité, main dominante, classement actuel
   - Nombre de matchs joués et pourcentage de victoires par surface
   - Participation et victoires aux tournois majeurs (Grand Chelems, Masters 1000, etc.)

3. **Analyse Versus : qui domine qui ?**
   - Comparaison directe entre deux joueurs
   - Nombre de victoires de chacun, classement actuel
   - Evolution du classement ATP
   - Distribution des matchs joués par surface

4. **Analyse des tournois : le tennis autour du globe**
   - Carte interactive des tournois ATP
   - Sélecteurs de date et type de tournoi
   - Informations sur chaque tournoi, incluant le vainqueur de l’année

### B - Clustering : segmentation des joueurs

Le clustering est utilisé via la méthode **k-means** pour regrouper les joueurs selon leurs performances. L’utilisateur peut :

- Définir le nombre de clusters
- Sélectionner les variables pour représenter les joueurs clusterisés

Cela permet d’identifier des groupes homogènes : légendes du Big Four, joueurs du top 10 à 50, outsiders…

Nous avons aussi utilisé une **ACP (Analyse en Composantes Principales)** pour réduire la dimension des données et proposer une visualisation complète sur 2 et 3 axes.

### C - Battle : prédictions de rencontres

En sélectionnant deux joueurs, l’application estime la probabilité de victoire de chacun grâce à un modèle de régression logistique. L’outil permet de choisir le contexte du match : surface, série, nombre de sets, et étape du tournoi.

Vous trouverez un onglet **“Explications sur notre modèle”** pour plus de détails sur la modélisation.

L’outil idéal pour anticiper le résultat d’un match.  
Enfin, un **onglet “Simulation de tournoi”** permet de simuler un arbre de tournoi (à partir des quarts de finale).

## Conclusion

À travers ce projet, nous avons pu découvrir en profondeur la visualisation de données avec **R Shiny**. La mise en place de méthodes de gestion de projet, notamment **GitHub**, nous a permis de réaliser l’application dans les temps. De plus, nous avons eu la chance de nous rencontrer régulièrement pour évoquer les différentes fonctionnalités à mettre en place.

Notre application ne se contente pas de présenter des chiffres : elle raconte une histoire, celle des joueurs, des tournois et des affrontements qui ont marqué l’histoire du tennis. Que vous soyez analyste, fan ou simple curieux, ce dashboard vous offre une vision globale de l’univers du tennis professionnel.

Cependant, avec plus de temps, nous aurions pu développer de meilleurs modèles et les tester de manière plus rigoureuse. De plus, dans le temps imparti, nous devions faire des choix quant aux visualisations proposées, mais d’autres pistes auraient pu être explorées.

Avec des outils puissants comme le **clustering**, les **prédictions de duel** et les **cartes interactives**, nous vous invitons à naviguer à travers les données et à découvrir des insights précieux sur ce sport.

Accédez à l’application dès maintenant :  
[**Lien vers l’application**](https://elliotrm.shinyapps.io/TennisDashboard/)

---

### Installation et Pré-requis

Pour installer et exécuter l'application localement, veuillez suivre ces étapes :

1. Clonez ce repository sur votre machine :
   ```bash
   git clone https://github.com/ziraax/Shiny-ATP500Dashboard.git
   ```

2. Installez renv si ce n'est pas déjà fait. Vous pouvez l'installer depuis R avec la commande suivante :
    ```bash
    install.packages("renv")
    ```

3. Initialisez renv dans le répertoire du projet :
    ```bash
    renv::restore()
    ```

4. Lancez l'application avec la commande suivante : 
    ```
    shiny::runApp("app")
    ```

### Licence

Ce projet est sous **licence MIT**. 




