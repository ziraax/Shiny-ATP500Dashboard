library(DT)

a_propos_ui <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "a_propos",
    
    # Bloc 1 : Présentation du projet
    bs4Card(
      title = "À propos",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      HTML("
        <p>Nous sommes trois étudiants en M1 MAS à l'université de Rennes et Rennes 2, et nous avions un projet
        portant sur la visualisation de données avec une application RShiny.</p>
        
        <p><strong>Équipe :</strong></p>
        <ul>
          <li>
            <a href='https://www.linkedin.com/in/hugo-walter-774960142' target='_blank'>
              <img src='https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png' 
              style='width:16px; height:16px; margin-right:5px; vertical-align:middle;'>
              Hugo Walter
            </a>
            <img src='hugo.jpg' style='width:30px; height:30px; border-radius:50%; margin-left:10px; vertical-align:middle;'>
          </li>
          <li>
            <a href='https://www.linkedin.com/in/marla-donnio-87039b238' target='_blank'>
              <img src='https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png' 
              style='width:16px; height:16px; margin-right:5px; vertical-align:middle;'>
              Marla Donnio
            </a>
            <img src='marla.jpg' style='width:30px; height:30px; border-radius:50%; margin-left:10px; vertical-align:middle;'>
          </li>
          <li>
            <a href='https://www.linkedin.com/in/elliot-rault-maisonneuve-904a31277' target='_blank'>
              <img src='https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png' 
              style='width:16px; height:16px; margin-right:5px; vertical-align:middle;'>
              Elliot Rault-Maisonneuve
            </a>
            <img src='elliot.jpg' style='width:30px; height:30px; border-radius:50%; margin-left:10px; vertical-align:middle;'>
          </li>
        </ul>

        <p>Ce projet de visualisation de données porte sur la thématique du tennis, car nous étions attirés 
        par un jeu de données qui semblait très intéressant à étudier. De plus, il existe peu de visualisations 
        de données sur le tennis, ce qui nous a encouragés à en créer une nous-mêmes.</p>

        <p>Notre jeu de données principal rassemble des informations sur les matchs de tennis, incluant :</p>
        <ul>
          <li>Le nom du tournoi</li>
          <li>La date</li>
          <li>Le type de tournoi (« Series »)</li>
          <li>Le type de court (intérieur ou extérieur)</li>
          <li>La surface (gazon, terre battue, etc.)</li>
          <li>Le tour du tournoi (« Round »)</li>
          <li>Les noms des deux joueurs</li>
          <li>Le vainqueur du match</li>
          <li>Le score</li>
          <li>Le classement des joueurs</li>
          <li>Le nombre de points ATP</li>
        </ul>

        <p>A partir de ce jeu de données, nous avons effectué plusieurs transformations comme :</p>
        <ul>
          <li>Le calcul du pourcentage de victoires par surface</li>
          <li>Le nombre total de victoires</li>
          <li>Le pourcentage global de victoires</li>
        </ul>

        <p>Enfin, nous avons enrichi notre base en ajoutant des informations complémentaires sur les joueurs 
        et les tournois, telles que :</p>
        <ul>
          <li>La date de naissance des joueurs</li>
          <li>La nationalité</li>
          <li>La main dominante</li>
          <li>Les coordonnées GPS des tournois</li>
        </ul>
      ")
    ),
    
    # Bloc 2 : Affichage des datasets
    bs4Card(
      title = "Données",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      HTML("<p><em>Affichage limité aux 30 premières lignes.</em></p>"),
      DTOutput(ns("dataset1_table")),
      br(),
      DTOutput(ns("dataset2_table"))
    )
  )
}
