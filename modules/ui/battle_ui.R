battle_ui <- function(id) {
  ns <- NS(id)

  
  bs4TabItem(
    tabName = "battle",
    bs4Card(
      title = "Duel de Joueurs ⚔️",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      "Choisissez deux joueurs pour prédire le vainqueur grâce à notre modèle de machine learning. 🤖",
      
      fluidRow(
        column(6, selectizeInput(ns("player_select_battle_1"), "👤 Choisissez le premier joueur :", choices = NULL, multiple = FALSE)),
        column(6, selectizeInput(ns("player_select_battle_2"), "👤 Choisissez le second joueur :", choices = NULL, multiple = FALSE))
      ),
      
      fluidRow(
        column(3, selectInput(ns("surface"), "Surface :", choices = c("Clay", "Grass", "Hard"))),
        column(3, selectInput(ns("series"), "Série :", choices = c("International", "Masters", "Grand Slam"))),
        column(3, selectInput(ns("best_of"), "Nombre de sets :", choices = c(3, 5))),
        column(3, selectInput(ns("round"), "Tour :", choices = c("1st Round", "2nd Round", "Quarterfinals", "Semifinals", "The Final")))
      ),
      
      fluidRow(
        column(12, align = "center", 
               actionButton(ns("predict_button"), "Prédire le vainqueur 🏆"))
      ),      
      
      br(), br(),
      
      h4("Résultat de la prédiction 📊"),
      
      fluidRow(
        column(5, uiOutput(ns("player1_info"))),  # Player 1 information
        column(2, uiOutput(ns("win_probability"))),  # Win probability
        column(5, uiOutput(ns("player2_info")))  # Player 2 information        
      )
    ),
    
    bs4Card(
      title = "Simulation de tournoi 🏟️",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      "Simuler un tournoi avec vos joueurs favoris ! 🎉",
      fluidRow(
        column(12, align = "center", 
               selectizeInput(ns("tournament_players"), "👥 Sélectionnez 8 joueurs :", 
                              choices = NULL, multiple = TRUE, 
                              options = list(maxItems = 8, placeholder = "Sélectionnez 8 joueurs")))
      ),
      fluidRow(
        column(12, align = "center", 
               actionButton(ns("start_tournament"), "Commencer le tournoi 🚀"))
      ),
      br(),
      # Affichage du bracket
      fluidRow(
        column(12, class = "bracket",
               uiOutput(ns("quarterfinals")),
               uiOutput(ns("semifinals")),
               uiOutput(ns("final")),
               uiOutput(ns("winner")))
      ),
      # Boutons pour simuler chaque étape du tournoi
      fluidRow(
        column(12, align = "center",
               actionButton(ns("simulate_quarterfinals"), "Simuler les quarts de finale 🎾"),
               actionButton(ns("simulate_semifinals"), "Simuler les demi-finales 🎾"),
               actionButton(ns("simulate_final"), "Simuler la finale 🏆"))
      )
    ),
    
    
    bs4Card(
      title = "Explications sur notre modèle",
      status = "gray",
      solidHeader = TRUE,
      width = 12,
      collapsed = TRUE,
      h4("Modèle de Régression Logistique"),
      p("Le modèle utilisé pour prédire le vainqueur d'un match de tennis est une régression logistique. Voici comment il fonctionne :"),
      tags$ul(
        tags$li("Le modèle prend en compte plusieurs caractéristiques des joueurs, telles que leur classement, leur pourcentage de victoires, et leurs performances sur différentes surfaces."),
        tags$li("Il utilise ces caractéristiques pour calculer la probabilité que chaque joueur remporte le match."),
        tags$li("La prédiction finale est basée sur ces probabilités.")
      ),
      h4("Équation du Modèle"),
      withMathJax(
        p("La régression logistique est définie par l'équation suivante :"),
        p("$$ P(Y=1) = \\frac{1}{1 + e^{-(\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + ... + \\beta_n X_n)}} $$"),
        p("Où :"),
        tags$ul(
          tags$li("$$ P(Y=1) $$ est la probabilité que le joueur 1 gagne le match."),
          tags$li("$$ \\beta_0, \\beta_1, ..., \\beta_n $$ sont les coefficients du modèle."),
          tags$li("$$ X_1, X_2, ..., X_n $$ sont les caractéristiques des joueurs.")
        )
      ),
      h4("Graphiques d'Ajustement du Modèle"),
      fluidRow(
        column(6, plotOutput(ns("cv_plot"))),  # Graphique de validation croisée
        column(6, plotOutput(ns("importance_plot")))  # Graphique d'importance des variables
      ),
      h4("Matrice de Confusion"),
      fluidRow(
        column(12, verbatimTextOutput(ns("conf_matrix")))  # Affichage de la matrice de confusion
      )
    )
  )
}
