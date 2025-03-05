battle_ui <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "battle",
    bs4Card(
      title = "Duel de Joueurs",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      "Choisissez deux joueurs pour prédire le vainqueur grâce à notre modèle de machine learning. 🤖",
      
      fluidRow(
        column(6, selectizeInput(ns("player_select_battle_1"), "Choisissez le premier joueur :", choices = NULL, multiple = FALSE)),
        column(6, selectizeInput(ns("player_select_battle_2"), "Choisissez le second joueur :", choices = NULL, multiple = FALSE))
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
      title = "Simulation de tournoi",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      "Simuler un tournoi avec vos joueurs favoris ! 🎉"
    )
  )
}
