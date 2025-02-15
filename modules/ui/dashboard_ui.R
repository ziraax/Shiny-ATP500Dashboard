dashboard_ui <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "dashboard",
    
    bs4TabCard(
      id = ns("dashboard_tabs"),
      title = "",
      status = "primary",
      solidHeader = TRUE,
      maximizable = TRUE,
      collapsible = FALSE,
      width = 12,
      type = "tabs", 
      
      ##
      # TABPANEL RESERVÉ AUX STATISTIQUES GLOBALES
      ## 
      tabPanel(
        title = "Vue d'ensemble",
        h4("Vue d'ensemble des statistiques."),
        
        
        ### CARD DES STATS GLOBALES
        bs4Card(
          
          title = "Statistiques générales",  # 🏆 Grande carte principale
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          # Value Boxes
          fluidRow(
            bs4ValueBoxOutput(ns("total_tournaments")),
            bs4ValueBoxOutput(ns("unique_winners")),
            bs4ValueBoxOutput(ns("total_matches"))
          ),
          
          # Graphiques interactifs
          fluidRow(
            column(6, plotlyOutput(ns("surface_distribution"))),
            column(6, plotlyOutput(ns("top_10_winners")))
          )
        ),
        
        ### CARD DES TENDANCES ET PERFORMANCES
        bs4Card(
          
          title = "2️⃣ Tendances & Performances",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          # Graphiques interactifs
          fluidRow(
            column(6, plotlyOutput(ns("nb_matchs_annees"))),
            column(6, plotlyOutput(ns("top_winners_by_year"))),
          ),
          
          fluidRow(
            column(6, plotlyOutput(ns("tournaments_per_year"))),
            column(6, plotlyOutput(ns("upsets_per_year"))),
          )
        ),
        
        
      ),
      
      tabPanel(
        title = "Analyse joueur",
        h4("Analyse détaillée d'un joueur."),
        selectInput(ns("player_select"), "Choisissez un joueur :", choices = NULL),
        textOutput(ns("player_stats"))
      ),
      
      tabPanel(
        title = "Analyse versus",
        h4("Analyse d'une rencontre."),
        textOutput(ns("match_stats"))
      ),
      
      tabPanel(
        title = "Analyse tournois",
        h4("Statistiques des tournois."),
        textOutput(ns("tournament_stats"))
      )
    )
  )
}
