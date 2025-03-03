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
      
      ##
      # TABPANEL RESERVÉ A L'ANALYSE VERSUS
      ## 
      tabPanel(
        title = "Analyse versus",
        h4("Analyse d'une rencontre."),
        fluidRow(
          column(6, selectizeInput(ns("player_select_1"), "Choisissez le premier joueur :", choices = NULL, multiple = FALSE)),
          column(6, selectizeInput(ns("player_select_2"), "Choisissez le second joueur :", choices = NULL, multiple = FALSE))
        ),

        fluidRow(
          column(3, div(style = "width: 100%", bs4ValueBoxOutput(ns("player1_wins"), width = "100%"))),
          column(3, div(style = "width: 100%", bs4ValueBoxOutput(ns("ranking1"), width = "100%"))),
          column(3, div(style = "width: 100%", bs4ValueBoxOutput(ns("player2_wins"), width = "100%"))),
          column(3, div(style = "width: 100%", bs4ValueBoxOutput(ns("ranking2"), width = "100%")))
        ),

        textOutput(ns("total_matches_text")),

        fluidRow(
          column(6, plotlyOutput(ns("ranking_evolution"))),  # Graphique d'évolution du classement
          column(6, plotlyOutput(ns("points_evolution")))    # Graphique d'évolution des points
        ),
        fluidRow(
          column(6, plotlyOutput(ns("surface_distribution_players"))),  # Graphique de répartition des surfaces
          column(6, plotlyOutput(ns("wins_by_surface")))       # Graphique du nombre de matchs gagnés par surface
        )
      ),
      
      ##
      # TABPANEL RESERVÉ A L'ANALYSE DES TOURNOIS
      ## 
      tabPanel(
        title = "Analyse tournois",
        h4("Visualisation cartographique des vainqueurs de tournois."),
        
        # Première ligne : Slider sur toute la largeur
        sliderInput(
          inputId = ns("date_slider"),
          label = "Sélectionnez une date :",
          min = as.Date(min(data$Date, na.rm = TRUE)),
          max = as.Date(max(data$Date), na.rm = TRUE),
          value = as.Date("2010-01-01"),
          timeFormat = "%Y-%m-%d",
          animate = FALSE,
          width = "100%"  # slider occupe toute la largeur
        ),
        
        # Deuxième ligne : Menu checkbox et carte
        fluidRow(
          
          # Colonne filtres 
          column(3,  # 3/12 
                 # Sélection des types de tournoi
                 checkboxGroupInput(ns("tournament_types"), 
                                    "Sélectionnez les types de tournoi",
                                    choices = c("Grand Slam", "International", "International Gold", 
                                                "Masters", "Masters Cup", "ATP250", 
                                                "ATP500", "Masters 1000"),  
                                    selected = c("Grand Slam", "International", "International Gold", 
                                                 "Masters", "Masters Cup", "ATP250", "ATP500", "Masters 1000"),  # Par défaut, tous sélectionnés
                                    inline = FALSE),
                 
                 # Sélection des types de surface
                 checkboxGroupInput(ns("surface_types"), 
                                    "Sélectionnez les types de surface",
                                    choices = c("Hard", "Grass", "Clay", "Carpet"),  # Liste des surfaces
                                    selected = c("Hard", "Grass", "Clay", "Carpet"),  # Par défaut, toutes sélectionnées
                                    inline = FALSE)
          ),
          
          # Colonne droite carte 
          column(9,  # 9/12 largeur
                 leafletOutput(ns("tournament_map"), height = 600)  # Carte Leaflet
          )
        )
      )
      
      
    )
  )
}
