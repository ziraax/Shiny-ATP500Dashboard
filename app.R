library(shiny)
library(bs4Dash)
library(tidyverse)
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(lubridate)
library(DT)

# Importer les modules
source("modules/ui/home_ui.R")
source("modules/ui/dashboard_ui.R")
source("modules/ui/clustering_ui.R")
source("modules/ui/battle_ui.R")
source("modules/ui/a_propos_ui.R")

source("modules/server/home_server.R")
source("modules/server/dashboard_server.R")
source("modules/server/clustering_server.R")
source("modules/server/battle_server.R")
source("modules/server/a_propos_server.R")


dataset <- read_csv("data/atp_tennis.csv")
player_stat <- read_csv("data/players_stats.csv")

# UI principale
ui <- bs4DashPage(
  title = "ATP Tennis Dashboard 🎾",
  fullscreen = TRUE,
  
  # Header
  header = bs4DashNavbar(
    skin = "light",
    status = "primary",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("th"),
    
    title = bs4DashBrand(
      title = "🎾 ATP Tennis Dashboard",
      color = "primary",
      href = "#",
      image = NULL
    )
  ),
  
  # Sidebar pour la navigation
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "primary",
    title = "ATP500 Insights",
    brandColor = "primary",
    
    bs4SidebarMenu(
      bs4SidebarMenuItem("Home", tabName = "home", icon = icon("home")),
      bs4SidebarMenuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      bs4SidebarMenuItem("Clustering", tabName = "clustering", icon = icon("project-diagram")),
      bs4SidebarMenuItem("Battle", tabName = "battle", icon = icon("crosshairs")),
      bs4SidebarMenuItem("A propos", tabName="a_propos", icon= icon("info-circle"))
    )
  ),
  
  # Onglets
  body = bs4DashBody(
    # Charger le CSS personnalisé
    tags$head(
      tags$style(HTML("
        .bracket {
          display: grid !important;
          grid-template-columns: repeat(4, 1fr) !important; /* 4 colonnes pour chaque round */
          gap: 20px !important; /* Espacement entre les colonnes */
          justify-content: center !important;
          align-items: center !important;
          width: 100% !important;
        }
        
        .round {
          display: flex !important;
          flex-direction: column !important;
          align-items: center !important;
          width: 100% !important;
        }
        
        .match {
          border: 2px solid #ddd !important;
          border-radius: 8px !important;
          padding: 12px !important;
          margin: 20px 0 !important;
          background-color: #f8f9fa !important;
          text-align: center !important;
          font-weight: bold !important;
          width: 200px !important; /* Largeur uniforme pour tous les matchs */
        }
        
        .match-winner {
          background-color: #28a745 !important;
          color: white !important;
        }
      
        .connector {
          position: relative !important;
          width: 50px !important;
          height: 20px !important;
        }
        
        .connector::before, .connector::after {
          content: '' !important;
          position: absolute !important;
          width: 50% !important;
          height: 2px !important;
          background-color: #ddd !important;
        }
      
        .connector::before {
          top: 0 !important;
          left: 50% !important;
        }
        
        .connector::after {
          bottom: 0 !important;
          left: 50% !important;
        }
      "))
    ),
    bs4TabItems(
      home_ui("home_ui"),
      dashboard_ui("dashboard_ui"),
      clustering_ui("clustering_ui"),
      battle_ui("battle_ui"),
      a_propos_ui("a_propos_ui")
    )
  )
)

# Serveur principal
server <- function(input, output, session) {
  # Appels des modules (avec moduleServer)
  home_server("home_ui", dataset)
  dashboard_server("dashboard_ui", dataset)
  clustering_server("clustering_ui", player_stat)
  battle_server("battle_ui", dataset, player_stat)
  a_propos_server("a_propos_ui", dataset, player_stat)
  
}

# Exécuter l'app
shinyApp(ui, server)