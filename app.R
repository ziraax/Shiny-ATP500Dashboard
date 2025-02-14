library(shiny)
library(bs4Dash)
library(tidyverse)

# Importer les modules
source("modules/ui/home_ui.R")
source("modules/ui/dashboard_ui.R")
source("modules/ui/clustering_ui.R")
source("modules/ui/battle_ui.R")

source("modules/server/home_server.R")
source("modules/server/dashboard_server.R")
source("modules/server/clustering_server.R")
source("modules/server/battle_server.R")

dataset <- read_csv("data/atp_tennis.csv")

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
      bs4SidebarMenuItem("Battle", tabName = "battle", icon = icon("crosshairs"))
    )
  ),
  
  # Onglets
  body = bs4DashBody(
    bs4TabItems(
      home_ui("home_ui"),
      dashboard_ui("dashboard_ui"),
      clustering_ui("clustering_ui"),
      battle_ui("battle_ui")
    )
  )
)

# Serveur principal
server <- function(input, output, session) {
  # Appels des modules (avec moduleServer)
  home_server("home_ui", dataset)
  dashboard_server("dashboard_ui", dataset)
  clustering_server("clustering_ui")
  battle_server("battle_ui")
}

# Exécuter l'app
shinyApp(ui, server)