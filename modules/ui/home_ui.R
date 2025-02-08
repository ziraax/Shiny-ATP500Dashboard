home_ui <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "home",
    
    fluidRow(
      bs4Card(
        title = "Bienvenue sur le dashboard de l'ATP",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        "Explorez les statistiques, comparez les joueurs et visualisez les insights clés des tournois de tennis."
      )
    ),
    
    fluidRow(
      bs4ValueBox(
        value = textOutput(ns("total_matches")),
        subtitle = "Matchs enregistrés",
        icon = icon("table-tennis-paddle-ball"),
        color = "primary",
        width = 4
      ),
      bs4ValueBox(
        value = textOutput(ns("unique_players")),
        subtitle = "Joueurs uniques",
        icon = icon("user"),
        color = "info",
        width = 4
      ),
      bs4ValueBox(
        value = textOutput(ns("date_range")),
        subtitle = "Période couverte",
        icon = icon("calendar-alt"),
        color = "success",
        width = 4
      )
    ),
    
    bs4Carousel(
      id = ns("carousel1"),
      indicators = TRUE,
      width = 12,

      
      bs4CarouselItem(
        active = TRUE,
        caption = "Explorez les statistiques des plus grands tournois ATP.",
        tags$img(src = "ATP_logo.jpg", style = "width:100%; height:auto;")
      ),
      bs4CarouselItem(
        caption = "Comparez les joueurs et prédisez les gagnants !",
        tags$img(src = "djoko.jpg", style = "width:100%; height:auto;")
      ),
      bs4CarouselItem(
        caption = "Analysez les performances avec des techniques avancées de machine learning.",
        tags$img(src = "perf.jpg", style = "width:100%; height:auto;")
      )
    ),
    

    

  )
}
