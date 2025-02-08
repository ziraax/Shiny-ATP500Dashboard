dashboard_ui <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "dashboard",
    bs4Card(
      title = "Analyse des Tournois",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      "Ici, vous trouverez des visualisations interactives des performances des joueurs et des tendances des tournois."
    )
  )
  
}
