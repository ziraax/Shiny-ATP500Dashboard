clustering_ui <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "clustering",
    bs4Card(
      title = "Clustering des Joueurs",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      "Visualisez les groupes de joueurs basés sur leurs performances et caractéristiques."
    )
  )
}
