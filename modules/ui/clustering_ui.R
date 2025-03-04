clustering_ui <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "clustering",
    bs4Card(
      title = "Clustering des Joueurs",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      # Sélecteur pour le nombre de clusters (k)
      numericInput(ns("k_value"), "Nombre de clusters (k)", value = 3, min = 2, max = 10),
      
      # Sélecteurs pour les axes X et Y
      fluidRow(
        column(4, selectInput(ns("x_axis"), "Axe X", choices = NULL)),
        column(4, selectInput(ns("y_axis"), "Axe Y", choices = NULL)),
        column(4, numericInput(ns("max_points_per_cluster"), "Points max par cluster", value = 20, min = 1))
      ),
      
      # Affichage des graphiques 2D côte à côte
      fluidRow(
        column(6, plotlyOutput(ns("cluster_plot_2d"))),  # Graphique des clusters 2D
        column(6, plotlyOutput(ns("pca_plot_2d")))       # Graphique PCA 2D
      ),
      
      # Affichage du graphique 3D en dessous
      plotlyOutput(ns("pca_plot_3d"))  # Graphique PCA 3D
    )
  )
}