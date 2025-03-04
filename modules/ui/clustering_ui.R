clustering_ui <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "clustering",
    bs4Card(
      title = "Clustering des Joueurs",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 3,
          numericInput(ns("k_value"), "Nombre de clusters (k)", value = 3, min = 2, max = 10)
        ),
        column(
          width = 9,
          tabPanel(
            id = ns("tabs"),
            tabPanel(
              title = "Clusters 2D",
              plotOutput(ns("cluster_plot_2d"))
            ),
            tabPanel(
              title = "PCA 2D",
              plotOutput(ns("pca_plot_2d"))
            ),
            tabPanel(
              title = "PCA 3D",
              plotlyOutput(ns("pca_plot_3d"))
            )
          )
        )
      )
    )
  )
}