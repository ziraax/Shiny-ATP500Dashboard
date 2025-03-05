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
    ),
    bs4Card(
      title = "Explications sur le clustering",
      status = "gray",
      solidHeader = TRUE,
      width = 12,
      
      h4("K-means non supervisé"),
      
      p("On se place ici dans le cadre d'un clustering k-means non supervisée, ce qui signifie qu'il n'y a pas de labels pour évaluer directement la qualité des clusters. Par conséquent, on ne peut pas utiliser de ré-échantillonnage (comme la validation croisée) pour déterminer le meilleur \\(k\\)."),
      p("À la place, on utilise des méthodes intrinsèques comme la méthode du coude ou le score de silhouette pour guider le choix de \\(k\\)."),
      
      withMathJax(
        p("Le clustering k-means est une méthode de partitionnement de données en \\(k\\) clusters. L'objectif est de minimiser la somme des carrés intra-cluster (WCSS) :"),
        p("$$\\text{WCSS} = \\sum_{i=1}^{k} \\sum_{x \\in C_i} \\|x - \\mu_i\\|^2$$"),
        p("où \\(C_i\\) est le cluster \\(i\\), \\(\\mu_i\\) est le centroïde du cluster \\(i\\), et \\(x\\) est un point de données.")
      ),
      
      
      h4("Méthode du coude (Elbow Method)"),
      plotOutput(ns("elbow_plot")),  # Graphique de la méthode du coude
      
      h4("Score de Silhouette"),
      plotOutput(ns("silhouette_plot")),  # Graphique du score de silhouette
      
      
      
      h4("Explications du choix de k"),
      withMathJax(
        p("Le choix de \\(k = 3\\) est justifié par la structure métier du tennis. On s'attend à trois groupes principaux :"),
        tags$ul(
          tags$li("Les meilleurs joueurs (élite)"),
          tags$li("Les joueurs en progression (aspirants)"),
          tags$li("Les joueurs amateurs ou moins performants")
        ),
        p("La méthode du coude permet de déterminer le nombre optimal de clusters en cherchant le point où l'ajout d'un cluster supplémentaire n'améliore plus significativement la réduction de la WCSS."),
        p("Le score de silhouette mesure la qualité du clustering en évaluant à quel point un point est bien regroupé avec son cluster par rapport aux autres clusters. Pour un point \\(i\\), le score de silhouette \\(s(i)\\) est calculé comme suit :"),
        p("$$s(i) = \\frac{b(i) - a(i)}{\\max(a(i), b(i))}$$"),
        p("où :"),
        tags$ul(
          tags$li("\\(a(i)\\) est la distance moyenne entre le point \\(i\\) et tous les autres points du même cluster."),
          tags$li("\\(b(i)\\) est la distance moyenne entre le point \\(i\\) et tous les points du cluster le plus proche.")
        ),
        p("Le score de silhouette moyen pour tous les points est utilisé pour évaluer la qualité globale du clustering :"),
        p("$$\\text{Score de silhouette moyen} = \\frac{1}{N} \\sum_{i=1}^{N} s(i)$$"),
        p("Un score proche de 1 indique un clustering de bonne qualité.")
      )
    )
  )
}