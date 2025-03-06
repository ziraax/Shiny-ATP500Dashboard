clustering_server <- function(id, player_stat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Nettoyage des données (sans standardisation)
    player_stat_clean <- reactive({
      player_stat %>%
        select(
          Player, Total_wins, Wins_percent, AVG_rank, VAR_rank,
          Clay_wins_percent, Hard_wins_percent, Grass_wins_percent, Carpet_wins_percent,
          `Grand Slam_nmatches`, `Grand Slam_nwins`,
          Masters_nmatches, Masters_nwins,
          ATP250_nmatches, ATP500_nmatches, `Masters 1000_nmatches`, `Masters Cup_nmatches`
        ) %>%
        replace(is.na(.), 0)  # Remplacer les NA par des zéros
    })
    
    # Standardisation des données pour le clustering
    player_stat_scaled <- reactive({
      player_stat_clean() %>%
        mutate(across(-Player, scale))  # Standardisation sans toucher à la colonne 'Player'
    })
    
    # Mettre à jour les choix pour les axes X et Y
    # On enleve certaines colonnes pour la visualisation car sinon ça pose problème
    observe({
      updateSelectInput(session, "x_axis", choices = names(player_stat_clean() 
                                                           %>% select(-Player, 
                                                                      -Masters_nmatches, 
                                                                      -Masters_nwins, 
                                                                      -ATP250_nmatches,
                                                                      -ATP500_nmatches,
                                                                      -`Masters 1000_nmatches`,
                                                                      -`Masters Cup_nmatches`,
                                                                      -`Grand Slam_nmatches`,
                                                                      -`Grand Slam_nwins`)))
      updateSelectInput(session, "y_axis", choices = names(player_stat_clean() 
                                                           %>% select(-Player, 
                                                                      -Masters_nmatches, 
                                                                      -Masters_nwins, 
                                                                      -ATP250_nmatches,
                                                                      -ATP500_nmatches,
                                                                      -`Masters 1000_nmatches`,
                                                                      -`Masters Cup_nmatches`,
                                                                      -`Grand Slam_nmatches`,
                                                                      -`Grand Slam_nwins`)))
    })
    
    # Réactive pour recalculer le clustering en fonction de k
    clusters <- reactive({
      req(input$k_value)  # S'assurer que k est défini
      
      # Calculer les clusters à partir des données standardisées
      kmeans_result <- kmeans(player_stat_scaled() %>% select(-Player), centers = input$k_value, nstart = 25)
      
      # Ajouter les clusters aux données non standardisées
      player_stat_clean() %>%
        mutate(cluster = as.factor(kmeans_result$cluster))
    })
    
    # Ré-échantillonnage des points par cluster
    sampled_data <- reactive({
      req(input$max_points_per_cluster)
      clusters() %>%
        group_by(cluster) %>%
        sample_n(min(n(), input$max_points_per_cluster)) %>%
        ungroup()
    })
    
    # Graphique des clusters en 2D avec plotly (données non standardisées)
    output$cluster_plot_2d <- renderPlotly({
      req(input$x_axis, input$y_axis)  # S'assurer que les axes sont sélectionnés
      
      # Utiliser les données non standardisées pour l'affichage
      data <- sampled_data()
      
      plot_ly(data, x = as.formula(paste0("~", input$x_axis)), 
              y = as.formula(paste0("~", input$y_axis)), 
              color = ~cluster, text = ~Player,
              type = "scatter", mode = "markers+text", marker = list(size = 10)) %>%
        layout(title = "Clusters en fonction des axes sélectionnés",
               xaxis = list(title = input$x_axis),
               yaxis = list(title = input$y_axis))
    })
    
    # PCA en 2D avec plotly (données standardisées et ré-échantillonnées)
    output$pca_plot_2d <- renderPlotly({
      # Calculer la PCA sur les données standardisées
      pca_result <- prcomp(player_stat_scaled() %>% select(-Player), center = TRUE, scale. = TRUE)
      
      # Extraire les scores PCA et ajouter les clusters
      pca_data <- as_tibble(pca_result$x) %>%
        select(PC1, PC2) %>%
        mutate(cluster = clusters()$cluster, Player = clusters()$Player)
      
      # Appliquer le ré-échantillonnage aux données PCA
      pca_data_sampled <- pca_data %>%
        group_by(cluster) %>%
        sample_n(min(n(), input$max_points_per_cluster)) %>%
        ungroup()
      
      # Afficher le graphique PCA 2D
      plot_ly(pca_data_sampled, x = ~PC1, y = ~PC2, color = ~cluster, text = ~Player,
              type = "scatter", mode = "markers+text", marker = list(size = 10)) %>%
        layout(title = "PCA - Réduction à 2 dimensions",
               xaxis = list(title = "PC1"),
               yaxis = list(title = "PC2"))
    })
    
    # PCA en 3D avec plotly (données standardisées et ré-échantillonnées)
    output$pca_plot_3d <- renderPlotly({
      # Calculer la PCA sur les données standardisées
      pca_result <- prcomp(player_stat_scaled() %>% select(-Player), center = TRUE, scale. = TRUE)
      
      # Extraire les scores PCA et ajouter les clusters
      pca_data_3d <- as_tibble(pca_result$x) %>%
        select(PC1, PC2, PC3) %>%
        mutate(cluster = clusters()$cluster, Player = clusters()$Player)
      
      # Appliquer le ré-échantillonnage aux données PCA
      pca_data_3d_sampled <- pca_data_3d %>%
        group_by(cluster) %>%
        sample_n(min(n(), input$max_points_per_cluster)) %>%
        ungroup()
      
      # Afficher le graphique PCA 3D
      plot_ly(pca_data_3d_sampled, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, text = ~Player,
              type = "scatter3d", mode = "markers+text") %>%
        layout(title = "PCA - Visualisation 3D des clusters",
               scene = list(xaxis = list(title = 'PC1'),
                            yaxis = list(title = 'PC2'),
                            zaxis = list(title = 'PC3')))
    })
    
    # Graphique de la méthode du coude
    output$elbow_plot <- renderPlot({
      wcss <- sapply(1:10, function(k) {
        kmeans(player_stat_scaled() %>% select(-Player), centers = k, nstart = 25)$tot.withinss
      })
      plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, 
           xlab = "Nombre de clusters (k)", ylab = "WCSS",
           main = "Méthode du coude pour déterminer k")
    })
    
    # Graphique du score de silhouette
    output$silhouette_plot <- renderPlot({
      library(cluster)
      silhouette_scores <- sapply(2:10, function(k) {
        kmeans_result <- kmeans(player_stat_scaled() %>% select(-Player), centers = k, nstart = 25)
        silhouette_score <- silhouette(kmeans_result$cluster, dist(player_stat_scaled() %>% select(-Player)))
        mean(silhouette_score[, 3])
      })
      plot(2:10, silhouette_scores, type = "b", pch = 19, frame = FALSE, 
           xlab = "Nombre de clusters (k)", ylab = "Score de silhouette moyen",
           main = "Score de silhouette pour différents k")
    })
  })
}