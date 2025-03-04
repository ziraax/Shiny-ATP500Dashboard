clustering_server <- function(id, player_stat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Nettoyage et standardisation des données
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
    
    player_stat_scaled <- reactive({
      player_stat_clean() %>%
        mutate(across(-Player, scale))  # Standardisation sans toucher à la colonne 'Player'
    })
    
    # Réactive pour recalculer le clustering en fonction de k
    clusters <- reactive({
      req(input$k_value)  # S'assurer que k est défini
      kmeans_result <- kmeans(player_stat_scaled() %>% select(-Player), centers = input$k_value, nstart = 25)
      player_stat_scaled() %>%
        mutate(cluster = as.factor(kmeans_result$cluster))
    })
    
    # Graphique des clusters en 2D
    output$cluster_plot_2d <- renderPlot({
      ggplot(clusters(), aes(x = Total_wins, y = AVG_rank, color = cluster, label = Player)) +
        geom_point(size = 3) +
        geom_text(aes(label = Player), hjust = 0.5, vjust = -0.5, size = 3, show.legend = FALSE) +
        labs(title = "Clusters en fonction des Victoires Totales et du Rang Moyen",
             x = "Total des victoires", y = "Rang moyen") +
        theme_minimal()
    })
    
    # PCA en 2D
    output$pca_plot_2d <- renderPlot({
      pca_result <- prcomp(clusters() %>% select(-cluster, -Player), center = TRUE, scale. = TRUE)
      pca_data <- as_tibble(pca_result$x) %>%
        select(PC1, PC2) %>%
        mutate(cluster = clusters()$cluster, Player = clusters()$Player)
      
      ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster, label = Player)) +
        geom_point(size = 3) +
        geom_text(aes(label = Player), hjust = 0.5, vjust = -0.5, size = 3, show.legend = FALSE) +
        labs(title = "PCA - Réduction à 2 dimensions",
             x = "PC1", y = "PC2") +
        theme_minimal()
    })
    
    # PCA en 3D
    output$pca_plot_3d <- renderPlotly({
      pca_result <- prcomp(clusters() %>% select(-cluster, -Player), center = TRUE, scale. = TRUE)
      pca_data_3d <- as_tibble(pca_result$x) %>%
        select(PC1, PC2, PC3) %>%
        mutate(cluster = clusters()$cluster, Player = clusters()$Player)
      
      # Échantillonnage pour éviter la surcharge
      pca_data_3d_sampled <- pca_data_3d %>%
        group_by(cluster) %>%
        sample_n(min(n(), 20)) %>%
        ungroup()
      
      plot_ly(pca_data_3d_sampled, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, text = ~Player, type = "scatter3d", mode = "markers+text") %>%
        layout(title = "PCA - Visualisation 3D des clusters",
               scene = list(xaxis = list(title = 'PC1'),
                            yaxis = list(title = 'PC2'),
                            zaxis = list(title = 'PC3')))
    })
  })
}