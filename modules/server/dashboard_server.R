library(plotly)


dashboard_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    #########################
    # PARTIE STATS GENERALES#
    #########################
    
    
    # 🏆 Nombre total de tournois
    output$total_tournaments <- renderbs4ValueBox({
      bs4ValueBox(
        value = length(unique(dataset$Tournament)),  
        subtitle = "Tournois disputés",
        icon = icon("trophy"),
        color = "success"
      )
    })
    
    # ⭐ Nombre de vainqueurs uniques
    output$unique_winners <- renderbs4ValueBox({
      bs4ValueBox(
        value = length(unique(dataset$Winner)),  
        subtitle = "Vainqueurs uniques",
        icon = icon("star"),
        color = "info"
      )
    })
    
    # 📊 Nombre total de matchs joués
    output$total_matches <- renderbs4ValueBox({
      bs4ValueBox(
        value = nrow(dataset),  # 📊 Compter toutes les lignes du dataset
        subtitle = "Matchs joués",
        icon = icon("chart-line"),
        color = "warning"
      )
    })
    
    # Graphique : Répartition des matchs par surface
    output$surface_distribution <- renderPlotly({
      surface_counts <- table(dataset$Surface)
      
      # Définition de 4 couleurs pour les surfaces
      colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")  # Bleu, Orange, Vert, Rouge
      
      plot_ly(
        labels = names(surface_counts),
        values = as.numeric(surface_counts),
        type = "pie",
        textinfo = "label+percent",
        insidetextfont = list(color = "white"),  # Meilleure lisibilité
        marker = list(colors = colors, line = list(color = "#FFFFFF", width = 2))  # Ajout de bordures blanches
      ) %>%
        layout(
          title = "Répartition des matchs par surface",
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.3, y = -0.2)  # Légende en bas pour plus de clarté
        )
    })
    
    
    # Graphique : Top 10 des joueurs avec le plus de victoires
    output$top_10_winners <- renderPlotly({
      top_winners <- dataset %>%
        group_by(Winner) %>%
        summarise(Wins = n()) %>%
        arrange(desc(Wins)) %>%
        head(10)
      
      plot_ly(
        data = top_winners,
        x = ~Wins,
        y = ~reorder(Winner, Wins),
        type = "bar",
        orientation = "h",
        marker = list(
          color = colorRampPalette(c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", "#fc8d59", "#d73027"))(10),  # Dégradé de couleurs
          line = list(color = "black", width = 1.2)  # Bordures noires pour plus de clarté
        )
      ) %>%
        layout(
          title = "Top 10 des joueurs avec le plus de victoires",
          xaxis = list(title = "Nombre de victoires", gridcolor = "gray80"),  # Lignes de fond plus légères
          yaxis = list(title = "Joueur"),
          margin = list(l = 150),  # Augmenter la marge à gauche pour éviter le chevauchement des noms
          hovermode = "y"  # Affichage interactif des valeurs au survol
        )
    })
    
    
    #############################
    # FIN PARTIE STATS GENERALES#
    #############################
    
    
    
    ###################################
    # PARTIE TENDANCES ET PERFORMANCES#
    ###################################
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # Mise à jour dynamique des joueurs dans le selectInput
    observe({
      updateSelectInput(session, "player_select", choices = unique(c(dataset$Player_1, dataset$Player_2)))
    })
    
    # Statistiques d'un joueur sélectionné
    output$player_stats <- renderText({
      req(input$player_select)
      player_matches <- dataset %>% 
        filter(Player_1 == input$player_select | Player_2 == input$player_select)
      
      paste("Nombre de matchs joués par", input$player_select, ":", nrow(player_matches))
    })
    
    # Placeholder pour les autres sections
    output$match_stats <- renderText("Données sur un match spécifique bientôt disponibles...")
    output$tournament_stats <- renderText("Analyse des tournois en cours de développement...")
    
  })
}
