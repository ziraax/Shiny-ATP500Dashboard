library(plotly)
library(leaflet)



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
    
    output$nb_matchs_annees <- renderPlotly({
      dataset %>%
        mutate(Year = as.numeric(substr(Date, 1, 4))) %>%  # Extraction de l'année
        filter(Year < 2025) %>% 
        group_by(Year) %>%
        summarise(Num_Matches = n()) %>%
        plot_ly(x = ~Year, y = ~Num_Matches, type = "scatter", mode = "lines+markers",
                line = list(color = "blue")) %>%
        layout(
          title = "Évolution du nombre de matchs par année",
          xaxis = list(title = "Année"),
          yaxis = list(title = "Nombre de matchs")
        )
    })
    
    output$top_winners_by_year <- renderPlotly({
      top_winners_by_year <- dataset %>%
        mutate(Year = as.numeric(substr(Date, 1, 4))) %>%
        filter(Year < 2025) %>% 
        group_by(Year, Winner) %>%
        summarise(Wins = n(), .groups = "drop") %>%
        arrange(Year, desc(Wins)) %>%
        group_by(Year) %>%
        slice_max(Wins, n = 1)# Sélectionne le joueur avec le plus de victoires par année
      
      plot_ly(top_winners_by_year, x = ~Year, y = ~Wins, type = "scatter", mode = "lines+markers",
              text = ~Winner, hoverinfo = "text+y",
              marker = list(color = "blue")) %>%
        layout(
          title = "Joueur avec le plus de victoires par année",
          xaxis = list(title = "Année"),
          yaxis = list(title = "Nombre de victoires")
        )
    })
    
    
    output$tournaments_per_year <- renderPlotly({
      dataset %>% 
        mutate(Year = as.numeric(substr(Date, 1, 4))) %>%
        filter(Year < 2025) %>% 
        group_by(Year) %>% 
        summarise(Num_Tournaments = n_distinct(Tournament), .groups = "drop") %>% 
        plot_ly(x = ~Year, y = ~Num_Tournaments, type="scatter", mode = "lines+markers",
                marker = list(color = "blue")) %>% 
        layout(
          title = "Evolution du nombre de tournois par année",
          xaxis = list(title = "Année"),
          yaxis = list(title = "Nombre de tournois")
        )
        
    })

    output$upsets_per_year <- renderPlotly({
      dataset %>% 
        mutate(Year = as.numeric(substr(Date, 1, 4))) %>%
        filter(Rank_1 > Rank_2) %>% 
        filter(Winner == Player_2) %>% 
        group_by(Year) %>% 
        summarise(Num_Upsets = n(), .groups = "drop") %>% 
        plot_ly(x = ~Year, y = ~Num_Upsets, type="scatter", mode = "lines+markers",
                marker = list(color= "blue")) %>% 
        layout(
          title = "Evolution du nombre d'upsets par année",
          xaxis = list(title = "Année"),
          yaxis = list(title = "Nombre d'upsets")
        )
    })
    
    ########################################
    # FIN PARTIE TENDANCES ET PERFORMANCES #
    ########################################
    
    
    
    
    
    
    
    ################################
    # PARTIE CARTOGRAPHIE ONGLET 4 #
    ################################
    
    tournament_coords <- read.csv2("./data/geocode2.csv", header = TRUE, sep=",")
    data_with_coords <- merge(data, tournament_coords, by="Tournament", all.x = TRUE)

    
    data_with_coords$Latitude <- as.numeric(data_with_coords$Latitude)
    data_with_coords$Longitude <- as.numeric(data_with_coords$Longitude)
    
    
    filtered_tournaments <- reactive({
      selected_date <- input$date_slider
      
      # Filtrer les données via la date et les checkbox
      data_filtered <- data_with_coords %>% 
        filter(Date <= selected_date) %>%
        group_by(Tournament) %>% 
        slice_max(Date, n=1) %>% 
        mutate(
          color = case_when(
            Surface == "Hard" ~ "#FF5733",    # Hard - Orange
            Surface == "Grass" ~ "#28B463",   # Grass - Green
            Surface == "Clay" ~ "#8E44AD",    # Clay - Purple
            Surface == "Carpet" ~ "#F39C12",  # Carpet - Yellow
            TRUE ~ "#BDC3C7"  # Autre surface - Gris
          ),
          
          fillColor = case_when(
            #Series == "Grand Slam" ~ "#FFC300",  # Si on veut une couleur spécifique pour les tounrois du grand schlem
            TRUE ~ color  # Autre
          ),
          
          radius = case_when(
            Series == "Grand Slam" ~ 20,  # Grand Slam - Taille plus grande
            TRUE ~ 8   # Autres tournois en taille standard
          )
        )
      
      # gestion des checkbox (filtrage)
      if (!is.null(input$tournament_types) && length(input$tournament_types) > 0) {
        data_filtered <- data_filtered %>% filter(Series %in% input$tournament_types)
      }
      
      if (!is.null(input$surface_types) && length(input$surface_types) > 0) {
        data_filtered <- data_filtered %>% filter(Surface %in% input$surface_types)
      }
      
      return(data_filtered)
    })
    
    
    
    output$tournament_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>% 
        setView(lng = 0, lat = 30, zoom = 2) %>%  
        addLegend(
          position = "bottomright", 
          colors = c("#FF5733", "#28B463", "#8E44AD", "#F39C12"),  # Couleurs de la surface
          labels = c("🟫 Hard", "🌱 Grass", "🟩 Clay", "💠 Carpet"),
          title = "Surface des Tournois"
        )
    })
    
    observe({
      data_map <- filtered_tournaments()
      
      data_map <- data_map %>% 
        filter(!is.na(Latitude) & !is.na(Longitude))
      
      data_map <- data_map %>%
        filter(
          Last_Edition == "Ongoing" | as.numeric(Last_Edition) >= as.numeric(format(input$date_slider, "%Y"))
        )
      
      leafletProxy("tournament_map", data = data_map) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~paste0(
            "<b>", Tournament, "</b><br>",
            "📅 Date : ", Date, "<br>",
            "🏆 Dernier vainqueur : ", Winner, "<br>",
            "🏙️ Ville : ", City, "<br>",
            "📍 Lieu : ", Venue, "<br>",
            "🏸 Surface : ", 
            ifelse(Surface == "Hard", "🟫 - Hard", 
                   ifelse(Surface == "Grass", "🌱 - Grass",
                          ifelse(Surface == "Clay", "🟩 - Clay", 
                                 ifelse(Surface == "Carpet", "💠 - Carpet", "❓ - Unknown")))), "<br>",
            "🏆 Type de tournoi : ", 
            ifelse(Series == "Grand Slam", "🏅 - Grand Slam", 
                   ifelse(Series == "International", "🌍 - International", 
                          paste0("🎾 - ", Series))), "<br>",
            "⛳ Côté court : ", 
            ifelse(Court == "Outdoor", "🌞 - Outdoor", "🏠 - Indoor"), "<br>",
            "🔢 Meilleur de : ", Best.of, " sets<br>",
            "🗓️ Première édition : ", First_Edition, "<br>",
            "🗓️ Dernière édition : ", Last_Edition
          ),
          radius = ~radius,
          color = ~color,  
          fillColor = ~fillColor,  
          fillOpacity = 0.7
        )
    })
    
    
    
    
    
    ############################
    # FIN PARTIE CARTOGRAPHIES #
    ############################
    
    
    
    
    
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
