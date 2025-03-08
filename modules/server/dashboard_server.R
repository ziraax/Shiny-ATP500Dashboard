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
      colors <- c("#d62728", "#ff7f0e", "#2ca02c", "#1f77b4")
      
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
                marker = list(color = "darkblue")) %>%
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
              marker = list(color = "darkblue")) %>%
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
                marker = list(color = "darkblue")) %>% 
        layout(
          title = "Evolution du nombre de tournois par année",
          xaxis = list(title = "Année"),
          yaxis = list(title = "Nombre de tournois")
        )
        
    })

    output$upsets_per_year <- renderPlotly({
      dataset %>% 
        mutate(Year = as.numeric(substr(Date, 1, 4))) %>%
        filter(Year < 2025) %>% 
        filter(Rank_1 > Rank_2) %>% 
        filter(Winner == Player_2) %>% 
        group_by(Year) %>% 
        summarise(Num_Upsets = n(), .groups = "drop") %>% 
        plot_ly(x = ~Year, y = ~Num_Upsets, type="scatter", mode = "lines+markers",
                marker = list(color= "darkblue")) %>% 
        layout(
          title = "Evolution du nombre d'upsets par année",
          xaxis = list(title = "Année"),
          yaxis = list(title = "Nombre d'upsets")
        )
    })
    
    ########################################
    # FIN PARTIE TENDANCES ET PERFORMANCES #
    ########################################
    
    #########################
    # PARTIE ANALYSE JOUEUR #
    #########################
    observe({
      player_list <- sort(unique(c(dataset$Player_1, dataset$Player_2)))
      updateSelectizeInput(session, "player_select", choices = player_list, server = TRUE, options = list(maxOptions = 2000))
    })
    
    # Affichage avec HTML des informations sur le joueur
    output$infos_joueurs <- renderUI({
      req(input$player_select) 
      
      players <- read_csv("./data/players_stats.csv")
      
      player_info <- players %>%
        filter(Player == input$player_select) %>%
        select(Player, flag_url, Date_of_birth, Country, AVG_rank, Wins_percent, Total_wins, Hand)
      
      flag <- player_info$flag_url
      country <- countrycode(player_info$Country, origin = "iso3c", destination = "country.name")
      player_name <- player_info$Player
      age <- as.integer(interval(player_info$Date_of_birth, Sys.Date()) / years(1))
      avg_rank <- round(player_info$AVG_rank)
      win_percent <- round(player_info$Wins_percent, 1)
      total_wins <- player_info$Total_wins
      hand <- ifelse(!is.na(player_info$Hand), 
                     ifelse(player_info$Hand == "R", "D", "G"), 
                     "Main non spécifiée")
      
      HTML(
        paste0(
          "<div style='display: flex; align-items: center; gap: 15px; padding: 10px; background-color: #f9f9f9; border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);'>",
          "  <img src='", flag, "' width='400' height='270' style='border-radius: 5px; box-shadow: 1px 1px 5px rgba(0,0,0,0.2);'>",
          "  <table style='border-collapse: collapse; font-size: 18px; line-height: 1.6; color: #333;'>",
          "    <tr>",
          "      <td style='padding-right: 20px;'><b>Nom :</b></td><td style='padding-right: 30px;'>", player_name, "</td>",
          "      <td style='padding-right: 20px;'><b>Âge :</b></td><td>", age, " ans</td>",
          "    </tr>",
          "    <tr>",
          "      <td style='padding-right: 20px;'><b>Nationalité :</b></td><td style='padding-right: 30px;'>", country, "</td>",
          "      <td style='padding-right: 20px;'><b>Main dominante :</b></td><td>", hand, "</td>",
          "    </tr>",
          "    <tr>",
          "      <td style='padding-right: 20px;'><b>🌍 Rang moyen :</b></td><td>", avg_rank, "</td>",
          "    </tr>",
          "    <tr>",
          "      <td style='padding-right: 20px;'><b>🏆 Victoires totales :</b></td><td>", total_wins, "</td>",
          "    </tr>",
          "    <tr>",
          "      <td style='padding-right: 20px;'><b>📊 Pourcentage de victoires :</b></td><td>", win_percent, "%</td>",
          "    </tr>",
          "  </table>",
          "</div>"
        )
      )
      
    })
    
    
    
    # premier graphique : pourcentage de victoire par terrain
    output$graphique_terrains <- renderPlotly({
      req(input$player_select)
      
      player <- input$player_select
      players <- read_csv("./data/players_stats.csv")
      
      player_data <- players %>%
        filter(Player == player) %>%
        select(Player, Clay_wins_percent, Grass_wins_percent, Hard_wins_percent, Carpet_wins_percent) %>%
        pivot_longer(cols = -Player, names_to = "Surface", values_to = "Win_Percent")
      
      player_data$Surface <- factor(player_data$Surface, 
                                    levels = c("Clay_wins_percent", "Grass_wins_percent", "Hard_wins_percent", "Carpet_wins_percent"),
                                    labels = c("Clay", "Grass", "Hard", "Carpet"))
      
      player_data$Win_Percent <- tidyr::replace_na(player_data$Win_Percent, 0)
      
      # Création d'un barplot interactif avec plotly
      p <- plot_ly(data = player_data, 
                   x = ~Surface, 
                   y = ~Win_Percent, 
                   type = 'bar', 
                   color = ~Surface, 
                   colors = c("#ff7f0e", "#2ca02c", "#1f77b4", "#d62728"),
                   hovertext = ~paste(Surface, ": ", round(Win_Percent), "%"),
                   hoverinfo = 'text') %>%
        layout(title = paste("Pourcentage de victoire par surface -", player),
               xaxis = list(title = "Surface"),
               yaxis = list(title = "Pourcentage de victoire (%)"))
      
      p
    })
    
    output$evolution_rank <- renderPlotly({
      req(input$player_select)
      
      # Filtrer les données pour chaque joueur
      player_data <- dataset %>%
        filter(Player_1 == input$player_select | Player_2 == input$player_select) %>%
        mutate(Rank = ifelse(Player_1 == input$player_select, Rank_1, Rank_2)) %>%
        select(Date, Rank)
      
      # Tracer le graphique
      plot_ly() %>%
        add_trace(data = player_data, x = ~Date, y = ~Rank, type = "scatter", mode = "lines+markers", name = input$player_select) %>%
        layout(
          title = "Évolution du classement ATP",
          xaxis = list(title = "Date"),
          yaxis = list(title = " ")
        )
    })
    
    output$championnats_graphique <- renderPlotly({
      
      players <- read_csv("./data/players_stats.csv")
      
      championnats <- players %>%
        filter(Player == input$player_select) %>%
        select(Player, International_nmatches, `Grand Slam_nmatches`, `International Gold_nmatches`, 
               Masters_nmatches, ATP250_nmatches, ATP500_nmatches, `Masters 1000_nmatches`, 
               `Masters Cup_nmatches`) %>%
        pivot_longer(cols = -Player, names_to = "Championnat", values_to = "nmatches")
      
      championnats <- championnats %>%
        mutate(Percent = ifelse(!is.finite(nmatches) | is.na(nmatches), 0, nmatches / sum(nmatches) * 100))
      
      championnats$Championnat <- gsub("_nmatches", "", championnats$Championnat)
      
      champ_colors <- c("International" = "#FF6347", "Grand Slam" = "#4682B4", "International Gold" = "#32CD32", "Masters" = "#FFD700",          
                        "ATP250" = "#00CED1", "ATP500" = "#8A2BE2", "Masters 1000" = "#D2691E", "Masters Cup" = "#A52A2A")
      
      championnats$Championnat <- factor(championnats$Championnat, 
                                         levels = names(champ_colors))
      
      championnats$color <- champ_colors[championnats$Championnat]
      
      # Doughnut chart
      plot_ly(championnats, 
              labels = ~Championnat, 
              values = ~Percent, 
              type = 'pie',
              text = ~paste(nmatches),
              hoverinfo = 'label+text',
              textinfo = 'percent',
              hovertemplate = ~paste(Championnat, "<br>Nombre de matchs : ", nmatches),
              showlegend = TRUE,
              marker = list(colors = championnats$color)) %>% 
        layout(title = list(text = "Répartition des championnats joués",
                            xanchor = 'right'),
               legend = list(orientation = 'v', x = 1.5, xanchor = 'right', y = 0.5, yanchor = 'middle'))
    })
    
    output$championnats_graphique2 <- renderPlotly({
      
      players <- read_csv("./data/players_stats.csv")
      
      championnats <- players %>%
        filter(Player == input$player_select) %>%
        select(Player, International_nmatches, `Grand Slam_nmatches`, `International Gold_nmatches`, 
               Masters_nmatches, ATP250_nmatches, ATP500_nmatches, `Masters 1000_nmatches`, 
               `Masters Cup_nmatches`, International_nwins, `Grand Slam_nwins`, `International Gold_nwins`, 
               Masters_nwins, ATP250_nwins, ATP500_nwins, `Masters 1000_nwins`, 
               `Masters Cup_nwins`) %>%
        pivot_longer(
          cols = c(International_nmatches:`Masters Cup_nmatches`, 
                   International_nwins:`Masters Cup_nwins`),
          names_to = c("Championnat", ".value"),
          names_pattern = "(.*)_(nmatches|nwins)"
        )
      
      championnats <- championnats %>%
        mutate(Percent = ifelse(!is.finite(nmatches) | is.na(nmatches) & !is.finite(nwins) | is.na(nwins), 0, nwins/sum(nwins) * 100))
      
      
      champ_colors <- c("International" = "#FF6347", "Grand Slam" = "#4682B4", "International Gold" = "#32CD32", "Masters" = "#FFD700",          
                        "ATP250" = "#00CED1", "ATP500" = "#8A2BE2", "Masters 1000" = "#D2691E", "Masters Cup" = "#A52A2A")
      
      championnats$Championnat <- factor(championnats$Championnat, 
                                         levels = names(champ_colors))
      
      championnats$color <- champ_colors[championnats$Championnat]
      
      plot_ly(championnats, 
              labels = ~Championnat, 
              values = ~Percent,
              type = 'pie',
              text = ~paste(nwins),
              hoverinfo = 'label+text',
              textinfo = 'percent',
              hovertemplate = ~paste(Championnat, "<br>Nombre de victoires : ", nwins),
              showlegend = FALSE,
              marker = list(colors = championnats$color)) %>% 
        layout(title = "Répartition des victoires par championnat",
               showlegend = FALSE)
    })
    
    ########################################
    # FIN PARTIE TENDANCES ET PERFORMANCES #
    ########################################
    
    #########################
    # PARTIE ANALYSE VERSUS #
    #########################
    
    observe({
      player_list <- sort(unique(c(dataset$Player_1, dataset$Player_2)))
      updateSelectizeInput(session, "player_select", choices = player_list, server = TRUE, options = list(maxOptions = 2000))
    })
    
    # Mise à jour dynamique des joueurs dans les selectizeInput pour "Analyse versus"
    observe({
      player_list <- sort(unique(c(dataset$Player_1, dataset$Player_2)))
      updateSelectizeInput(session, "player_select_1", choices = player_list, server = TRUE, options = list(maxOptions = 2000))
      updateSelectizeInput(session, "player_select_2", choices = player_list, server = TRUE, options = list(maxOptions = 2000))
    })
    
    
    
    
    # Nombre total de matchs
    output$total_matches_text <- renderText({
      req(input$player_select_1, input$player_select_2)
      
      player1 <- input$player_select_1
      player2 <- input$player_select_2
      
      matches <- dataset %>%
        filter((Player_1 == player1 & Player_2 == player2) | (Player_1 == player2 & Player_2 == player1))
      
      paste("Nombre total de matchs disputés entre", player1, "et", player2, ":", nrow(matches))
    })
    
    
    # Victoires du joueur 1
    output$player1_wins <- renderbs4ValueBox({
      req(input$player_select_1, input$player_select_2)
      
      player1 <- input$player_select_1
      matches <- dataset %>%
        filter((Player_1 == player1 & Player_2 == input$player_select_2) | (Player_1 == input$player_select_2 & Player_2 == player1))
      
      player1_wins <- nrow(matches %>% filter(Winner == player1))
      
      bs4ValueBox(
        value = player1_wins,
        subtitle = paste("Victoires de", player1),
        icon = icon("trophy"),
        color = "success"
      )
    })
    
    # Victoires du joueur 2
    output$player2_wins <- renderbs4ValueBox({
      req(input$player_select_1, input$player_select_2)
      
      player2 <- input$player_select_2
      matches <- dataset %>%
        filter((Player_1 == input$player_select_1 & Player_2 == player2) | (Player_1 == player2 & Player_2 == input$player_select_1))
      
      player2_wins <- nrow(matches %>% filter(Winner == player2))
      
      bs4ValueBox(
        value = player2_wins,
        subtitle = paste("Victoires de", player2),
        icon = icon("trophy"),
        color = "danger"
      )
    })
    
    # Classement ATP du joueur 1
    output$ranking1 <- renderbs4ValueBox({
      req(input$player_select_1)
      
      player1 <- input$player_select_1
      last_match_player1 <- dataset %>%
        filter(Player_1 == player1 | Player_2 == player1) %>%
        arrange(desc(Date)) %>%
        slice(1)
      
      ranking1 <- ifelse(last_match_player1$Player_1 == player1, last_match_player1$Rank_1, last_match_player1$Rank_2)
      
      bs4ValueBox(
        value = ranking1,
        subtitle = paste("Classement ATP de", player1),
        icon = icon("chart-line"),
        color = "primary"
      )
    })
    
    # Classement ATP du joueur 2
    output$ranking2 <- renderbs4ValueBox({
      req(input$player_select_2)
      
      player2 <- input$player_select_2
      last_match_player2 <- dataset %>%
        filter(Player_1 == player2 | Player_2 == player2) %>%
        arrange(desc(Date)) %>%
        slice(1)
      
      ranking2 <- ifelse(last_match_player2$Player_1 == player2, last_match_player2$Rank_1, last_match_player2$Rank_2)
      
      bs4ValueBox(
        value = ranking2,
        subtitle = paste("Classement ATP de", player2),
        icon = icon("chart-line"),
        color = "primary"
      )
    })
    
    
    # Graphique : Évolution du classement des deux joueurs
    output$ranking_evolution <- renderPlotly({
      req(input$player_select_1, input$player_select_2)
      
      player1 <- input$player_select_1
      player2 <- input$player_select_2
      
      # Filtrer les données pour chaque joueur
      player1_data <- dataset %>%
        filter(Player_1 == player1 | Player_2 == player1) %>%
        mutate(Rank = ifelse(Player_1 == player1, Rank_1, Rank_2)) %>%
        select(Date, Rank)
      
      player2_data <- dataset %>%
        filter(Player_1 == player2 | Player_2 == player2) %>%
        mutate(Rank = ifelse(Player_1 == player2, Rank_1, Rank_2)) %>%
        select(Date, Rank)
      
      # Tracer le graphique
      plot_ly() %>%
        add_trace(data = player1_data, x = ~Date, y = ~Rank, type = "scatter", mode = "lines", name = player1,  line = list(color='#28a745')) %>%
        add_trace(data = player2_data, x = ~Date, y = ~Rank, type = "scatter", mode = "lines", name = player2,  line = list(color='#dc3545')) %>%
        layout(
          title = "Évolution du classement ATP",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Classement ATP")
        )
    })
    
    # Graphique : Évolution du nombre de points des deux joueurs
    output$points_evolution <- renderPlotly({
      req(input$player_select_1, input$player_select_2)
      
      player1 <- input$player_select_1
      player2 <- input$player_select_2
      
      # Filtrer les données pour chaque joueur
      player1_data <- dataset %>%
        filter(Player_1 == player1 | Player_2 == player1) %>%
        mutate(Points = ifelse(Player_1 == player1, Pts_1, Pts_2)) %>%
        select(Date, Points)
      
      player2_data <- dataset %>%
        filter(Player_1 == player2 | Player_2 == player2) %>%
        mutate(Points = ifelse(Player_1 == player2, Pts_1, Pts_2)) %>%
        select(Date, Points)
      
      # Tracer le graphique
      plot_ly() %>%
        add_trace(data = player1_data, x = ~Date, y = ~Points, type = "scatter", mode = "lines", name = player1, line = list(color='#28a745')) %>%
        add_trace(data = player2_data, x = ~Date, y = ~Points, type = "scatter", mode = "lines", name = player2, line = list(color='#dc3545')) %>%
        layout(
          title = "Évolution du nombre de points",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Points")
        )
    })
    
    
    # Graphique : Répartition des surfaces des matchs entre les deux joueurs
    output$surface_distribution_players <- renderPlotly({
      req(input$player_select_1, input$player_select_2)
      
      player1 <- input$player_select_1
      player2 <- input$player_select_2
      
      # Filtrer les matchs entre les deux joueurs
      matches <- dataset %>%
        filter((Player_1 == player1 & Player_2 == player2) | (Player_1 == player2 & Player_2 == player1))
      
      if (nrow(matches) == 0) {
        return(plot_ly(x = 0, y = 0, type = "scatter", mode = "markers") %>%
                 layout(title = "Aucun match trouvé entre ces deux joueurs."))  # On spécifie x, y, type et mode juste pour pas avoir de warnings dans la console 
      }
      
      # Calculer la répartition des surfaces
      surface_counts <- table(matches$Surface)
      
      # Définir les couleurs pour chaque type de surface
      colors <- c("Clay" = "#ff7f0e", "Grass" = "#2ca02c", "Hard" = "#1f77b4", "Carpet" = "#d62728" )
      
      # Tracer le graphique en disque
      plot_ly(
        labels = names(surface_counts),
        values = as.numeric(surface_counts),
        type = "pie",
        hole = 0.4,  # Pour créer un effet "donut"
        textinfo = "label+percent",
        marker = list(colors = colors[names(surface_counts)], line = list(color = "#FFFFFF", width = 2))  # Ajout de bordures blanches
      ) %>%
        layout(
          title = "Répartition des surfaces des matchs entre les deux joueurs",
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.3, y = -0.2)  # Légende en bas pour plus de clarté
        )
    })
    
    # Graphique : Nombre de matchs gagnés par surface
    output$wins_by_surface <- renderPlotly({
      req(input$player_select_1, input$player_select_2)
      
      player1 <- input$player_select_1
      player2 <- input$player_select_2
      
      # Filtrer les matchs entre les deux joueurs
      matches <- dataset %>%
        filter((Player_1 == player1 & Player_2 == player2) | (Player_1 == player2 & Player_2 == player1))
      
      if (nrow(matches) == 0) {
        return(plot_ly(x = 0, y = 0, type = "scatter", mode = "markers") %>%      # On spécifie x, y, type et mode juste pour pas avoir de warnings dans la console 
                 layout(title = "Aucun match trouvé entre ces deux joueurs."))
      }
      
      # Calculer le nombre de matchs gagnés par chaque joueur sur chaque surface
      wins_data <- matches %>%
        group_by(Surface, Winner) %>%
        summarise(Wins = n()) %>%
        filter(Winner %in% c(player1, player2))
      
      # Tracer le graphique à barres empilées
      plot_ly(
        data = wins_data,
        x = ~Surface,
        y = ~Wins,
        color = ~Winner,
        colors = c("#dc3545", "#28a745"),
        type = "bar",
        text = ~paste(Wins),
        textposition = "auto"
      ) %>%
        layout(
          title = "Nombre de matchs gagnés par surface",
          xaxis = list(title = "Surface"),
          yaxis = list(title = "Nombre de matchs gagnés"),
          barmode = "stack"  # Empiler les barres pour chaque joueur
        )
    })
    
    
    #############################
    # FIN PARTIE ANALYSE VERSUS #
    #############################
    
    
    
    
    ################################
    # PARTIE CARTOGRAPHIE ONGLET 4 #
    ################################
    
    tournament_coords <- read.csv2("./data/geocode2.csv", header = TRUE, sep=",")
    data_with_coords <- merge(dataset, tournament_coords, by="Tournament", all.x = TRUE)
    head(data_with_coords)
    
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
            Surface == "Hard" ~ "#1f77b4",    # Hard - Orange
            Surface == "Grass" ~ "#2ca02c",   # Grass - Green
            Surface == "Clay" ~ "#ff7f0e",    # Clay - Purple
            Surface == "Carpet" ~ "#d62728",  # Carpet - Yellow
            TRUE ~ "#BDC3C7"  # Autre surface - Gris
          ),
          
          fillColor = case_when(
            #Series == "Grand Slam" ~ "#FFC300",  # Si on veut une couleur spécifique pour les tounrois du grand schlem
            TRUE ~ color  # Autre
          ),
          
          radius = case_when(
            Series == "Grand Slam" ~ 8,  # Grand Slam - Taille plus grande
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
          colors = c("#1f77b4" , "#2ca02c", "#ff7f0e", "#d62728"),  # Couleurs de la surface
          labels = c("🟦 Hard", "🌱 Grass", "🟧 Clay", "🟥 Carpet"),
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
      
      data_map
      
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
            ifelse(Surface == "Hard", "🟦 - Hard", 
                   ifelse(Surface == "Grass", "🌱 - Grass",
                          ifelse(Surface == "Clay", "🟧 - Clay", 
                                 ifelse(Surface == "Carpet", "🟥 - Carpet", "❓ - Unknown")))), "<br>",
            "🏆 Type de tournoi : ", 
            ifelse(Series == "Grand Slam", "🏅 - Grand Slam", 
                   ifelse(Series == "International", "🌍 - International", 
                          paste0("🎾 - ", Series))), "<br>",
            "⛳ Côté court : ", 
            ifelse(Court == "Outdoor", "🌞 - Outdoor", "🏟️ - Indoor"), "<br>",
            "🗓️ Première édition : ", First_Edition, "<br>",
            "🗓️ Dernière édition : ", Last_Edition
          ),
          radius = ~radius,
          color = ~color,
          opacity =1,
          fillColor = ~fillColor,  
          fillOpacity = 1
        )
    })
    
    
    
    
    
    ############################
    # FIN PARTIE CARTOGRAPHIES #
    ############################
    
    
    
    
    

    output$tournament_stats <- renderText("Analyse des tournois en cours de développement...")
    
  })
}
