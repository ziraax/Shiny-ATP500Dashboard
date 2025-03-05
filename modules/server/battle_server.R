battle_server <- function(id, dataset, player_stat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ################
    # MODEL TRAINING
    ################
    
    # Etape 1: Merge match data with player data
    merged_data <- dataset %>%
      left_join(player_stat, by = c("Player_1" = "Player")) %>%
      left_join(player_stat, by = c("Player_2" = "Player"), suffix = c("_1", "_2"))
    
    # Etape 2: Feature Engineering
    merged_data <- merged_data %>%
      mutate(
        rank_diff = Rank_1 - Rank_2,
        wins_percent_diff = Wins_percent_1 - Wins_percent_2,
        hard_wins_percent_diff = Hard_wins_percent_1 - Hard_wins_percent_2,
        clay_wins_percent_diff = Clay_wins_percent_1 - Clay_wins_percent_2,
        grass_wins_percent_diff = Grass_wins_percent_1 - Grass_wins_percent_2,
        total_wins_diff = Total_wins_1 - Total_wins_2,
        surface = as.factor(Surface),
        series = as.factor(Series),
        best_of = as.factor(`Best of`),
        round = as.factor(Round),
        age_diff = as.numeric(difftime(ymd(Date_of_birth_2), ymd(Date_of_birth_1), units = "days")) / 365,
        handedness = ifelse(Hand_1 == Hand_2, "Same", "Different")
      )
    
    # Etape 3: Missing values
    merged_data <- merged_data %>%
      drop_na()
    
    # Etape 4: Definir la target variable
    merged_data$target <- ifelse(merged_data$Winner == merged_data$Player_1, "Class1", "Class0")
    merged_data$target <- as.factor(merged_data$target)
    
    # Etape 5: Select relevant features for modeling
    model_data <- merged_data %>%
      select(
        target, rank_diff, wins_percent_diff, hard_wins_percent_diff,
        clay_wins_percent_diff, grass_wins_percent_diff, total_wins_diff,
        surface, series, best_of, round, age_diff, handedness
      )
    
    # Etape 6: Split the data into training and testing sets
    set.seed(123)
    train_index <- createDataPartition(model_data$target, p = 0.8, list = FALSE)
    train_data <- model_data[train_index, ]
    test_data <- model_data[-train_index, ]
    
    # Etape 7: Repeated cross-validation
    ctrl <- trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 5,
      summaryFunction = defaultSummary,
      classProbs = TRUE
    )
    
    # Etape 8: Logistic regression model
    logistic_model <- train(
      target ~ .,
      data = train_data,
      method = "glm",
      family = "binomial",
      trControl = ctrl,
      metric = "Accuracy"
    )
    
    # Etape 9: Test set
    test_predictions <- predict(logistic_model, test_data)
    conf_matrix <- confusionMatrix(test_predictions, test_data$target)

    ################
    # END MODEL TRAINING
    ################
    
    
    # Etape 12: Display confusion matrix
    output$conf_matrix <- renderPrint({
      # Extract the confusion matrix table
      conf_matrix_table <- conf_matrix$table
      
      # Extract other metrics
      accuracy <- conf_matrix$overall["Accuracy"]
      kappa <- conf_matrix$overall["Kappa"]
      sensitivity <- conf_matrix$byClass["Sensitivity"]
      specificity <- conf_matrix$byClass["Specificity"]
      
      # Print the confusion matrix and metrics
      cat("Accuracy:", accuracy, "\n")
      cat("Kappa:", kappa, "\n")
      cat("Sensitivity:", sensitivity, "\n")
      cat("Specificity:", specificity, "\n")
    })
    
    
    # Etape 10: Cross-validation results
    output$cv_plot <- renderPlot({
      cv_results <- logistic_model$resample
      ggplot(cv_results, aes(x = 1:nrow(cv_results), y = Accuracy)) +
        geom_point(size = 3, color = "blue") +
        geom_line(color = "red") +
        labs(
          title = "Cross-Validation Accuracy",
          x = "Fold",
          y = "Accuracy"
        ) +
        theme_minimal()
    })
    
    # Etape 11: Feature importance
    output$importance_plot <- renderPlot({
      importance <- varImp(logistic_model)
      plot(importance, main = "Feature Importance")
    })
    
    # Function to extract player data
    extract_player_data <- function(player_name) {
      player_stat %>% filter(Player == player_name) %>%
        select(
          Rank = AVG_rank,
          Wins_percent,
          Hard_wins_percent,
          Clay_wins_percent,
          Grass_wins_percent,
          Total_wins,
          Date_of_birth,
          Hand
        )
    }
    
    # Function to create a new match row
    create_new_match <- function(player1_data, player2_data) {
      data.frame(
        rank_diff = player1_data$Rank - player2_data$Rank,
        wins_percent_diff = player1_data$Wins_percent - player2_data$Wins_percent,
        hard_wins_percent_diff = player1_data$Hard_wins_percent - player2_data$Hard_wins_percent,
        clay_wins_percent_diff = player1_data$Clay_wins_percent - player2_data$Clay_wins_percent,
        grass_wins_percent_diff = player1_data$Grass_wins_percent - player2_data$Grass_wins_percent,
        total_wins_diff = player1_data$Total_wins - player2_data$Total_wins,
        
        
        series = factor(input$series, levels = levels(merged_data$series)),  # Utiliser la série sélectionnée
        surface = factor(input$surface, levels = levels(merged_data$surface)),  # Utiliser la surface sélectionnée
        best_of = factor(input$best_of, levels = levels(merged_data$best_of)),  # Utiliser le nombre de sets sélectionné
        round = factor(input$round, levels = levels(merged_data$round)),  # Utiliser le tour sélectionné
        
        
        
        age_diff = as.numeric(difftime(ymd(player2_data$Date_of_birth), ymd(player1_data$Date_of_birth), units = "days")) / 365,
        handedness = factor("Same")
      )
    }
    
    # Function to predict the winner probability
    predict_winner_probability <- function(player1_name, player2_name) {
      player1_data <- extract_player_data(player1_name)
      player2_data <- extract_player_data(player2_name)
      
      if (nrow(player1_data) == 0) {
        stop(paste("Le joueur", player1_name, "n'a pas été trouvé dans le dataset."))
      }
      if (nrow(player2_data) == 0) {
        stop(paste("Le joueur", player2_name, "n'a pas été trouvé dans le dataset."))
      }
      
      new_match <- create_new_match(player1_data, player2_data)
      
      if (any(is.na(new_match))) {
        stop("Des valeurs manquantes (NA) ont été détectées dans les nouvelles données. Veuillez vérifier les données des joueurs.")
      }
      
      prob_player1_win <- predict(logistic_model, new_match, type = "prob")$Class1
      prob_player2_win <- 1 - prob_player1_win
      
      result <- data.frame(
        Player = c(player1_name, player2_name),
        Probability_of_Winning = c(prob_player1_win, prob_player2_win)
      )
      
      return(result)
    }
    
    # Function to create a player info card
    create_player_info <- function(player_data, player_name) {
      tags$div(
        style = "padding: 10px; border: 1px solid #ddd; border-radius: 5px;",
        h4(player_name, style = "color: #007BFF;"),
        p(strong("Average Rank:"), round(player_data$Rank, 2)),
        p(strong("Win Percentage:"), paste0(round(player_data$Wins_percent, 2), "%")),
        p(strong("Hard Court Win %:"), paste0(round(player_data$Hard_wins_percent, 2), "%")),
        p(strong("Clay Court Win %:"), paste0(round(player_data$Clay_wins_percent, 2), "%")),
        p(strong("Grass Court Win %:"), paste0(round(player_data$Grass_wins_percent, 2), "%")),
        p(strong("Total Wins:"), player_data$Total_wins),
        p(strong("Hand:"), player_data$Hand)
      )
    }
    
    # Update player choices in the selectizeInput
    observe({
      updateSelectizeInput(session, "player_select_battle_1", choices = unique(dataset$Player_1))
      updateSelectizeInput(session, "player_select_battle_2", choices = unique(dataset$Player_1))
    })
    
    # Event listener for the predict button
    observeEvent(input$predict_button, {
      req(input$player_select_battle_1, input$player_select_battle_2)
      
      player1 <- input$player_select_battle_1
      player2 <- input$player_select_battle_2
      
      if (player1 == player2) {
        showNotification("Veuillez sélectionner deux joueurs différents.", type = "error")
        return()
      }
      
      result <- tryCatch({
        predict_winner_probability(player1, player2)
      }, error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error")
        return(NULL)
      })
      
      if (!is.null(result)) {
        # Extract player data
        player1_data <- extract_player_data(player1)
        player2_data <- extract_player_data(player2)
        
        # Display player information
        output$player1_info <- renderUI({
          create_player_info(player1_data, player1)
        })
        
        output$player2_info <- renderUI({
          create_player_info(player2_data, player2)
        })
        
        # Display win probability
        output$win_probability <- renderUI({
          tags$div(
            style = "text-align: center; padding: 20px;",
            h4("Probabilité de victoire", style = "color: black;"),
            p(strong(player1), ":", round(result$Probability_of_Winning[1] * 100, 2), "%"),
            p(strong(player2), ":", round(result$Probability_of_Winning[2] * 100, 2), "%")
          )
        })
      }
    })
    
    
    ### FIN PREMIERE PARTIE DUEL DE JOUEURS
    
    # Mettre à jour la liste des joueurs disponibles
    observe({
      updateSelectizeInput(session, "tournament_players", choices = unique(dataset$Player_1))
    })
    
    # Variables réactives pour stocker les résultats du tournoi
    tournament_results <- reactiveValues(
      quarterfinals = NULL,
      semifinals = NULL,
      final = NULL,
      winner = NULL
    )
    
    # Fonction pour afficher un match dans le bracket
    display_match <- function(player1, player2, winner = NULL) {
      # Obtenir les probabilités de victoire
      result <- predict_winner_probability(player1, player2)
      prob_player1 <- round(result$Probability_of_Winning[1] * 100, 2)
      prob_player2 <- round(result$Probability_of_Winning[2] * 100, 2)
      
      # Afficher le match avec les probabilités
      tags$div(
        class = if (!is.null(winner)) "match match-winner" else "match",
        p(strong(player1), " (", prob_player1, "%)"),
        p(strong(player2), " (", prob_player2, "%)"),
        if (!is.null(winner)) p(strong("Gagnant :"), winner)
      )
    }
    
    # Fonction pour simuler un match
    simulate_match <- function(player1, player2) {
      result <- predict_winner_probability(player1, player2)
      if (result$Probability_of_Winning[1] > result$Probability_of_Winning[2]) {
        return(player1)
      } else {
        return(player2)
      }
    }
    
    # Événement pour commencer le tournoi
    observeEvent(input$start_tournament, {
      req(input$tournament_players)  # Vérifier que 8 joueurs sont sélectionnés
      
      if (length(input$tournament_players) != 8) {
        showNotification("Veuillez sélectionner exactement 8 joueurs pour commencer le tournoi.", type = "error")
        return()
      }
      
      # Réinitialiser les résultats du tournoi
      tournament_results$quarterfinals <- NULL
      tournament_results$semifinals <- NULL
      tournament_results$final <- NULL
      tournament_results$winner <- NULL
      
      # Afficher les quarts de finale
      output$quarterfinals <- renderUI({
        players <- input$tournament_players
        tagList(
          tags$div(class = "round",
                   display_match(players[1], players[2]),
                   tags$div(class = "connector"),
                   display_match(players[3], players[4]),
                   tags$div(class = "connector"),
                   display_match(players[5], players[6]),
                   tags$div(class = "connector"),
                   display_match(players[7], players[8]))
        )
      })
      
      # Masquer les demi-finales, la finale et le vainqueur au début
      output$semifinals <- renderUI({ NULL })
      output$final <- renderUI({ NULL })
      output$winner <- renderUI({ NULL })
    })
    
    # Événement pour simuler les quarts de finale
    observeEvent(input$simulate_quarterfinals, {
      req(input$tournament_players)  # Vérifier que 8 joueurs sont sélectionnés
      
      # Simuler les quarts de finale
      players <- input$tournament_players
      tournament_results$quarterfinals <- c(
        simulate_match(players[1], players[2]),
        simulate_match(players[3], players[4]),
        simulate_match(players[5], players[6]),
        simulate_match(players[7], players[8])
      )
      
      # Afficher les résultats des quarts de finale
      output$quarterfinals <- renderUI({
        tagList(
          tags$div(class = "round",
                   display_match(players[1], players[2], tournament_results$quarterfinals[1]),
                   tags$div(class = "connector"),
                   display_match(players[3], players[4], tournament_results$quarterfinals[2]),
                   tags$div(class = "connector"),
                   display_match(players[5], players[6], tournament_results$quarterfinals[3]),
                   tags$div(class = "connector"),
                   display_match(players[7], players[8], tournament_results$quarterfinals[4]))
        )
      })
      
      # Afficher les demi-finales
      output$semifinals <- renderUI({
        tagList(
          tags$div(class = "round",
                   display_match(tournament_results$quarterfinals[1], tournament_results$quarterfinals[2]),
                   tags$div(class = "connector"),
                   display_match(tournament_results$quarterfinals[3], tournament_results$quarterfinals[4]))
        )
      })
    })
    
    # Événement pour simuler les demi-finales
    observeEvent(input$simulate_semifinals, {
      req(tournament_results$quarterfinals)  # Vérifier que les quarts de finale sont terminés
      
      # Simuler les demi-finales
      tournament_results$semifinals <- c(
        simulate_match(tournament_results$quarterfinals[1], tournament_results$quarterfinals[2]),
        simulate_match(tournament_results$quarterfinals[3], tournament_results$quarterfinals[4])
      )
      
      # Afficher les résultats des demi-finales
      output$semifinals <- renderUI({
        tagList(
          tags$div(class = "round",
                   display_match(tournament_results$quarterfinals[1], tournament_results$quarterfinals[2], tournament_results$semifinals[1]),
                   tags$div(class = "connector"),
                   display_match(tournament_results$quarterfinals[3], tournament_results$quarterfinals[4], tournament_results$semifinals[2]))
        )
      })
      
      # Afficher la finale
      output$final <- renderUI({
        tagList(
          tags$div(class = "round",
                   display_match(tournament_results$semifinals[1], tournament_results$semifinals[2]))
        )
      })
    })
    
    # Événement pour simuler la finale
    observeEvent(input$simulate_final, {
      req(tournament_results$semifinals)  # Vérifier que les demi-finales sont terminées
      
      # Simuler la finale
      tournament_results$winner <- simulate_match(tournament_results$semifinals[1], tournament_results$semifinals[2])
      
      # Afficher le résultat de la finale
      output$final <- renderUI({
        tagList(
          tags$div(class = "round",
                   display_match(tournament_results$semifinals[1], tournament_results$semifinals[2], tournament_results$winner))
        )
      })
      
      # Afficher le vainqueur
      output$winner <- renderUI({
        tagList(
          h4("Vainqueur 🏆"),
          p(strong(tournament_results$winner))
        )
      })
    })
  })
}