home_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Calcul des statistiques dynamiques
    output$total_matches <- renderText({
      format(nrow(dataset), big.mark = ",")
    })
    
    output$unique_players <- renderText({
      players <- unique(c(dataset$Player_1, dataset$Player_2))
      format(length(players), big.mark = ",")
    })
    
    output$date_range <- renderText({
      start_date <- min(as.Date(dataset$Date))
      end_date <- max(as.Date(dataset$Date))
      paste(format(start_date, "%Y"), "-", format(end_date, "%Y"))
    })

  })
}
