a_propos_server <- function(id, dataset, player_stat) {
  moduleServer(id, function(input, output, session) {
    output$dataset1_table <- renderDT({
      datatable(head(dataset, 30),  # Limite à 30 lignes
                options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE))
    })
    
    output$dataset2_table <- renderDT({
      datatable(head(player_stat, 30),  # Limite à 30 lignes
                options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE))
    })
    
  })
}

