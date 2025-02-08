battle_ui <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "battle",
    bs4Card(
      title = "Duel de Joueurs",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      "Choisissez deux joueurs pour prédire le vainqueur grâce à notre modèle de machine learning."
    )
  )
}
