library(shiny)

# Carregar os arquivos UI e Server
source("ui.R")
source("server.R")

# setwd("C:/Users/quiqu/OneDrive/Área de Trabalho/CancerRCDShiny/CancerRCDShiny")

# Iniciar a aplicação
shinyApp(ui, server)

# Rum the app
shiny::runApp()


