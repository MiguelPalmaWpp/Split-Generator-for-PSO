# Posit Connect entry point. global.R is loaded by Shiny before this file;
# ui.R and server.R retain the app's existing public module wiring.
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)
