library(shiny)
library(bslib)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Carga de datos - CitizenLab CU 04"),
      fluidRow(
        h3("StopApp test"),
        actionButton("stopapp", "Stop App")
    )
  )}

server <- function(input, output, session) {
  observeEvent("input$stopapp", {
    stopApp(returnValue = 1)
  })
}

shinyApp(ui, server)
