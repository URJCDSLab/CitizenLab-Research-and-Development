library(shiny)
library(bslib)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Carga de datos - CitizenLab CU 04"),
    mainPanel(
      fluidRow(
        h3("Fichero de zonas"),
        column(6,
               tabsetPanel(id = "tszonas",
                 tabPanel("Import data", 
                          fileInput("file", "Data", buttonLabel = "Upload..."),
                          textInput("delim", "Delimiter (leave blank to guess)", ""),
                          numericInput("skip", "Rows to skip", 0, min = 0),
                          numericInput("rows", "Rows to preview", 10, min = 1)
                 ),
                 tabPanel("Set parameters"),
                 tabPanel("Visualise results")
               )
        ),
        column(6,
               p("kk")),
        fluidRow("fff")
      )
    )
  )}

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
