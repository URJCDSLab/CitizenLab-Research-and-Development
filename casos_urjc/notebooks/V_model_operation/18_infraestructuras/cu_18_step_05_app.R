# APP PASO 5 (PREDICCIÓN)
## Paquetes
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(bslib, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
#library(leafem)
library(leaflet)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)
# if (!require(mapSpain)) {
#     install.packages("mapSpain")
# }
# library(mapSpain)

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

# UI
ui <- function(request) {

  fluidPage(

    theme = bs_theme(bootswatch = "flatly"),

    useShinydashboard(),

    titlePanel(title = "Visualización - CitizenLab CU 18"),




    # ... Otros elementos de la UI

   

    sidebarLayout(

      sidebarPanel(

        column(width = 6,
               uiOutput("uimodelo"),
               uiOutput("select_variable")

        )

      ),

      mainPanel(

        tabsetPanel(

                    tabPanel("Datos Modelo",

                  column(width = 12,
                   verbatimTextOutput("modelo_resumen"),
                   plotOutput("dispersion")
            )),

          tabPanel("Predicción",

                   column(width = 12,

                          DT::dataTableOutput("table_data")

                   )

          )

        )

      )

    )

  )
  

}


# SERVER
server <- function(input, output, session) {
  ## Reactives ----
  carpetas <- reactive({
    
    carpeta_entrada <- getQueryString()$carpeta_entrada
    carpeta_salida <- getQueryString()$carpeta_salida
    carpeta_maestros <- getQueryString()$carpeta_maestros
    if(any(is.null(carpeta_entrada), 
           is.null(carpeta_salida),
           is.null(carpeta_maestros))){
      confirmSweetAlert(
        session = session,
        inputId = "error_carpetas_faltan",
        title = "Error",
        text = "Revise la url, alguna carpeta requerida en el caso no se ha especificado (carpeta_entrada, carpeta_salida, carpeta_maestros). La aplicación se cerrará.",
        type = "error",
        btn_labels = c("", "Cerrar"),
        btn_colors = c("white", "red")
      )
      warning("Revise la url, alguna carpeta requerida en el caso no se ha especificado (carpeta_entrada, carpeta_salida, carpeta_maestros).")
      invisible(NULL)
    } else if(!all(file.exists(carpeta_entrada), 
                   file.exists(carpeta_salida), 
                   file.exists(carpeta_maestros))){
      confirmSweetAlert(
        session = session,
        inputId = "error_carpetas_existen",
        title = "Error",
        text = "Revise la url, alguna carpeta requerida en el caso no existe. La aplicación se cerrará.",
        type = "error",
        btn_labels = c("", "Cerrar"),
        btn_colors = c("white", "red")
      )
      warning("Revise la url, alguna carpeta requerida en el caso no existe.")
      invisible(NULL)
    } else{
      return(invisible(list(carpeta_entrada = carpeta_entrada,
                            carpeta_salida = carpeta_salida,
                            carpeta_maestros = carpeta_maestros)))
    }
  })
  
  ## observers ----
  
  observeEvent(input$error_carpetas_faltan,{
    stopApp()
  })
  
  observeEvent(input$error_carpetas_existen,{
    stopApp()
  })

  ## Read variable data
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })

  escenario <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "/ESCENARIO_REGRESION.csv"))
  })
  
  ## MODELO
  modelo_zona <- reactive({
    read_rds(file.path(carpetas()$carpeta_maestros,"/mod_glm_zona.rds"))})
  
  modelo_infra <- reactive({
    read_rds(file.path(carpetas()$carpeta_maestros,"/mod_glm_infra.rds"))})

      # Render the inputs
  output$uimodelo <- renderUI({
    selectInput(
      inputId = "modelo",
      label = "Modelo de predicción",
      choices = c("Infraestructuras", "Zonas"), 
      selected = "Infraestructuras"    
      )
  })

  output$select_variable <- renderUI({
  selectInput(
    inputId = "variable",
    label = "Variable para gráfico de dispersión",
    choices = names(escenario()), 
    selected = names(escenario())[1]    
  )
})

  output$modelo_resumen <- renderPrint({
    req(input$modelo)
    if (input$modelo == "Infraestructuras") {
        summary(modelo_infra())}else{summary(modelo_zona())
        }

  })

    output$dispersion <- renderPlot({
        req(input$modelo)
    if (input$modelo == "Infraestructuras") {
    modelo_infra()$model %>%
      ggplot(aes_string(x = input$variable, y = "evento_infra")) +
      geom_point(alpha = 0.1) }else{
         modelo_zona()$model %>%
      ggplot(aes_string(x = input$variable, y = "evento_zona")) +
      geom_point(alpha = 0.1) 
      }
  })



  output$table_data <- DT::renderDataTable({
    req(input$modelo)
    if (input$modelo == "Infraestructuras") {

      datatable(escenario() |> mutate(Prob.evento = predict(modelo_infra(), escenario(), type = "response")), options=list(scrollX = TRUE))

    }else{

      datatable(escenario() |> mutate(Prob.evento = predict(modelo_zona(), escenario(), type = "response")), options=list(scrollX = TRUE))

    } 

  })
}

shinyApp(ui, server)


