########
# APP PASO 2 (PARÁMETROS DE USUARIO) CU 18 (Comportamiento Infra. Eventos extremos)
########

## Paquetes ----


## UI
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(leaflet)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(janitor)
library(cluster)
library(nnet)
library(DT)


ui <- function(request) {

  fluidPage(

    theme = bs_theme(bootswatch = "flatly"),

    useShinydashboard(),

    titlePanel(title = "Predicción - CitizenLab CU 34"),

    # ... Otros elementos de la UI

    tabsetPanel(
        tabPanel("Tabla detalle predicciones",
            column(width = 12, dataTableOutput("pred_table"))
        ),
        tabPanel("Gráfico barras predicciones",
            column(width = 12, plotOutput("pred_chart"))
        )
    )
  )
}


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

  # Dataframes

  dfvariables <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "VARIABLES.csv"), show_col_types = FALSE)
  })

  dfservicios <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "SERVICIOS.csv"), show_col_types = FALSE)
  })

  dfescenarios <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "ESCENARIOS.csv"), show_col_types = FALSE)
  })

  sfsecciones <- reactive({
    read_sf(file.path(carpetas()$carpeta_entrada, "SECCIONES.json"))
  })

  modelo_ann <- reactive({
   read_rds(file.path(carpetas()$carpeta_maestros, "modelo_nnet.rds"))
  })

  dfpred <- reactive({
    data <- dfescenarios()
    predict(modelo_ann(), dfescenarios()) |> round(2)
  })

  dfpredclass <- reactive({
    data <- dfescenarios()
    predict(modelo_ann(), dfescenarios(), type = "class")
  })

  ## render outputs ----

  ## dynamic UI ----

  # Render the inputs
  output$pred_chart <- renderPlot({
    barplot(table(dfpredclass()), main = "Frecuencia de los cluster (predicción)", xlab = "Cluster", ylab = "Frequency")
  })

  output$pred_table <- renderDataTable({
    datatable(dfpred(),
              options = list(scrollX = TRUE))
  })
}

shinyApp(ui, server)