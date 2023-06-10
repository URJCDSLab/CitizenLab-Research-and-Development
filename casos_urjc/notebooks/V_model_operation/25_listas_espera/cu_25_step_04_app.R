########
# APP PASO 4 (PROYECCIÓN CAMPAÑA)
########

library(shiny)

## UI
library(bslib, warn.conflicts = FALSE)
library(shinycssloaders)
library(shinydashboard, warn.conflicts = FALSE)
library(shinyWidgets)
library(DT, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(gratia)
library(leaflet)
library(waiter)
library(xgboost)

## Server
library(readr)
library(mgcv)
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),
    
    titlePanel(title = "Proyección campaña - CitizenLab CU 04"),

    # ... Otros elementos de la UI
    
    fluidRow(
      column(width = 6,
             uiOutput("uihorizonte")
      ),
      column(width = 6,
             uiOutput("uiespecialidad")
      ),

      column(width = 6,
             uiOutput("uiparametro")
      ),
        column(width = 12,
         plotlyOutput("serieTemporalPlot")
  )
    )
  )
}


server <- function(input, output, session) {
  

  
  ## . carpetas ----
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
  
    # Resto del código del servidor...
  
  dfhistorico <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_25_05_07_02_lista_espera.csv"), 
             show_col_types = FALSE)
  })

    dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })
  
  # Cargar modelos Prophet/XGBoost

  modelos_xgboost <- reactive({
    read_rds(paste0(carpetas()$carpeta_maestros, 
                    "/modelos_tiempo_xgboost.rds"))
  })

  
  output$uihorizonte <- renderUI({
  selectInput(
    inputId = "horizonte",
    label = "Horizonte temporal (semanas)",
    choices = seq(1, 52),
    selected = dfvariables()$valor[dfvariables()$variable == "HORIZONTE"]
  )
})

  output$uiespecialidad <- renderUI({
    selectInput(
      inputId = "especialidad",
      label = "Especialidad",
      choices = unique(dfhistorico()$Especialidad),
      selected = NULL
    )
  })
  

  output$uiparametro <- renderUI({
    selectInput(
      inputId = "parametro",
      label = "Parámetro a predecir",
      choices = c("media_tiempo_dias", "pacientes"),
      selected = NULL
    )
  })



output$serieTemporalPlot <- renderPlotly({
  # Obtener los datos históricos
  datosHistoricos <- dfhistorico()
  
  # Obtener el parámetro seleccionado
  parametro <- input$parametro
  print("00000000")
  # Filtrar los datos históricos por especialidad
  especialidad <- input$especialidad
  datosHistoricos <- datosHistoricos %>% filter(Especialidad == especialidad)
  
  # Obtener las fechas y valores históricos
 

# Obtener las fechas y valores históricos
print("1111111111")
  ano <- datosHistoricos$ano
  semana <- datosHistoricos$semana
  fechas <- as.Date(paste(ano, semana, 1, sep = "-"), format = "%Y-%U-%u")
  print("222")
  valoresHistoricos <- datosHistoricos[[parametro]]
  print(valoresHistoricos)
  print(fechas)
  # Generar las predicciones utilizando el modelo XGBoost

  fechasPrediccion <- seq(max(fechas) + 1, max(fechas) + as.numeric(input$horizonte) * 7, by = "day")
  datosPrediccion <- data.frame(Fecha = fechasPrediccion)
  datosPrediccion[[parametro]] <- predict(xgb.load(modelos_xgboost()), newdata = datosPrediccion)
  print("33")
  # Unir los datos históricos y de predicción
  datos <- bind_rows(datosHistoricos, datosPrediccion)
  
  # Crear la serie temporal
  serieTemporal <- ggplot(data = datos, aes(x = Fecha, y = .data[[parametro]])) +
    geom_line() +
    labs(x = "Fecha", y = parametro) +
    theme_minimal()
  
  # Convertir la serie temporal a plotly para una mejor interactividad
  serieTemporal <- ggplotly(serieTemporal)
  
  # Devolver el gráfico
  return(serieTemporal)
})

}

shinyApp(ui, server)
