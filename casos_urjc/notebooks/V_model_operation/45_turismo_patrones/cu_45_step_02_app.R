########
# APP PASO 2 (PARÁMETROS DE USUARIO) CU 04 (GESTIÓN VACUNAS GRIPE)
########

## Paquetes ----

## UI
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)


ui <- function(request){
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Parámetros de usuario - CitizenLab CU 45"),
    fluidRow(
      column(2,
             uiOutput("uihorizonte"),
      ),
      column(2,
             uiOutput("uinper"),
      )
      
    ),
    fluidRow(
      column(2,
             textOutput("texthorizonte")
      ),
      column(2,
             textOutput("textnper")
      )
      
    ),
    hr(),
    # fluidRow(
    h3("Guardar datos para el siguiente paso"),
    actionBttn("abguardar",
               "Guardar datos",
               size = "md",
               icon = icon("floppy-disk")),
    br(),br()
    # )
    
  )
}

server <- function(input, output, session) {
  ## Reactives ----
  carpetas <- reactive({
    
    carpeta_entrada <- getQueryString()$carpeta_entrada
    carpeta_salida <- getQueryString()$carpeta_salida
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
                   file.exists(carpeta_salida))){
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
  
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })
  
  dfhistorico <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/HISTORICO.csv"), 
             show_col_types = FALSE)
  })
  
  dfindicadoresmeta <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/INDICADORES_META.csv"), 
             show_col_types = FALSE)
  })
  
  dfindicadores <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/INDICADORES.csv"), 
             show_col_types = FALSE)
  })
  dfescucha <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/ESCUCHA.csv"), 
             show_col_types = FALSE)
  })
  
  dfpredictores <- reactive({
    data.frame(predictor = colnames(dfhistorico()),
               fichero = "Histórico") |> 
      bind_rows(data.frame(predictor = colnames(dfindicadores()),
                           fichero = "Indicadores")) |> 
      bind_rows(data.frame(predictor = colnames(dfescucha()),
                           fichero = "Escucha")) |> 
      left_join(dfindicadoresmeta(),
                by = c("predictor" = "Tabla")) |> 
      mutate(Indicador = if_else(is.na(Indicador), 
                                 predictor, 
                                 Indicador)) |> 
      filter(!(predictor %in% c("GEOCODIGO", "DESBDT", "ano", "semana", "n_vacunas")))
  })
  
  
  observeEvent(input$abguardar, {
    ## Guardar variables en output
    VARIABLES <- dfvariables()
    VARIABLES$valor[VARIABLES$variable == "HORIZONTE"] <- input$horizonte
    VARIABLES$valor[VARIABLES$variable == "NPER"] <- input$nper


    write_csv(VARIABLES, paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))

    file.copy(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
              paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_01_municipios_geo.json"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_01_municipios_geo.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_04_interno_prov.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_04_interno_prov.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_05_interno_mun.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_05_interno_mun.csv"))

    sendSweetAlert(
      session = session,
      title = "¡¡ Éxito !!",
      text = "Se han guardado los ficheros para el siguiente paso del caso.",
      type = "success"
    )
  })
  
  ## render outputs ----
  
  ## dynamic UI ----
  
  # Render the 'Horizonte' input
  output$uihorizonte <- renderUI({
    selectInput(
      inputId = "horizonte",
      label = "Horizonte temporal (semanas)",
      choices = seq(1, 52),
      selected = 1
    )
  })

  output$textano <- renderText({
    dfvariables() |> filter(variable == "HORIZONTE") |> pull(descripcion)
  })
  
    # Render the 'NPER' input
  output$uinper <- renderUI({
    numericInput(
      inputId = "nper",
      label = "Número de periodos (días) a simular",
      value = 365,
      min = 1,
      max = 7300
    )
  })

    output$textnper <- renderText({
    dfvariables() |> filter(variable == "NPER") |> pull(descripcion)
  })

}

shinyApp(ui, server)