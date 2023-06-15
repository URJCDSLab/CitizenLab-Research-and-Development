########
# APP PASO 2 (PARÁMETROS DE USUARIO) CU 45 (GESTIÓN VACUNAS GRIPE)
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
library(stringr)
library(purrr)
library(tibble)
library(tidyr)


ui <- function(request){
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Parámetros de usuario - CitizenLab CU 45"),
    fluidRow(
      column(2,
             uiOutput("uinclus")
      ),
        
      column(2,
             uiOutput("uikrieg")
      )
      
    ),
    fluidRow(
      column(2,
             textOutput("textnclus")
      ),
        
      column(2,
             textOutput("textkrieg")
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
           is.null(carpeta_salida))){
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
                            carpeta_salida = carpeta_salida)))
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
  
  
  observeEvent(input$abguardar, {
    ## Guardar variables en output
    VARIABLES <- dfvariables()
    VARIABLES$valor[VARIABLES$variable == "NCLUS"] <- input$nclus
    VARIABLES$valor[VARIABLES$variable == "KRIG_OBJ"] <- input$krieg
      
    write_csv(VARIABLES, paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))

    file.copy(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
              paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_02_valoracion_sim.json"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_02_valoracion_sim.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_01_municipios_geo.json"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_01_municipios_geo.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_04_interno_prov.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_04_interno_prov.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_05_interno_mun.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_05_interno_mun.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_03_receptor.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_03_receptor.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_REG.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO_REG.csv"))

    sendSweetAlert(
      session = session,
      title = "¡¡ Éxito !!",
      text = "Se han guardado los ficheros para el siguiente paso del caso.",
      type = "success"
    )
  })
  
  ## render outputs ----
  
  ## dynamic UI ----
  
  # Render the 'NCLUS' input
  output$uinclus <- renderUI({
    numericInput(
      inputId = "nclus",
      label = "Número de Clusters",
      value = 4,
      min = 2,
      max = 10
    )
  })
  output$uikrieg <- renderUI({
    selectInput(
      inputId = "krieg",
      label = "Sector objetivo",
      choices = c("turismo", "hosteleria", "comercio")
    )
  })

  output$textnclus <- renderText({
    dfvariables() |> filter(variable == "NCLUS") |> pull(descripcion)
  })

  output$textkrieg <- renderText({
    dfvariables() |> filter(variable == "KRIG_OBJ") |> pull(descripcion)
  })

}

shinyApp(ui, server)