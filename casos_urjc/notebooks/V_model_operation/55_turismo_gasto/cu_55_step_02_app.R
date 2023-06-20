########
# APP PASO 2 (PARÁMETROS DE USUARIO) CU 55 (GASTOS TURISTAS)
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
    titlePanel(title = "Parámetros de usuario - CitizenLab CU 55"),
    fluidRow(
     column(2,
             uiOutput("uinsimul")
      ),
        
      column(2,
             uiOutput("uitipoesc")
      )
      
    ),
fluidRow(
      column(2,
             textOutput("textnsimul")
      ),
        
      column(2,
             textOutput("texttipoesc")
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
    VARIABLES$valor[VARIABLES$variable == "NSIM"] <- input$nsimul
    VARIABLES$valor[VARIABLES$variable == "TIPOESC"] <- input$tipoesc
      
    write_csv(VARIABLES, paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))

    file.copy(paste0(carpetas()$carpeta_entrada, "/ZONAS.json"),
              paste0(carpetas()$carpeta_salida, "/ZONAS.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/GASTOCOM.csv"),
              paste0(carpetas()$carpeta_salida, "/GASTOCOM.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/GASTO.csv"),
              paste0(carpetas()$carpeta_salida, "/GASTO.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_ORIGEN.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO_ORIGEN.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_DESTINO.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO_DESTINO.csv"))


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
 output$uinsimul <- renderUI({
    numericInput(
      inputId = "nsimul",
      label = "Número de simulaciones",
      value = 1000,
      min = 10,
      max = 1e+06
    )
  })
  output$uitipoesc <- renderUI({
    selectInput(
      inputId = "tipoesc",
      label = "Tipo de escenario",
      choices = c("Destino", "Origen")
    )
  })

output$textnsimul <- renderText({
    dfvariables() |> filter(variable == "NSIM") |> pull(descripcion)
  })

  output$texttipoesc <- renderText({
    dfvariables() |> filter(variable == "TIPOESC") |> pull(descripcion)
  })

}

shinyApp(ui, server)