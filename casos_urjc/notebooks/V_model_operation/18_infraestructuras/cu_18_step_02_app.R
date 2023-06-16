########
# APP PASO 2 (PARÁMETROS DE USUARIO) CU 18 (Comportamiento Infra. Eventos extremos)
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
    titlePanel(title = "Parámetros de usuario - CitizenLab CU 18"),
    fluidRow(
      column(2,
             uiOutput("uimodelo"),
      ),
      column(2,
             uiOutput("uinivel"),
      ),
      column(2,
             uiOutput("uinsim"),
      )
      
    ),
    fluidRow(
      column(2,
             textOutput("textmodelo")
      ),
      column(2,
             textOutput("textnivel")
      ),
      column(2,
             textOutput("textnsim")
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
  
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })
  
  
  observeEvent(input$abguardar, {
    ## Guardar variables en output
    VARIABLES <- dfvariables()
    VARIABLES$valor[VARIABLES$variable == "MODELO"] <- input$modelo
    VARIABLES$valor[VARIABLES$variable == "NIVEL"] <- input$nivel
    VARIABLES$valor[VARIABLES$variable == "NSIM"] <- input$nsim

    write_csv(VARIABLES, paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))

    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_18_05_03_distritos_geo.json"),
              paste0(carpetas()$carpeta_salida, "/CU_18_05_03_distritos_geo.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_18_05_16_distritos_variables.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_18_05_16_distritos_variables.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_18_05_19_01_infraestructuras.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_18_05_19_01_infraestructuras.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_18_05_20_diario_infra.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_18_05_20_diario_infra.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_CLUSTER_DIARIO.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO_CLUSTER_DIARIO.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_CLUSTER_DIST.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO_CLUSTER_DIST.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_REGRESION.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO_REGRESION.csv"))

    sendSweetAlert(
      session = session,
      title = "¡¡ Éxito !!",
      text = "Se han guardado los ficheros para el siguiente paso del caso.",
      type = "success"
    )
  })
  
  ## render outputs ----
  
  ## dynamic UI ----
  
  # Render the inputs
  output$uimodelo <- renderUI({
    selectInput(
      inputId = "modelo",
      label = "Modelo del experimento",
      choices = c("Clustering", "Regresión", "Simulación"), 
      selected = "CLustering"    
      )
  })

    output$textmodelo <- renderText({
    dfvariables() |> filter(variable == "MODELO") |> pull(descripcion)
  })

    output$uinivel <- renderUI({
    selectInput(
      inputId = "nivel",
      label = "Nivel de detalle",
      choices = c("Distrito", "Diario"),
      selected = "Distrito"    
      
    )
  })

    output$textnivel <- renderText({
    dfvariables() |> filter(variable == "NIVEL") |> pull(descripcion)
  })

    output$uinsim <- renderUI({
    numericInput(
      inputId = "nsim",
      label = "Número de simulaciones",
      value = 1000,
      min = 10,
      max = 1000000
    )
  })

  output$textnsim <- renderText({
    dfvariables() |> filter(variable == "NSIM") |> pull(descripcion)updateSelectInput(session, "variable_plot",

                        choices = colnames(dfhistorico() %>% select(where(is.numeric))))
  })

}

shinyApp(ui, server)