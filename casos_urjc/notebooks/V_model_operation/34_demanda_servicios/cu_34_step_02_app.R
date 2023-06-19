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
    titlePanel(title = "Parámetros de usuario - CitizenLab CU 34"),
    fluidRow(
      column(2,
             uiOutput("uinsim")
      )
    ),
    fluidRow(
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
    VARIABLES$valor[VARIABLES$variable == "NSIM"] <- input$nsim

    write_csv(VARIABLES, paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))

    file.copy(paste0(carpetas()$carpeta_entrada, "/SECCIONES.json"),
              paste0(carpetas()$carpeta_salida, "/SECCIONES.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIOS.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIOS.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/SERVICIOS.csv"),
              paste0(carpetas()$carpeta_salida, "/SERVICIOS.csv"))

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

  output$uinsim <- renderUI({
    numericInput(
      inputId = "nsim",
      label = "Número de simulaciones",
      value = as.numeric(dfvariables() |> filter(variable == "NSIM") |> pull(valor)),
      min = as.numeric(dfvariables() |> filter(variable == "NSIM") |> pull(min)),
      max = as.numeric(dfvariables() |> filter(variable == "NSIM") |> pull(max))
    )
  })

  output$textnsim <- renderText({
    dfvariables() |> filter(variable == "NSIM") |> pull(descripcion)
  })

}

shinyApp(ui, server)