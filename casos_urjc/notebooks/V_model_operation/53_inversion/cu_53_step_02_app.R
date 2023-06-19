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

# We only need to plot:
# score_bhn          | score_fow          | score_opp

ui <- function(request){
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Parámetros de usuario - CitizenLab CU 53"),
    fluidRow(
      column(2,
             uiOutput("uimodelo"),
      ),
      column(2,
             uiOutput("uianyoopt"),
      ),
      column(2,
             uiOutput("uirestinvtot"),
      ),
      column(2,
             uiOutput("uirestinvinf"),
      ),
      column(2,
             uiOutput("uirestinvtur"),
      ),
      column(2,
             uiOutput("uirestinvsan"),
      )

    ),
    fluidRow(
      column(2,
             textOutput("textmodelo"),
      ),
      column(2,
             textOutput("textanyoopt"),
      ),
      column(2,
             textOutput("textrestinvtot"),
      ),
      column(2,
             textOutput("textrestinvinf"),
      ),
      column(2,
             textOutput("textrestinvtur"),
      ),
      column(2,
             textOutput("textrestinvsan"),
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
    read_csv(file.path(carpetas()$carpeta_entrada, "VARIABLES.csv"), show_col_types = FALSE)
  })

  dfinversiones <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "INVERSIONES_PAISES.csv"), show_col_types = FALSE)
  })

  dfinversionescmdetail <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "INVERSIONES_REGION_DETAIL.csv"), show_col_types = FALSE)
  })

  dfinversionescm <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "INVERSIONES_REGION.csv"), show_col_types = FALSE)
  })

  dfspimeta <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "SPI_META.csv"), show_col_types = FALSE)
  })

  dfspi <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "SPI.csv"), show_col_types = FALSE)
  })

  sfpaises <- reactive({
    read_sf(paste0(carpetas()$carpeta_entrada, "/PAISES.json"))
  })

  observeEvent(input$abguardar, {
    ## Guardar variables en output
    VARIABLES <- dfvariables()
    VARIABLES$valor[VARIABLES$variable == "MODELO"] <- input$simodelo
    VARIABLES$valor[VARIABLES$variable == "ANYOOPT"] <- input$sianyo
    VARIABLES$valor[VARIABLES$variable == "RESTINVTOT"] <- input$sirestinvtot
    VARIABLES$valor[VARIABLES$variable == "RESTINVINF"] <- input$sirestinvinf
    VARIABLES$valor[VARIABLES$variable == "RESTINVTUR"] <- input$sirestinvur
    VARIABLES$valor[VARIABLES$variable == "RESTINVSAN"] <- input$sirestinvan

    write_csv(VARIABLES, paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))
    st_write(obj = sfpaises(),
             dsn = paste0(carpetas()$carpeta_salida, "/PAISES.json"),
             driver = "GeoJSON",
             delete_dsn = TRUE)
    write_csv(dfinversiones(), paste0(carpetas()$carpeta_salida, "/INVERSIONES_PAISES.csv"))
    write_csv(dfinversionescm(), paste0(carpetas()$carpeta_salida, "/INVERSIONES_REGION.csv"))
    write_csv(dfinversionescmdetail(), paste0(carpetas()$carpeta_salida, "/INVERSIONES_REGION_DETAIL.csv"))
    write_csv(dfspi(), paste0(carpetas()$carpeta_salida, "/SPI.csv"))
    write_csv(dfspimeta(), paste0(carpetas()$carpeta_salida, "/SPI_META.csv"))
    ## Copiar resto input a output para siguientes pasos

    sendSweetAlert(
      session = session,
      title = "¡¡ Éxito !!",
      text = "Se han guardado los ficheros para el siguiente paso del caso.",
      type = "success"
    )
  })

  ## render outputs ----

  ## dynamic UI ----

  # Modelo
  output$uimodelo <- renderUI({
     selectInput(
      inputId = "simodelo",
      label = "Modelo",
      choices = c("Regresión", "Serie Temporal", "Optimización"),
      selected = "Regresión"
    )
  })
  output$textmodelo <- renderText({
    dfvariables() |> filter(variable == "MODELO") |> pull(descripcion)
  })

  # Anyo opt
  output$uianyoopt <- renderUI({
     selectInput(
      inputId = "sianyo",
      label = "Anyo optimizacion",
      choices = seq(dfvariables() |> filter(variable == "ANYOOPT") |> pull(min), dfvariables() |> filter(variable == "ANYOOPT") |> pull(max)),
      selected = dfvariables() |> filter(variable == "ANYOOPT") |> pull(min)
    )
  })
  output$textanyoopt <- renderText({
    dfvariables() |> filter(variable == "ANYOOPT") |> pull(descripcion)
  })

  # Inv tot
  output$uirestinvtot <- renderUI({
    numericInput(
      inputId = "sirestinvtot",
      label = dfvariables() %>%
        filter(variable == "RESTINVTOT") %>%
        pull(etiqueta),
      value = dfvariables() %>%
        filter(variable == "RESTINVTOT") %>%
        pull(valor),
      min = dfvariables() %>%
        filter(variable == "RESTINVTOT") %>%
        pull(min),
      max = dfvariables() %>%
        filter(variable == "RESTINVTOT") %>%
        pull(max)
    )
  })
  output$textrestinvtot <- renderText({
    dfvariables() |> filter(variable == "RESTINVTOT") |> pull(descripcion)
  })

  # Inv inf
  output$uirestinvinf <- renderUI({
    numericInput(
      inputId = "sirestinvinf",
      label = dfvariables() %>%
        filter(variable == "RESTINVINF") %>%
        pull(etiqueta),
      value = dfvariables() %>%
        filter(variable == "RESTINVINF") %>%
        pull(valor),
      min = dfvariables() %>%
        filter(variable == "RESTINVINF") %>%
        pull(min),
      max = dfvariables() %>%
        filter(variable == "RESTINVINF") %>%
        pull(max)
    )
  })

  output$textrestinvinf <- renderText({
    dfvariables() |> filter(variable == "RESTINVINF") |> pull(descripcion)
  })

  # Inv ur

  output$uirestinvtur <- renderUI({
    numericInput(
      inputId = "sirestinvur",
      label = dfvariables() %>%
        filter(variable == "RESTINVTUR") %>%
        pull(etiqueta),
      value = dfvariables() %>%
        filter(variable == "RESTINVTUR") %>%
        pull(valor),
      min = dfvariables() %>%
        filter(variable == "RESTINVTUR") %>%
        pull(min),
      max = dfvariables() %>%
        filter(variable == "RESTINVTUR") %>%
        pull(max)
    )
  })

  output$textrestinvtur <- renderText({
    dfvariables() |> filter(variable == "RESTINVTUR") |> pull(descripcion)
  })

  # Inv an

  output$uirestinvsan <- renderUI({
    numericInput(
      inputId = "sirestinvan",
      label = dfvariables() %>%
        filter(variable == "RESTINVSAN") %>%
        pull(etiqueta),
      value = dfvariables() %>%
        filter(variable == "RESTINVSAN") %>%
        pull(valor),
      min = dfvariables() %>%
        filter(variable == "RESTINVSAN") %>%
        pull(min),
      max = dfvariables() %>%
        filter(variable == "RESTINVSAN") %>%
        pull(max)
    )
  })

  output$textrestinvsan <- renderText({
    dfvariables() |> filter(variable == "RESTINVSAN") |> pull(descripcion)
  })

}

shinyApp(ui, server)