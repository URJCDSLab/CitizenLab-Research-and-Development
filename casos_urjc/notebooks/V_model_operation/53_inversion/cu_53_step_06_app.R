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


## Server
library(readr)
library(mgcv)
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)
library(glmnet)
library(lpSolve)

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)




ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),
    titlePanel(title = "Optimización - CitizenLab CU 53"),
    navbarPage("Optimización",
    id="paneloptimizacion",
    tabPanel(
      title="Optimizacion",
      fluidRow(
        column(width = 4,
              sidebarPanel(
                p("Para lanzar el cálculo, haga click en en botón 'Optimizar'"),
                p("Ten en cuenta que puede llevar un tiempo."),
                actionButton("calculate_button", "Optimizar"),
                br(),
                br(),
                #dataTableOutput("input_table")
              )
        ),
        column(width = 8,
              mainPanel(
                dataTableOutput("result_table")
              )
        )
      )
    )
    )
)

}

server <- function(input, output, session) {

  ## Reactives ----

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


  optimize_scenarios <- function() {
    # Code for your model calculation
    # Replace the following with your actual model calculation code
    parameters <- dfparameters()

    modelo <- parameters %>%
      filter(variable == "MODELO") %>%
      pull(valor)

    anyoopt <- parameters %>%
      filter(variable == "ANYOOPT") %>%
      pull(valor) |> as.numeric()

    restinvtot <- parameters %>%
      filter(variable == "RESTINVTOT") %>%
      pull(valor) |> as.numeric()

    restinvinf <- parameters %>%
      filter(variable == "RESTINVINF") %>%
      pull(valor) |> as.numeric()

    restinvtur <- parameters %>%
      filter(variable == "RESTINVTUR") %>%
      pull(valor) |> as.numeric()

    restinvsan <- parameters %>%
      filter(variable == "RESTINVSAN") %>%
      pull(valor) |> as.numeric()

    cc <- coef(mod_53_reg())
    b0 <- cc[1,1] + anyoopt*cc[2,1]
    f.obj <- cc[3:5]
    f.con <- matrix(c(1, 1, 1,
                      1, 0, 0,
                      0, 1, 0,
                      0, 0, 1), nrow = 4, byrow = TRUE)
    f.dir <- c("<=",
              ">=",
              ">=",
              ">=")
    f.rhs <- c(restinvtot, restinvinf, restinvtur, restinvsan)
    res <- lp("max", f.obj, f.con, f.dir, f.rhs)

    ## Resultado optimización, valor óptimo:
    res$objval + b0

    ## Resultado optimización, inversiones necesarias:
    data.frame(grupo = rownames(cc)[3:5],
              porc_inv = res$solution)
  }

  ## observers ----
  observeEvent(input$calculate_button, {

    # Long-running model calculation
    result_df <- optimize_scenarios()

    output$result_table <- renderDataTable(datatable(result_df))
  })

  observeEvent(input$error_carpetas_faltan,{
    stopApp()
  })

  observeEvent(input$error_carpetas_existen,{
    stopApp()
  })

  ## . Data frames ----
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

  mod_53_reg <- reactive({
    read_rds(file.path(carpetas()$carpeta_entrada, "MODELO_REG.rds"))
  })

  dfparameters <- reactive({
    dfvariables() |> select(variable, valor)
  })

  ## . outputs ----
  #output$input_table <- renderDataTable({
  #  datatable(dfparameters())
  #})
}

shinyApp(ui, server)
