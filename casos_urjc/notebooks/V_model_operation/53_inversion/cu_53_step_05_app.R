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
library(tsibble)
library(feasts)
library(forecast)
library(fable)

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)




ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),

    titlePanel(title = "Proyección SPIs (ARIMA) - CitizenLab CU 53"),
    navbarPage("Modelo proyección",
          id = "panelproyeccion",
          tabPanel(
            title = "Escenario",
            dataTableOutput("tescenario")),
          tabPanel(
            title = "Modelo",
            fluidRow(h4("Poisson Generalized Additive Model (GAM)")),
            fluidRow(
              column(9,
                     tabBox(width = 12,
                            # title = "Elementos Modelo",
                            tabPanel(
                              icon = icon("circle-info"),
                              solid = TRUE,
                              title = "Detalles modelo",
                              collapsible = TRUE,
                              verbatimTextOutput("modelo_gam"),
                              verbatimTextOutput("modelo_lambda")),
                            tabPanel(
                              icon = icon("chart-line"),
                              title = "Gráfico efectos",
                              fluidRow(
                                column(8,
                                       plotOutput("plot_gam") |>
                                         withSpinner(8)
                                )
                              )
                            )
                     )
              )
            )
          ),
          ## . proyección tab ----
          tabPanel(title = "Proyección",
                   fluidRow(
                     column(width = 10,plotOutput("serieproy"))
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

  ## observers ----

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


  df_x_inv <- reactive({
    inversiones <- dfinversionescmdetail()
    x_inv <- inversiones |>
    group_by(anyo) |>
    mutate(total_anyo = sum(inversion),
          porc_inv = round(100*(inversion/total_anyo), 2)) |>
    ungroup() |>
    pivot_wider(id_cols = anyo, names_from = "grupo",
                            values_from = "porc_inv") |>
    rename(spiyear = anyo,
          inv_inf = INFRAESTRUCTURAS,
          inv_tur = TURISMO,
          inv_san = SANIDAD) |>
    select(-RESTO) |>
    as.matrix()
  })


  df_spi_ts <- reactive({
    x_inv <- df_x_inv()
    spi_madrid <- x_inv |>
      bind_cols(predict(mod_53_reg(), x_inv)) |>
      rename(spi = s0)
    spi_ts <- spi_madrid |> as_tsibble(index = spiyear)
  })

  df_proy <- reactive({
    prediction <- mod_53_arima() |> forecast(h = 3)
    prediction
  })

  mod_53_reg <- reactive({
    read_rds(file.path(carpetas()$carpeta_maestros, "MODELO_REG.rds"))
  })

  mod_53_arima <- reactive({
    spi_ts <- df_spi_ts()
    modelo_ts <- spi_ts |>
      model(arima = ARIMA(spi))
  })

  ## . outputs ----
  output$tescenario <- renderDataTable({
    datatable(df_x_inv(),
              options = list(scrollX = TRUE))
  })

  ## . modelo gam ----
  output$modelo_gam <- renderPrint({
    as.matrix(coef(mod_53_arima()))
  })

  output$plot_gam <- renderPlot({
    plot(mod_53_arima())
  })

  ## Tabla de proyeccion
  output$tablaproy <- renderDataTable({
    datatable(df_proy())
  })

  ## .. Serie proyección ----
  output$serieproy <- renderPlot({
    df_proy() |> autoplot(df_spi_ts())
  })

}

shinyApp(ui, server)
