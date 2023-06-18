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

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)




ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),

    titlePanel(title = "Proyección SPIs (Regresion) - CitizenLab CU 53"),
    navbarPage("Modelo proyección",
          id = "panelproyeccion",
          tabPanel(
            title = "Escenario",
            dataTableOutput("tescenario")),
          tabPanel(
            title = "Modelo",
            fluidRow(h4("GLM Model")),
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
                              title = "Gráfico modelo",
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
                     column(width = 10,
                         tabsetPanel(
                           tabPanel(
                             title = "Serie",
                             icon = icon("chart-line"),
                             plotlyOutput("serieproy")  |> withSpinner(2, color.background = COL1)
                           ),
                           tabPanel(
                             title = "Datos",
                             icon = icon("table"),
                             dataTableOutput("tablaproy") |> withSpinner(7)
                           ),
                           id = "vtpproy"
                         ))
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


  dfproy <- reactive({
    best_model <- mod_53_gam()
    X <- dfinversionescm() |> makeX()
    predictions <- predict(best_model, X)
    result <- data.frame(X, Proyeccion = predictions)
    colnames(result)[colnames(result) == "s0"] <- "Proyeccion"
    result
  })

  mod_53_gam <- reactive({
    dfmodel <- dfspi() |>
      filter(spicountrycode != "WWW") |>
      filter(!is.na(score_spi)) |>
      select(spicountrycode, spiyear, score_spi) |> inner_join(dfinversiones())

    x <- dfmodel |> select(-c(spicountrycode, score_spi) ) |> as.matrix()
    y <- dfmodel |> select(score_spi)  |> as.matrix()
    cv_model <- cv.glmnet(x, y, alpha = 0)
    best_lambda <- cv_model$lambda.min
    best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
    write_rds(best_model, paste0(carpetas()$carpeta_maestros, "/MODELO_REG.rds"))
    best_model
  })

  ## . outputs ----
  output$tescenario <- renderDataTable({
    datatable(dfspi(),
              options = list(scrollX = TRUE))
  })

  ## . modelo gam ----
  output$modelo_gam <- renderPrint({
    model <- mod_53_gam()
    as.matrix(coef(model))
  })

  output$plot_gam <- renderPlot({
    plot(mod_53_gam())
  })

  output$modelo_lambda <- renderPrint({
    paste("Lambda: ", mod_53_gam()$lambda)
  })

  ## Tabla de proyeccion
  output$tablaproy <- renderDataTable({
    datatable(dfproy())
  })

  ## .. Serie proyección ----
  output$serieproy <- renderPlotly({
    preds <- dfproy()

    p <- preds |>
      ggplot() +
      aes(x = spiyear,
          y = Proyeccion) +
      geom_line(col = COL1) +
      labs(x = "spiyear",
           y = "Proyeccion") +
      theme_bw() +
      theme(plot.margin = unit(c(1.2, 1, 1, 1), "cm"))
    ggplotly(p) |>
      layout(title = list(text = "Proyección SPIs"))
  })

}

shinyApp(ui, server)
