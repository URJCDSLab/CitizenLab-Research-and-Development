## UI
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(leaflet)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(janitor)
library(cluster)
library(nnet)
library(DT)
library(sjmisc)
library(purrr)
library(tibble)
library(stringr)

## Funciones -----
ui <- function(request) {

  fluidPage(

    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Simulación - CitizenLab CU 55"),

    # ... Otros elementos de la UI

    tabsetPanel(
      tabPanel("Escenarios",
               fluidRow(

                 column(width = 6,
                        h4("Datos del escenario"),
                        dataTableOutput("tescenario")),
                 column(width = 6,
                        h4("Parámetros para simulación"),
                        verbatimTextOutput("text_params")))
      ),
      tabPanel("Gráficas (datos simulación)",
               fluidRow(

                 column(width = 4, uiOutput("sel_chart")),
                 column(width = 8, plotOutput("charts"))
               )
      ),
      tabPanel("Resumen numérico (datos simulación)",
              p("Simulación turistas"),
              br(),
              column(width = 12, dataTableOutput("res_desc_tu")),

              p("Mes"),
              br(),
              column(width = 12, dataTableOutput("res_desc_mes")),

              p("Municipio destino"),
              br(),
              column(width = 12, dataTableOutput("res_desc_dest")),

              p("País origen"),
              br(),
              column(width = 12, dataTableOutput("res_desc_orig"))

              ),
      tabPanel("Resultados simulación",
               column(width = 12, dataTableOutput("sim_desc")))
    )
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

  # Dataframes
  dfvariables <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "VARIABLES.csv"), show_col_types = FALSE)
  })

  dfescenario_origen <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "ESCENARIO_ORIGEN.csv"), show_col_types = FALSE)
  })

  dfescenario_destino <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "ESCENARIO_DESTINO.csv"), show_col_types = FALSE)
  })

  dfescenario <- reactive({
    if(tipo_esc() == "Destino") {
        dfescenario_destino()
    } else {
        dfescenario_origen()
    }
  })

  dfgasto <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "GASTO.csv"), show_col_types = FALSE)
  })

  dfgasto_com <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "GASTOCOM.csv"), show_col_types = FALSE)
  })

  sfzonas <- reactive({
    read_sf(file.path(carpetas()$carpeta_entrada, "ZONAS.json"))
  })

  dfdm <- reactive({
    dm <- dfgasto() |>
    mutate(nmes = factor(str_sub(mes, 6, 7)),
            pais_orig = factor(pais_orig)) |>
    select(nmes, pais_orig, turistas, gasto) |>
    filter(str_detect(pais_orig, "Total", negate = TRUE))
  })

  n_sim <- reactive({
    as.numeric(dfvariables() |> filter(variable == "NSIM") |> pull(valor))
  })

  tipo_esc <- reactive({
    dfvariables() |> filter(variable == "TIPOESC") |> pull(valor)
  })

  parametros <- reactive({
    list(
        tipo_escenarios = tipo_esc(),
        numero_simulaciones = n_sim()
    )
  })


  simulation <- reactive({

    simulacion <- dfescenario() |>
    mutate(sim_turistas = list(rpois(n_sim(), turistas))) |>
    unnest(sim_turistas) |>
    select(-turistas)
    simulacion
  })

  simulation_pred <- reactive({
    modelo <- mod_55()
    dm <- dfdm()
    sim_data <- simulation()
    sim_data.x <- sim_data |>
        mutate(nmes = factor(str_sub(mes, 6, 7), levels = levels(dm$nmes)),
                pais_orig = factor(pais_orig, levels = levels(dm$pais_orig))) |>
        select(nmes, pais_orig, turistas = sim_turistas) |>
        model.matrix(~., data = _)

    prediccion_turistas <- predict(modelo, sim_data.x)
    cbind(sim_data, prediccion_turistas)
  })

  mod_55 <- reactive({
    read_rds(paste0(carpetas()$carpeta_maestros, "/modelo_xgb.rds"))
  })

  ## render outputs ----

  ## dynamic UI ----
  output$sel_chart <- renderUI({
    selectizeInput(
      inputId = "piselchart",
      label = "Seleccione variable a visualizar",
      selected = colnames(simulation())[1],
      choices =colnames(simulation())
    )
  })

  output$charts <- renderPlot({
    req(input$piselchart)
    if (input$piselchart != "sim_turistas") {
      ggplot(simulation(), aes(x = simulation()[[input$piselchart]])) +
        geom_bar(fill = "steelblue") +
        labs(title = paste("Gráfico de barras - ", input$piselchart), x = simulation()[[input$piselchart]], y = "Número")
    } else {
      ggplot(simulation(), aes_string(x = input$piselchart)) +
        geom_histogram(fill = "steelblue", color = "white") +
        labs(title = paste("Histograma de", input$piselchart), x = input$piselchart, y = "Frecuencia")
    }
  })

  output$res_desc_tu <- renderDataTable({
    datatable(descr(simulation()))
  })

  output$res_desc_mes <- renderDataTable({
    datatable(
      simulation() |> count(mes)
    )
  })

  output$res_desc_orig <- renderDataTable({
    datatable(
      simulation() |> count(pais_orig)
    )
  })

  output$res_desc_dest <- renderDataTable({
    datatable(
      simulation() |> count(mun_dest)
    )
  })

  grouping_variable <- reactive({
    if(tipo_esc() == "Destino") {
      "pais_orig"
    } else {
      "mun_dest"
    }
  })

  output$sim_desc <- renderDataTable({
    df <- descr(
        simulation_pred() |>
        group_by(simulation_pred()[[grouping_variable()]]) |> select (prediccion_turistas))

    list_with_group_column <- lapply(seq_along(df), function(i) {
      data_frame <- df[[i]]
      group_name <- attr(data_frame, "group")
      data_frame$group <- group_name
      return(data_frame)
    })

    # Combine all data frames into a single data frame
    combined_data_frame <- do.call(rbind, list_with_group_column)

    datatable(
        combined_data_frame |> select(-c(var, label)) |> rename(grupo=group) |> select(grupo, everything())
    , options=list(scrollX=TRUE))
  })

  output$tescenario <- DT::renderDataTable({
    dfescenario() |>
      datatable(options = list(scrollX = TRUE))
  })

  output$text_params <- renderPrint({
    parametros()
  })
}

shinyApp(ui, server)