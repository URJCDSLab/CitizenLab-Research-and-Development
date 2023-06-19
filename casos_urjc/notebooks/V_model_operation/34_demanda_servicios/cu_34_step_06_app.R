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

## Funciones -----
ui <- function(request) {

  fluidPage(

    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Simulación - CitizenLab CU 34"),

    # ... Otros elementos de la UI

    tabsetPanel(
        tabPanel("Gráficas simulación",
            column(width = 4, uiOutput("sel_chart")),
            column(width = 12, plotOutput("charts"))
        ),
        #tabPanel("Resumen numérico (frecuencia)",
        #    column(width = 12, verbatimTextOutput("res_num"))
        #),
        tabPanel("Resumen numérico (descripción)",
            column(width = 12, dataTableOutput("res_desc"))
        )
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

  dfservicios <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "SERVICIOS.csv"), show_col_types = FALSE)
  })

  dfescenarios <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "ESCENARIOS.csv"), show_col_types = FALSE)
  })

  sfsecciones <- reactive({
    read_sf(file.path(carpetas()$carpeta_entrada, "SECCIONES.json"))
  })

  n_sim <- reactive({
    as.numeric(dfvariables() |> filter(variable == "NSIM") |> pull(valor))
  })

  simulation <- reactive({
    escenario <- dfescenarios()
    pfutbol <- sum(escenario$Futbol == 1) / nrow(escenario)
    rate_nservicios <- mean(escenario$nservicios)
    rate_capacidad <- mean(escenario$capacidad)
    m_cont <- apply(escenario[,4:25], 2, mean)
    s_cont <- apply(escenario[,4:25], 2, sd)
    nsim <- n_sim()
    mat <- matrix(rep(NA_real_, nsim*ncol(escenario)), ncol = ncol(escenario))
    mat[, 1] <- rbinom(n_sim(), size = 1, prob = pfutbol)
    mat[, 2:3] <- sapply(c(rate_nservicios, rate_capacidad),
                        function(x) rpois(nsim, lambda = x))
    mat[, 4:25] <- sapply((4:25) - 3,
                        function(x) rnorm(nsim, mean = m_cont[x], sd = s_cont[x]))
    colnames(mat) <- colnames(escenario)
    simulacion <- as_tibble(mat)
    simulacion
  })

  dfpred <- reactive({
    data <- dfescenarios()
    predict(modelo_ann(), dfescenarios()) |> round(2)
  })

  dfpredclass <- reactive({
    data <- dfescenarios()
    predict(modelo_ann(), dfescenarios(), type = "class")
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
    if (input$piselchart == "Futbol") {
      ggplot(simulation(), aes(x = Futbol)) +
        geom_bar(fill = "steelblue") +
        labs(title = "Gráfico de barras - Futbol", x = "Futbol", y = "Frecuencia")
    } else {
      ggplot(simulation(), aes_string(x = input$piselchart)) +
        geom_histogram(fill = "steelblue", color = "white") +
        labs(title = paste("Histograma de", input$piselchart), x = input$piselchart, y = "Frecuencia")
    }
  })

  # Render the inputs
  #output$res_num  <- renderPrint({
  #  frq(simulation())
  #})

  output$res_desc <- renderDataTable({
    datatable(descr(simulation()))
  })
}

shinyApp(ui, server)