########
# APP PASO 2 (PARÁMETROS DE USUARIO) CU 18 (Comportamiento Infra. Eventos extremos)
########

## Paquetes ----


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


plotting_columns = c("Futbol", "nservicios", "capacidad", "tmed", "prec", "velmedia", "presMax", "ccaa", "CPRO", "t1_1", "t2_1", "t2_2", "t3_1", "t4_1", "t4_2", "t4_3", "t5_1", "t6_1", "t7_1", "t8_1", "t9_1", "t10_1", "t11_1", "t12_1", "NSEC", "area", "elevation", "densidad_hab_km2")

ui <- function(request) {

  fluidPage(

    theme = bs_theme(bootswatch = "flatly"),

    useShinydashboard(),

    titlePanel(title = "Visualización - CitizenLab CU 34"),




    # ... Otros elementos de la UI



    sidebarLayout(
      sidebarPanel(
        column(width = 6,
            uiOutput("uiservicio"),
            uiOutput("uicolumn"),
            uiOutput("uidate"),
            uiOutput("uiseccion"),
        )
      ),
      mainPanel(
        tabsetPanel(
            tabPanel("Tabla",
                column(width = 12, DT::dataTableOutput("table_data"))
            ),
            tabPanel("Visualización",
                column(width = 12, leafletOutput("map"))
            ),
            tabPanel("Serie",
                column(width = 12, plotlyOutput("serie"))
            )
        )
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

  filtered_df <- reactive({
    dfservicios() |>
            filter(Servicio == input$piservicio) |>
            filter(Fecha >= input$pidate[1] & Fecha <= input$pidate[2])

  })

  filtered_map_data <- reactive({
    req(input$piservicio)
    grouped_df <- filtered_df() |>
        group_by(CSEC, CDIS, CMUN) |>
        summarize(across(where(is.numeric), mean), Futbol = round(mean(Futbol)))
    sfsecciones() |>
      inner_join(grouped_df, by=c("CSEC", "CDIS", "CMUN"))

  })

  ## render outputs ----

  ## dynamic UI ----

  # Render the inputs

  output$uiservicio <- renderUI({
    selectizeInput(
      inputId = "piservicio",
      label = "Servicio",
      selected = unique(dfservicios()$Servicio)[1],
      choices = unique(dfservicios()$Servicio)
    )
  })

  output$uiseccion <- renderUI({
    selectizeInput(
      inputId = "piseccion",
      label = "Seccion  (solo series)",
      selected = unique(dfservicios()$CSEC)[1],
      choices = unique(dfservicios()$CSEC)
    )
  })

  output$uicolumn <- renderUI({
    selectizeInput(
      inputId = "picolumn",
      label = "Columna a visualizar",
      selected = plotting_columns[1],
      choices = plotting_columns
    )
  })

  output$uidate <- renderUI({
    dateRangeInput(
                inputId = "pidate",
                label = "dates", "Date range:",
                start = min(as.Date(na.omit(dfservicios()$Fecha))),
                end = max(as.Date(na.omit(dfservicios()$Fecha))))
  })

  output$table_data <- renderDataTable({
    datatable(filtered_df(),
              options = list(scrollX = TRUE))
  })

  # Render map
  output$map <- renderLeaflet({
    req(input$picolumn)

    plot_df <- filtered_map_data()
    pal <- colorNumeric(palette = "viridis", domain = plot_df[[input$picolumn]])
    plot_df |>
    leaflet() |>
      addTiles() |>
      addPolygons(fillColor = ~pal(plot_df[[input$picolumn]]),
                  color = "black",
                  weight = 1,
                  smoothFactor = 0.5,
                  fillOpacity = 1,
                  label = ~as.character(CSEC)) |>
        addLegend("bottomright",
            pal = pal,
            values = ~plot_df[[input$picolumn]],
            title = input$picolumn,
            labFormat = labelFormat(big.mark = " ")
        )
  })

  output$serie <- renderPlotly({
    req(input$piseccion)
    plot_df <- filtered_df() |>
        filter(CSEC == input$piseccion) |>
        group_by(Fecha) |>
        summarize(!!input$picolumn := mean(!!sym(input$picolumn), na.rm = TRUE))

    p <- plot_ly() |>
      add_trace(name=input$picolumn, data = plot_df, x = ~Fecha, y=plot_df[[input$picolumn]], type = "scatter", mode = "lines") |>
        layout(xaxis = list(title = "Año"),
        showlegend = TRUE)
  })
}

shinyApp(ui, server)