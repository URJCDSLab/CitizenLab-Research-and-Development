########
# APP PASO 3 (VISUALIZACIÓN HISTÓRICO)
########


## Paquetes
library(shiny)

## UI
library(shinyWidgets)
library(shinycssloaders)
library(bslib, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
# library(leafem)
library(leaflet)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Visualización - CitizenLab CU 53"),

    ## UI sidebar ----
    sidebarLayout(

      sidebarPanel(width = 2,
                   htmlOutput("uiselector") |> withSpinner(1),
                   uiOutput("uianyo"),
                   uiOutput("uipaises"),
                   uiOutput("uiindicador")

      ) ,

      ## UI mainpanel ----
      mainPanel(width = 10,
                navbarPage("Visualización",
                           tabPanel(title = "SPI mapa",
                                    tabsetPanel(
                                      id = "panelhistorico",
                                      tabPanel("Mapa",
                                               p(br(),
                                                 actionBttn("btncargamapa",
                                                            label = "Cargar mapa")
                                               ),
                                               leafletOutput("mapa_vac") |>
                                                 withSpinner(8)
                                      ),
                                      tabPanel("Serie",
                                               plotlyOutput("serie_vac") |>
                                                 withSpinner(2, color.background = COL1)
                                      ),
                                      tabPanel("Serie (CM)",
                                               plotlyOutput("serie_cm") |>
                                                 withSpinner(7)
                                      )
                                    )
                           )
                )
      )
    )


  )
}

server <- function(input, output, session) {

  ## Reactives ----

  ## . ReactiveValues ----

  r <- reactiveValues(mapapredmsg = "",
                      seriepredmsg = "",
                      nzona = "")



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
    } else {
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

  ## . data.frames ----
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

  dfspifiltered <- reactive({
    req(input$sianyo)
    spi <- dfspi()

    if (!(is.null(input$sipais) || length(input$sipais))) {
      spi <- subset(spi, country %in% input$sipais)
    }

    spi |> filter(spiyear == input$sianyo)
  })

  dfmapa <- reactive({
    req(input$sianyo)
    sfpaises() |>
      left_join(dfspi(), by = c("name_long"="country"), multiple = "all")
  })

  v <- reactive(
    list(
      MODELO = dfvariables() |>
        filter(variable == "MODELO") |>
        pull(valor),
      ANYOOPT = dfvariables() |>
        filter(variable == "ANYOOPT") |>
        pull(valor) |> as.numeric(),
      RESTINVTOT = dfvariables() |>
        filter(variable == "RESTINVTOT") |>
        pull(valor) |> as.numeric(),
      RESTINVINF = dfvariables() |>
        filter(variable == "RESTINVINF") |>
        pull(valor) |> as.numeric(),
      RESTINVTUR = dfvariables() |>
        filter(variable == "RESTINVTUR") |>
        pull(valor) |> as.numeric(),
      RESTINVTUR = dfvariables() |>
        filter(variable == "RESTINVSAN") |>
        pull(valor) |> as.numeric()
    )
  )


  ## UI rendering ----

  ## sidebar ui ----

  output$uiselector <- renderUI({
    div(h4("Selectores"))
  }
  )

  output$uianyo <- renderUI({
     selectInput(
      inputId = "sianyo",
      label = "Año",
      choices = seq(min(dfspi()$spiyear), max(dfspi()$spiyear)),
      selected = min(dfspi()$spiyear)
    )
  })

  output$uipaises <- renderUI({
    selectizeInput(
        inputId = "sipais",
        label = "Selecciona los paises (solo series)",
        choices = unique(dfspi()$country),
        selected = NULL,
        multiple = TRUE
      )
  })

  output$uiindicador <- renderUI({
    excluded_columns <- c("rank_score_spi", "spicountrycode", "status", "spiyear", "country")
    available_columns <- setdiff(colnames(dfspi()), excluded_columns)
    selectizeInput(
        inputId = "siindicador",
        label = "Selecciona el indicador (solo series)",
        choices = unique(available_columns),
        selected = NULL,
        multiple = FALSE
      )
  })

  ## mapa vacunación ----
  mapa_spi_cargado <-
    eventReactive(input$btncargamapa,
                  {
                    pal <- colorNumeric(palette = "Blues",
                                        domain = dfmapa()$rank_score_spi)
                    dfmapa() |>
                      leaflet() |>
                      addTiles() |>
                      addPolygons(color = "black",
                                  weight = 1,
                                  smoothFactor = 0.5,
                                  fillOpacity = 1,
                                  fillColor = ~pal(rank_score_spi),
                                  popup = ~paste0(
                                    "Basic human needs: ", score_bhn, "<br>",
                                    "Foundations of wellbeing: ", score_fow, "<br>",
                                    "Opportunity: ", score_opp
                                  ),
                                  label = ~name_long) |>
                      addLegend("bottomright",
                                pal = pal,
                                values = ~rank_score_spi,
                                title = "SPI score",
                                labFormat = labelFormat(big.mark = " "),
                                opacity = 1
                      )
                  })
  output$mapa_vac <-

    renderLeaflet({
      mapa_spi_cargado()

    })

  ## serie vacunación ----
  output$serie_vac <- renderPlotly({
    req(input$siindicador)

    dfplot <- dfspi() %>% filter(country %in% input$sipais)
    print(input$siindicador)
    p <- plot_ly() |>
     add_trace(data = dfplot, x = ~spiyear, y = ~dfplot[[input$siindicador]],
                  color = ~country, name = ~country, type = "scatter", mode = "lines") |>
      layout(xaxis = list(title = "Year"),
            yaxis = list(title = input$siindicador),
            showlegend = TRUE)
  })

  ## Tabla vacunación ----
  output$serie_cm <- renderPlotly({
    dfplot <- dfinversionescmdetail()
    p <- plot_ly() |>
      add_trace(data = dfplot, x = ~anyo, y=~inversion, color=~grupo, type = "scatter", mode = "lines") |>
        layout(xaxis = list(title = "Año"),
        showlegend = TRUE)
  })
}

shinyApp(ui, server)

