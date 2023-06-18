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
                   uiOutput("uipaises")

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
                                      tabPanel("Datos (CM)",
                                               DT::dataTableOutput("tabla_cm") |>
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
      #filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |>
      #group_by(GEOCODIGO, DESBDT) |>
      #summarise(n_vacunas = sum(n_vacunas), .groups = "drop")

  })

  dfmapapred <- reactive({
    r$mapapredmsg <- ""
    if (input$sipredictor %in% colnames(dfhistorico())){
      sfpaises() |>
        left_join(dfhistorico(), by = c("GEOCODIGO", "DESBDT"),
                  multiple = "all") |>
        filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |>
        group_by(GEOCODIGO, DESBDT) |>
        summarise(across(n_vacunas:n_citas, ~sum(.x, na.rm = TRUE)),
                  across(tmed:so2, ~mean(.x, na.rm = TRUE)),
                  .groups = "drop")

    } else if(input$sipredictor %in% colnames(dfindicadores())){

      sfpaises() |>
        left_join(dfindicadores(), by = c("GEOCODIGO", "DESBDT"),
                  multiple = "all")
    } else if (input$sipredictor %in% colnames(dfescucha())){
      r$mapapredmsg <- "Los datos de escucha no están geolocalizados. Seleccione otro predictor."
      NA
    } else if (input$sipredictor == ""){
      r$mapapredmsg <- "Seleccione un predictor en el menú lateral para poder representarlo en el mapa"
      NA
    }
  })

  dfseriepred <- reactive({
    r$seriepredmsg <- ""
    PREDICTOR <- input$sipredictor
    if (PREDICTOR %in% colnames(dfhistorico())){
      if(input$sizona == ""){
        sdata <- dfhistorico() |>
          select(-n_vacunas) |>
          filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |>
          group_by(ano, semana) |>
          summarise("{PREDICTOR}" := mean(eval(parse(text = input$sipredictor)), na.rm = TRUE), .groups = "drop")
        r$nzona <- ""
      } else{
        r$nzona <- sfpaises() |>
          filter(GEOCODIGO == input$sizona) |>
          pull(DESBDT)
        sdata <- dfhistorico() |>
          filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN),
                 GEOCODIGO == input$sizona)
      }
      sdata |>
        mutate(ano_semana = paste0(ano, "-", semana),
               fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))
    } else if (PREDICTOR %in% colnames(dfescucha())){
      dfescucha() |>
        filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |>
        mutate(ano_semana = paste0(ano, "-", semana),
               fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))
    } else if (PREDICTOR %in% colnames(dfindicadores())){
      r$seriepredmsg <- "Los datos de indicadores no están disponibles por semana"
      NA
    } else if (input$sipredictor == ""){
      r$seriepredmsg <- "Seleccione un predictor en el menú lateral para poder representarlo en el mapa"
      NA
    }
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
                                  #highlightOptions = highlightOptions(color = "white", weight = 2,
                                  #                                    bringToFront = TRUE),
                                  #popup = ~paste0(DESBDT, " (", GEOCODIGO, ")"),
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
    if(input$sizona == ""){
      sdata <- dfhistorico() |>
        filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |>
        group_by(ano, semana) |>
        summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop")
      NZONA <- NA
    } else{
      sdata <- dfhistorico() |>
        filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN),
               GEOCODIGO == input$sizona)
      NZONA <- sfpaises() |>
        filter(GEOCODIGO == input$sizona) |>
        pull(DESBDT)
    }
    sdata <- sdata |>
      mutate(ano_semana = paste0(ano, "-", semana),
             fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))

    p <- sdata |>
      ggplot() +
      aes(x = fecha,
          y = n_vacunas) +
      geom_line(col = COL1) +
      labs(x = "Semana",
           y = "Total vacunas") +
      scale_x_date(date_breaks = "1 month",
                   date_minor_breaks = "1 week",
                   labels = function(x) month(x, label = TRUE)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            plot.margin = unit(c(1.2, 1, 1, 1), "cm"))

    ggplotly(p) |>
      layout(title = list(text = paste0("Histórico campaña ", v()$ANO, "/", v()$ANO + 1,
                                        "<br><sup>",
                                        if_else(input$sizona == "",
                                                "Total zonas",
                                                paste0("Zona ",
                                                       input$sizona,
                                                       " (", NZONA, ")"))),
                          x = 0,
                          pad = list(b = 90, l = 130, r = 50 )))
  })

  ## Tabla vacunación ----
  output$tabla_cm <- DT::renderDataTable({
    dfinversionescm()
  })
}

shinyApp(ui, server)

