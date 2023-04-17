########
# APP PASO 6 (PREDICCIÓN CAMPAÑA ACTUAL)
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
# library(gratia)
library(leaflet)
# library(waiter)


## Server
library(readr)
# library(mgcv)
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(lubridate, warn.conflicts = FALSE)
library(tsibble, warn.conflicts = FALSE)
library(fable, warn.conflicts = FALSE)
# library(tidyr)

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)




ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    # useWaiter(),
    titlePanel(title = "Predicción campaña actual - CitizenLab CU 04"),
    navbarPage(
      "Modelo predicción",
      ## . predicción tab ----
      tabPanel("Escenario",
               dataTableOutput("tescenariots")),
      tabPanel("Modelo",
               fluidRow(h4("Time Series ARIMA model")),
               fluidRow(column(width = 6, 
                            title = "Modelos estimados",
                            dataTableOutput("tabla_modelos_ts")),
                        column(title = "Modelo zona",
                            width = 6,
                            verbatimTextOutput("modelo_ts")))),
      tabPanel("Predicción",
               fluidRow(
                 box(width = 2,
                     infoBoxOutput("ibsemanasfin") |> 
                       withSpinner(),
                     br(),
                     actionBttn("btncargamapa",
                                "Cargar mapa"),
                     br(),
                     br(),
                     uiOutput("uisizonats")
                 ),
                 box(width = 10,
                     tabsetPanel(
                       tabPanel(
                         title = "Mapa",
                         icon = icon("map"),
                         leafletOutput("mapapred") |> withSpinner(8)
                       ),
                       tabPanel(
                         title = "Serie",
                         icon = icon("chart-line"),
                         plotlyOutput("seriepred") #|> withSpinner(2, color.background = COL1)
                       ),
                       tabPanel(
                         title = "Datos",
                         icon = icon("table"),
                         dataTableOutput("tablapred") #|> withSpinner(7)
                       )
                     )
                 )
               ))
    )
    
  )
} 

server <- function(input, output, session) {
  
  ## Modal & conditionals ----
  
  
  ## Reactives ----
  
  ## . reactive values ----

  
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
  
  ## . Data frames and models ----
  
  mod_04_ts <- reactive({
    read_rds(paste0(carpetas()$carpeta_entrada, "/mod_04_ts.rds"))
  })
  
  sfzonas <- reactive({
    read_sf(paste0(carpetas()$carpeta_entrada, 
                   "/ZONAS.json"), 
            quiet = TRUE)
  })
  
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })
  
  dfnuevacampana <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/NUEVACAMPANA.csv"), 
             show_col_types = FALSE)
  })
  
  dfprediccion <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/PREDICCION_TS.csv"), 
             show_col_types = FALSE)
  })

  # actualts <- reactive({
  #   dfnuevacampana() |>
  #     mutate(tsweek = make_yearweek(ano, semana)) |>
  #     as_tsibble(key = GEOCODIGO, index = tsweek) |>
  #     fill_gaps()
  # })
  

  ## . UI dinamico ----
  
  output$uisizonats <- renderUI({
    selectizeInput("sizonats",
                   label = "Zona a visualizar en serie",
                   choices = c(`Todas Agregadas` = "", 
                               setNames(sfzonas()$GEOCODIGO,
                                        sfzonas()$DESBDT)),
                   selected = dfvariables() |> 
                     filter(variable == "ZONA") |> 
                     pull(valor),
                   options = list(
                     `live-search` = TRUE)
    )
  })
  

  
  # output$uibtnpredecirts <- renderUI({
  #   req(r$mod_04_ts)
  #   actionBttn("btnpredecirts",
  #              "Predecir")
  # })
  
  
  
  ## . outputs ----
  

  

  
  ## . Predicción campaña ----
  
  # observeEvent(input$tescenariots_rows_selected,
  #              {message(input$tescenariots_rows_selected)})
  
  
  ## .. Escenario predicción ----
  
  output$tescenariots <- renderDataTable({
    dfnuevacampana() |> 
      datatable(selection = "single") 
  },
  server = FALSE)
  
  ## .. Modelo predicción ----
  
  output$tabla_modelos_ts <- renderDataTable({
    # req(r$mod_04_ts)
    mod_04_ts() |> 
      mutate(modelo = as.character(arima)) |> 
      data.frame() |> 
      select(-arima) |> 
      left_join(sfzonas() |> st_drop_geometry() |> 
                  select(GEOCODIGO, DESBDT),
                by = "GEOCODIGO") |> 
      relocate(DESBDT, .after = GEOCODIGO) |> 
      datatable(selection = "single")
  })
  
  
  output$modelo_ts <- renderPrint({
    # req(r$mod_04_ts)
    req(input$tabla_modelos_ts_rows_selected)
    # summary(r$mod_04_ts)
    mod_04_ts() %>%
      slice(input$tabla_modelos_ts_rows_selected) %>%
      select(arima) %>%
      report()
  })
  
  output$ibsemanasfin <- renderInfoBox({
    infoBox(
      title = "Semanas",
      subtitle = "Semanas restantes",
      value = dfprediccion() |> 
        filter(dato == "Predicción") |> 
        pull(semana) |> 
        unique() |> 
        length(),
      icon = icon("calendar"))
  })
  
  ## .. Mapa predicción ----
  
 
  
  
  mapapred_cargado <- eventReactive(input$btncargamapa,{
    ldata <- sfzonas() |> 
      left_join(dfprediccion(), by = c("GEOCODIGO"), 
                multiple = "all") |> 
      filter(dato == "Predicción") |> 
      group_by(GEOCODIGO, DESBDT) |> 
      summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop")
    
    pal <- colorNumeric(palette = "Oranges", 
                        domain = ldata$n_vacunas)
    ldata |> 
      leaflet() |>
      addTiles() |> 
      addPolygons(color = "#444444", 
                  weight = 1, 
                  smoothFactor = 0.5,
                  fillOpacity = 1,
                  fillColor = ~pal(n_vacunas),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~paste0(DESBDT, " (", GEOCODIGO, ")"),
                  label = ~paste0(round(n_vacunas), " vacunas")) |> 
      addLegend("bottomright", 
                pal = pal, 
                values = ~n_vacunas,
                title = "Número de vacunas<br/>Predicción resto campaña",
                labFormat = labelFormat(big.mark = " "),
                opacity = 1
      )
  })
  
  output$mapapred <- renderLeaflet({
    mapapred_cargado()
  })
  
  ## .. serie ts pred ----
  output$seriepred <- renderPlotly({
    if(input$sizonats == ""){
      sdata <- dfprediccion() |>
        mutate(ano_semana = paste0(ano, "-", semana),
               fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w'))) |>
        group_by(fecha, dato) |>
        summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop")
      NZONA <- NA

    } else{
      sdata <- dfprediccion() |>
        filter(GEOCODIGO == input$sizonats) |>
        # select(GEOCODIGO, tsweek, .mean) |>
        mutate(ano_semana = paste0(ano, "-", semana),
               fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w'))) |>
        group_by(fecha, dato) |>
        summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop")
      NZONA <- sfzonas() |> tibble() |>
        filter(GEOCODIGO == input$sizonats) |>
        pull(DESBDT)
    }
    
    
    p <- sdata |>
      ggplot() +
      aes(x = fecha,
          y = n_vacunas) +
      geom_line(aes(col = dato)) +
      labs(x = "Semana",
           y = "Total vacunas",
           col = "Tipo de dato") +
      theme_bw() +
      theme(plot.margin = unit(c(1.2, 1, 1, 1), "cm"))

    ggplotly(p) |>
      layout(title = list(text = paste0("Predicción resto campaña ",
                                        min(year(sdata$fecha)), "/",
                                        min(year(sdata$fecha)) + 1,
                                        "<br><sup>",
                                        if_else(input$sizonats == "", "Total zonas",
                                                paste0("Zona ", input$sizonats,
                                                       " (", NZONA, ")")),
                                        "</sup>"),
                          x = 0.05))
  })  
  
  ## .. datos ts ----
  
  output$tablapred <- renderDataTable({
    dfprediccion() |> 
      right_join(tibble(sfzonas()) |> select(2:3), by = "GEOCODIGO") |> 
      group_by(GEOCODIGO, DESBDT, dato) |> 
      summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop") |> 
      datatable(rownames = FALSE, colnames = c("Código zona", "", "Tipo de dato", "Total predicción vacunas escenario")) |> 
      formatRound(4, dec.mark = ",", mark = ".", digits = 0)
  })
  
  
}

shinyApp(ui, server)
