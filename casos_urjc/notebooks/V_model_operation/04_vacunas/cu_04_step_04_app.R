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

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)




ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),
    
    titlePanel(title = "Proyección campaña - CitizenLab CU 04"),
    navbarPage("Modelo proyección", 
          id = "panelproyeccion",
          tabPanel(
            title = "Escenario",
            dataTableOutput("tescenario")),
          tabPanel(
            title = "Modelo",
            fluidRow(h4("Poisson Generalized Additive Model (GAM)")),
            fluidRow(
              column(3,
                     valueBoxOutput("modelo_r2", 12),
                     valueBoxOutput("modelo_devexp", 12)),
              column(9,
                     tabBox(width = 12,
                            # title = "Elementos Modelo",
                            tabPanel(
                              icon = icon("circle-info"),
                              solid = TRUE,
                              title = "Detalles modelo",
                              collapsible = TRUE,
                              verbatimTextOutput("modelo_gam")),
                            tabPanel(
                              icon = icon("chart-line"),
                              title = "Gráfico efectos",
                              fluidRow(
                                column(4,
                                       uiOutput("uisnivelcon"),
                                       uiOutput("uipipredictor"),
                                ),
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
                     column(width = 2,
                         fluidRow(
                           actionBttn(
                             inputId = "btnproy",
                             label = "Estimar",
                             style = "unite", 
                             color = "success"
                           )
                         ),
                         fluidRow(
                           p(br(),
                             infoBoxOutput("msgestimar")
                           ),
                           p(br(),
                             uiOutput("uibtncargarmapa")
                           )),
                         br(),
                         hr(),
                         uiOutput("uisizona")),
                     column(width = 10,
                         tabsetPanel(
                           # contentWidth = 10,
                           tabPanel(
                             title = "Mapa",
                             icon = icon("map"),
                             leafletOutput("mapaproy")  |> withSpinner(8)
                           ),
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
  
  ## Modal & conditionals ----
  
  modal_confirm <- modalDialog(
    "¿Seguro que desea continuar? Puede llevar algo de tiempo",
    title = "Estimación campaña",
    footer = tagList(
      actionButton("cancel", "Cancelar"),
      actionButton("ok", "Estimar", class = "btn btn-warning")
    )
  )
  
  ## Reactives ----
  
  ## . reactive values ----
  r <- reactiveValues(prediction = NA,
                      escenario_pred = NA)
  
  ## . Events and observers ----
  
  
  observeEvent(input$btnproy, {
    showModal(modal_confirm)
  })
  
  observeEvent(input$ok, {
    waiter_show(html = tagList(spin_pulsar(), 
                               h4("Realizando predicción GAM...")))
    modelo <- mod_04_gam()
    escenario <- dfescenario()
    steps <- nrow(escenario)
    dfpred <- predict.gam(modelo, escenario, se.fit = TRUE, type = "response")
    
    #####
    ## esto sería lo suyo pero tarda la vida
    # dfpred <- predict.gam(modelo, escenario[1,], se.fit = TRUE, type = "response")
    # withProgress(
    #   message = "Estimando número de vacunas", {
    #     for (i in 2:steps){
    #       dfpred <- bind_rows(dfpred, 
    #                           predict.gam(modelo, 
    #                                       escenario[i,], 
    #                                       se.fit = TRUE, 
    #                                       type = "response"))
    #       incProgress(1/steps)
    #     }
    #   })
    ####
    r$prediction <- dfpred
    r$escenario_pred <- dfescenario() |> bind_cols(data.frame(r$prediction) )
    write_csv(r$escenario_pred, paste0(carpetas()$carpeta_salida, "/PROYECCION.csv"))
    
    #### COPIAR RESTO DE ARCHIVOS A OUTPUT
    file.copy(dir(carpetas()$carpeta_entrada, full.names = TRUE), 
              carpetas()$carpeta_salida,
              overwrite = TRUE)
    
    waiter_hide()
    showNotification("Estimacón realizada y archivos guardados")
    
    removeModal()
  })
  observeEvent(input$cancel, {
    removeModal()
  })
  
  
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
  
  sfzonas <- reactive({
    read_sf(paste0(carpetas()$carpeta_entrada, 
                   "/ZONAS.json"), 
            quiet = TRUE)
  })
  
  dfescenario <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, 
                    "/ESCENARIO.csv"), 
             show_col_types = FALSE)
  })
  
  
  mod_04_gam <- reactive({
    read_rds(paste0(carpetas()$carpeta_maestros, 
                    "/mod_04_gam.rds"))
  })
  
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })
  
  ## . UI dinamico ----
  
  output$uisizona <- renderUI({
    selectizeInput("sizona",
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
  
  
  output$uibtncargarmapa <- renderUI({
    req(r$escenario_pred)
    actionBttn("btncargamapa",
               "Cargar mapa")
  })
  
  
  ## . outputs ----
  
  output$tescenario <- renderDataTable({
    datatable(dfescenario(),
              options = list(scrollX = TRUE)) 
  })
  
  ## . modelo gam ----
  output$modelo_gam <- renderPrint({
    summary(mod_04_gam())
  })
  output$modelo_r2 <- renderValueBox({
    valueBox(round(summary(mod_04_gam())$r.sq, 2),
             HTML("Adjusted R<sup>2</sup>"),
             color = "green",
             icon = icon("star"))
  })
  output$modelo_devexp <- renderValueBox({
    valueBox(round(summary(mod_04_gam())$dev.expl, 2),
             HTML("Deviance explained"),
             color = "yellow",
             icon = icon("star"))
  })
  
  output$uisnivelcon <- renderUI(
    sliderTextInput("snivelcon",
                    "Nivel de confianza (%)",
                    choices = seq(from = 80,
                                  to = 99,
                                  by = 1),
                    selected = dfvariables() |> 
                      filter(variable == "NIVEL_CONFIANZA") |> 
                      pull(valor),
                    grid = TRUE)
  )
  output$uipipredictor <- renderUI({
    pred.names <- attr(mod_04_gam()$terms, "term.labels")[-1]
    named_pred <-  setNames(seq_along(pred.names), pred.names)
    lista_pred <- c(`Seleccione uno (opcional)` = "", 
                    named_pred
    )
    selectizeInput("sipredictor",
                   label = "Predictor",
                   choices = lista_pred,
                   selected = dfvariables() |> 
                     filter(variable == "PREDICTOR") |> 
                     pull(valor),
                   options = list(
                     `live-search` = TRUE
                   )
    )
  })
  
  output$plot_gam <- renderPlot({
    req(input$snivelcon)
    req(input$sipredictor != "")
    p <- draw(mod_04_gam(), 
              select = as.numeric(input$sipredictor), 
              # parametric = TRUE,
              # terms = "GEOCODIGO",
              # ncol = 1
              # ,
              ci_level = as.numeric(input$snivelcon)/100,
              smooth_col = COL1
              # ,
              # fun = exp
              
    )
    p + theme_bw()
    # ggplotly(p)
  })
  
  ## . Proyección campaña ----
  output$msgestimar <- renderInfoBox({
    req(r$escenario_pred)
    infoBox("Estimación realizada",
            value = paste0(nrow(r$escenario_pred), " filas"),
            color = "green",
            icon = icon("check"))
  })
  
  ## .. Mapa proyección ----
  
  mapaproy_cargado <- eventReactive(input$btncargamapa,{
    req(r$escenario_pred)
    ldata <- sfzonas() |> 
      left_join(r$escenario_pred, by = c("GEOCODIGO"), 
                multiple = "all") |> 
      group_by(GEOCODIGO, DESBDT) |> 
      summarise(n_vacunas = sum(fit, na.rm = TRUE), .groups = "drop")
    
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
                title = "Número de vacunas<br/>Predicción escenario",
                labFormat = labelFormat(big.mark = " "),
                opacity = 1
      )
  })
  
  output$mapaproy <- renderLeaflet({
    mapaproy_cargado()
  })
  
  ## .. Serie proyección ----
  
  output$serieproy <- renderPlotly({
    req(r$escenario_pred)
    if(input$sizona == ""){
      sdata <- r$escenario_pred |> 
        group_by(scampana) |> 
        summarise(fit = sum(fit, na.rm = TRUE), .groups = "drop")
      NZONA <- NA
    } else{
      sdata <- r$escenario_pred |> 
        filter(GEOCODIGO == input$sizona)
      NZONA <- sfzonas() |> 
        filter(GEOCODIGO == input$sizona) |> 
        pull(DESBDT)
    }
    p <- sdata |> 
      ggplot() +
      aes(x = scampana,
          y = fit) +
      geom_line(col = COL1) +
      labs(x = "Semana",
           y = paste("Total vacunas")) +
      # scale_x_date(date_breaks = "1 month",
      #              date_minor_breaks = "1 week",
      #              labels = function(x) month(x, label = TRUE)) +
      theme_bw() +
      theme(#axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.margin = unit(c(1.2, 1, 1, 1), "cm"))
    ggplotly(p) |>
      layout(title = list(text = paste0("Proyección próxima campaña ",
                                        "<br><sup>",
                                        if_else(input$sizona == "",
                                                "Total zonas",
                                                paste0("Zona ",
                                                       input$sizona,
                                                       " (", NZONA, ")"))),
                          x = 0,
                          pad = list(b = 90, l = 130, r = 50 )))
    
    
  })
  
  ## .. Tabla proyección ----
  
  output$tablaproy <- renderDataTable({
    req(r$escenario_pred)
    r$escenario_pred |> right_join(tibble(sfzonas()) |> select(2:3), by = "GEOCODIGO") |> 
      group_by(GEOCODIGO, DESBDT) |> 
      summarise(fit = sum(fit, na.rm = TRUE), .groups = "drop") |> 
      datatable(rownames = FALSE, colnames = c("Código zona", "", "Total predicción vacunas escenario")) |> 
      formatRound(3, dec.mark = ",", mark = ".", digits = 0)
  })
  
  
  
  
}

shinyApp(ui, server)
