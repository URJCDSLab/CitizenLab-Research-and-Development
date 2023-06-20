########
# APP PASO 3 (VISUALIZACIÓN HISTÓRICO)
########


## Paquetes
library(shiny)
library(shinydashboard)

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
library(stringr)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

## FUNCIONES

fkmes <- function(x){
  if(x < 4){
    19
  }else if(x < 7){
    20
  }else if (x < 10){
    21
  }else{
    22
  }
}


fkmes_inv <- Vectorize(function(x){
  if(x == 19){
    "01"
  }else if(x == 20){
    "04"
  }else if (x == 21){
    "07"
  }else{
    "10"
  }
})


ui <- function(request) {
  fluidPage(
    useShinydashboard(),
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Visualización - CitizenLab CU 55"),
    
    ## UI sidebar ----
    sidebarLayout(
      
      sidebarPanel(width = 2,
                   uiOutput("uipicriterio"),
                   uiOutput("uipimunicipio"),
                   uiOutput("uipipais")
                   
      ) ,
      
      ## UI mainpanel ----
      mainPanel(width = 10,
                # navbarPage("Visualización", 
                tabPanel(title = "Visualización",
                         tabsetPanel(
                           id = "panelhistorico",
                           tabPanel("Mapa",
                                    # p(br(),
                                    #   actionBttn("btncargamapa",
                                    #              label = "Cargar mapa")
                                    # ),
                                    fluidRow(
                                      column(width = 6,
                                             uiOutput("uimes")),
                                      column(width = 6,
                                             infoBoxOutput("ibgastocom", width = 8)
                                      )
                                    ),
                                    fluidRow(
                                      column(width = 12,
                                             leafletOutput("mapa_gasto") |> 
                                               withSpinner(8)
                                      )
                                    )
                           ),
                           tabPanel("Serie",
                                    uiOutput("uipipaisesserie"),
                                    plotlyOutput("serie_gasto") 
                           ),
                           tabPanel("Ranking",
                                    plotlyOutput("ranking_gasto") |> 
                                      withSpinner(2, color.background = COL1)
                           ),
                           tabPanel("Datos",
                                    DT::dataTableOutput("tabla_gasto") |> 
                                      withSpinner(7)
                           )
                         )
                         # ),
                         # tabPanel(id = "panelhistorico",
                         #          title = "Variables predictoras",
                         #          tabsetPanel(
                         #            id = "panelpredictoras",
                         #            tabPanel("Mapa",
                         #                     p(br(),
                         #                       actionBttn("btncargamapapred",
                         #                                  label = "Cargar mapa"),
                         #                       textOutput("msgmapapred")
                         #                     ),
                         #                     leafletOutput("mapa_pred") |> 
                         #                       withSpinner(8)
                         #            ),
                         #            tabPanel("Serie",
                         #                     textOutput("msgseriepred"),
                         #                     plotlyOutput("serie_pred") |> 
                         #                       withSpinner(2, color.background = COL1)
                         #            ),
                         #            tabPanel("Datos",
                         #                     DT::dataTableOutput("tabla_pred") |> 
                         #                       withSpinner(7)
                         #            )
                         #          ))
                )
      )
    )
    
    
  )
}

server <- function(input, output, session) {
  
  ## Reactives ----
  
  ## . ReactiveValues ----
  
  # r <- reactiveValues(mapapredmsg = "",
  #                     seriepredmsg = "",
  #                     nzona = "")
  # 
  # 
  
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
  
  ## . data.frames ----
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })
  
  dfgasto <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/GASTO.csv"), 
             show_col_types = FALSE)
  })
  
  dfgastocom <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/GASTOCOM.csv"), 
             show_col_types = FALSE)
  })
  
  dfescenario_origen <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_ORIGEN.csv"), 
             show_col_types = FALSE)
  })
  dfescenario_destino <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_DESTINO.csv"), 
             show_col_types = FALSE)
  })
  
  # dfpredictores <- reactive({
  #   data.frame(predictor = colnames(dfhistorico()),
  #              fichero = "Histórico") |> 
  #     bind_rows(data.frame(predictor = colnames(dfindicadores()),
  #                          fichero = "Indicadores")) |> 
  #     bind_rows(data.frame(predictor = colnames(dfescucha()),
  #                          fichero = "Escucha")) |> 
  #     left_join(dfindicadoresmeta(),
  #               by = c("predictor" = "Tabla")) |> 
  #     mutate(Indicador = if_else(is.na(Indicador), 
  #                                predictor, 
  #                                Indicador)) |> 
  #     filter(!(predictor %in% c("GEOCODIGO", "DESBDT", "ano", "semana", "n_vacunas")))
  # })
  
  sfmunicipios <- reactive({
    read_sf(paste0(carpetas()$carpeta_entrada, "/ZONAS.json"))
  })
  
  vpaises <- reactive({
    dfpaises <- dfgasto() |> 
      group_by(pais_orig_cod,
               pais_orig) |> 
      summarise()
    
    vpaises <- setNames(dfpaises$pais_orig_cod, dfpaises$pais_orig)
  })
  
  gastocom <- reactive({
    req(input$simes, input$sipais)
    dfgastocom() |> 
      filter(Anyo == as.numeric(str_sub(input$simes, 1, 4)),
             cod_pais == input$sipais,
             FK_Periodo == fkmes(str_sub(input$simes, 6, 7))) |> 
      pull(Valor)
  })
  
  dfmapamun <- reactive({
    sfmunicipios() |> 
      inner_join(dfgasto() |> 
                   filter(mes == input$simes,
                          pais_orig_cod == input$sipais),
                 by = c("cmun" = "CMUN"))
  })
  
  dfmapapais <- reactive({
    
  })
  
  dfseriesmuni <- reactive({
    req(input$simunicipio, input$sipaisesserie)
    dfgasto() |> 
      filter(mun_dest == input$simunicipio,
             pais_orig_cod %in% input$sipaisesserie) |> 
      mutate(gasto_total = turistas*gasto,
             fecha = paste(str_sub(mes, 1, 4), 
                           str_sub(mes, 6, 7), 
                           "01", sep = "-"))
    
    
  })
  
  dfseriespaises <- reactive({
    req(input$sipaisesserie)
    print(as.numeric(input$sipaisesserie))
    dfserie <- dfgastocom() |> 
      filter(cod_pais %in% as.numeric(input$sipaisesserie)) |> 
      mutate(cod_pais = factor(cod_pais)) |>
      mutate(fecha = paste(Anyo, fkmes_inv(FK_Periodo), "01", sep = "-"))
  })
  
  # dfmapa <- reactive({
  #   sfzonas() |> 
  #     left_join(dfhistorico(), by = c("GEOCODIGO", "DESBDT"), 
  #               multiple = "all") |> 
  #     filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |> 
  #     group_by(GEOCODIGO, DESBDT) |> 
  #     summarise(n_vacunas = sum(n_vacunas), .groups = "drop")
  #   
  # })
  # 
  # dfmapapred <- reactive({
  #   r$mapapredmsg <- ""
  #   if (input$sipredictor %in% colnames(dfhistorico())){
  #     sfzonas() |> 
  #       left_join(dfhistorico(), by = c("GEOCODIGO", "DESBDT"), 
  #                 multiple = "all") |> 
  #       filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |> 
  #       group_by(GEOCODIGO, DESBDT) |> 
  #       summarise(across(n_vacunas:n_citas, ~sum(.x, na.rm = TRUE)), 
  #                 across(tmed:so2, ~mean(.x, na.rm = TRUE)), 
  #                 .groups = "drop")
  #     
  #   } else if(input$sipredictor %in% colnames(dfindicadores())){
  #     
  #     sfzonas() |> 
  #       left_join(dfindicadores(), by = c("GEOCODIGO", "DESBDT"), 
  #                 multiple = "all")
  #   } else if (input$sipredictor %in% colnames(dfescucha())){
  #     r$mapapredmsg <- "Los datos de escucha no están geolocalizados. Seleccione otro predictor."
  #     NA
  #   } else if (input$sipredictor == ""){
  #     r$mapapredmsg <- "Seleccione un predictor en el menú lateral para poder representarlo en el mapa"
  #     NA
  #   }
  # })
  # 
  # dfseriepred <- reactive({
  #   r$seriepredmsg <- ""
  #   PREDICTOR <- input$sipredictor
  #   if (PREDICTOR %in% colnames(dfhistorico())){
  #     if(input$sizona == ""){
  #       sdata <- dfhistorico() |> 
  #         select(-n_vacunas) |> 
  #         filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |> 
  #         group_by(ano, semana) |> 
  #         summarise("{PREDICTOR}" := mean(eval(parse(text = input$sipredictor)), na.rm = TRUE), .groups = "drop")
  #       r$nzona <- ""
  #     } else{
  #       r$nzona <- sfzonas() |> 
  #         filter(GEOCODIGO == input$sizona) |> 
  #         pull(DESBDT)
  #       sdata <- dfhistorico() |> 
  #         filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN),
  #                GEOCODIGO == input$sizona)
  #     }
  #     sdata |> 
  #       mutate(ano_semana = paste0(ano, "-", semana),
  #              fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))
  #   } else if (PREDICTOR %in% colnames(dfescucha())){
  #     dfescucha() |> 
  #       filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |> 
  #       mutate(ano_semana = paste0(ano, "-", semana),
  #              fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))
  #   } else if (PREDICTOR %in% colnames(dfindicadores())){
  #     r$seriepredmsg <- "Los datos de indicadores no están disponibles por semana"
  #     NA
  #   } else if (input$sipredictor == ""){
  #     r$seriepredmsg <- "Seleccione un predictor en el menú lateral para poder representarlo en el mapa"
  #     NA
  #   }
  # })
  # 
  v <- reactive(
    list(
      NSIM = dfvariables() |>
        filter(variable == "NSIM") |>
        pull(valor),
      TIPOESC = dfvariables() |>
        filter(variable == "TIPOESC") |>
        pull(valor) |> as.numeric()
    )
  )
  
  
  ## UI rendering ----
  
  ## sidebar ui ----
  
  output$uipicriterio <- renderUI({
    pickerInput("picriterio",
                choices = c("Origen", "Destino"),
                selected = v()$TIPOESC)
    
  })
  
  output$uipimunicipio <- renderUI({
    selectizeInput("simunicipio",
                   label = "Municipio de destino",
                   choices = sfmunicipios()$name,
                   options = list(
                     `live-search` = TRUE)
    )
  })
  
  output$uipipais <- renderUI({
    selectizeInput("sipais",
                   label = "País de origen (mapa)",
                   choices = vpaises(),
                   options = list(
                     `live-search` = TRUE)
    )
  })
  
  output$uimes <- renderUI({
    selectizeInput("simes",
                   label = "Mes",
                   choices = sort(unique(dfgasto()$mes)),
                   options = list(
                     `live-search` = TRUE)
    )
  })
  
  
  output$uipipaisesserie <- renderUI({
    selectizeInput("sipaisesserie",
                   label = "Países de origen",
                   choices = vpaises(),
                   multiple = TRUE,
                   options = list(
                     `live-search` = TRUE)
    )
  })
  
  
  
  # output$campana <- renderUI({
  #   div(h4("Campaña"),
  #       p(v()$ANO, "/", v()$ANO + 1),
  #       p("Desde semana ", v()$SEMANA_INICIO, " a la ", v()$SEMANA_FIN)
  #   )
  # }
  # )
  # 
  # 
  # output$uipipredictor <- renderUI({
  #   lista_pred <- list(`Seleccione uno (opcional)` = "", 
  #                      `Histórico` = setNames(dfpredictores() |> 
  #                                               filter(fichero == "Histórico") |> 
  #                                               pull(predictor),
  #                                             dfpredictores() |> 
  #                                               filter(fichero == "Histórico") |> 
  #                                               pull(Indicador)),
  #                      Indicadores = setNames(dfpredictores() |> 
  #                                               filter(fichero == "Indicadores") |> 
  #                                               pull(predictor),
  #                                             dfpredictores() |> 
  #                                               filter(fichero == "Indicadores") |> 
  #                                               pull(Indicador)),
  #                      Escucha = setNames(dfpredictores() |> 
  #                                           filter(fichero == "Escucha") |> 
  #                                           pull(predictor),
  #                                         dfpredictores() |> 
  #                                           filter(fichero == "Escucha") |> 
  #                                           pull(Indicador))
  #   )
  #   selectizeInput("sipredictor",
  #                  label = "Predictor",
  #                  choices = lista_pred,
  #                  selected = dfvariables() |> 
  #                    filter(variable == "PREDICTOR") |> 
  #                    pull(valor),
  #                  options = list(
  #                    `live-search` = TRUE
  #                  )
  #   )
  # })
  # 
  # ## mapa vacunación ----
  # mapa_vac_cargado <- 
  #   eventReactive(input$btncargamapa, 
  #                 {
  #                   pal <- colorNumeric(palette = "Blues", 
  #                                       domain = dfmapa()$n_vacunas)
  #                   dfmapa() |> 
  #                     leaflet() |>
  #                     addTiles() |> 
  #                     addPolygons(color = "#444444", 
  #                                 weight = 1, 
  #                                 smoothFactor = 0.5,
  #                                 fillOpacity = 1,
  #                                 fillColor = ~pal(n_vacunas),
  #                                 highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                                     bringToFront = TRUE),
  #                                 popup = ~paste0(DESBDT, " (", GEOCODIGO, ")"),
  #                                 label = ~paste0(n_vacunas, " vacunas")) |> 
  #                     addLegend("bottomright", 
  #                               pal = pal, 
  #                               values = ~n_vacunas,
  #                               title = "Número de vacunas",
  #                               labFormat = labelFormat(big.mark = " "),
  #                               opacity = 1
  #                     )
  #                 })
  # output$mapa_vac <- 
  #   
  #   renderLeaflet({
  #     mapa_vac_cargado()
  #     
  #   })
  # 
  # ## serie vacunación ----
  # output$serie_vac <- renderPlotly({
  #   if(input$sizona == ""){
  #     sdata <- dfhistorico() |> 
  #       filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |> 
  #       group_by(ano, semana) |> 
  #       summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop")
  #     NZONA <- NA
  #   } else{
  #     sdata <- dfhistorico() |> 
  #       filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN),
  #              GEOCODIGO == input$sizona)
  #     NZONA <- sfzonas() |> 
  #       filter(GEOCODIGO == input$sizona) |> 
  #       pull(DESBDT)
  #   }
  #   sdata <- sdata |> 
  #     mutate(ano_semana = paste0(ano, "-", semana),
  #            fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))
  #   
  #   p <- sdata |> 
  #     ggplot() +
  #     aes(x = fecha,
  #         y = n_vacunas) +
  #     geom_line(col = COL1) +
  #     labs(x = "Semana",
  #          y = "Total vacunas") +
  #     scale_x_date(date_breaks = "1 month",
  #                  date_minor_breaks = "1 week",
  #                  labels = function(x) month(x, label = TRUE)) +
  #     theme_bw() +
  #     theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
  #           plot.margin = unit(c(1.2, 1, 1, 1), "cm"))
  #   
  #   ggplotly(p) |> 
  #     layout(title = list(text = paste0("Histórico campaña ", v()$ANO, "/", v()$ANO + 1,
  #                                       "<br><sup>", 
  #                                       if_else(input$sizona == "", 
  #                                               "Total zonas", 
  #                                               paste0("Zona ", 
  #                                                      input$sizona,
  #                                                      " (", NZONA, ")"))),
  #                         x = 0,
  #                         pad = list(b = 90, l = 130, r = 50 )))
  # })
  # 
  # ## Tabla vacunación ----
  # output$tabla_vac <- DT::renderDataTable({
  #   dfhistorico() |> 
  #     filter((ano == v()$ANO & semana >= v()$SEMANA_INICIO) | (ano == v()$ANO + 1 & semana <= v()$SEMANA_FIN)) |> 
  #     group_by(GEOCODIGO, DESBDT) |> 
  #     summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop") |> 
  #     datatable(rownames = FALSE, colnames = c("Código zona", "Nombre zona", "Total vacunas campaña")) |> 
  #     formatRound(3, dec.mark = ",", mark = ".", digits = 0)
  # })
  # 
  # ## Pestaña predictores ----
  ## mapa gasto ----
  
  output$ibgastocom <- renderInfoBox({
    # print(gastocom())
    req(gastocom())
    infoBox(
      title = "Gasto medio trimestre",
      subtitle = "Comunidad de Madrid",
      value = gastocom(),
      icon = icon("credit-card"),
      color = "green")
  })
  
  mapa_munis <- reactive({
    req(input$sipais, input$simes)
    if(nrow(dfmapamun()) > 1){
      pal <- colorQuantile(palette = "Oranges", 
                           domain = dfmapamun()$turistas,
                           n = 4)
      
    }else{
      pal <- colorFactor(palette = "Oranges", 
                         domain = dfmapamun()$turistas)
    }
    dfmapamun() |> 
      leaflet() |> 
      addTiles() |> 
      addPolygons(label = ~name,
                  popup = ~paste0("Turistas: ", turistas, "<br/>",
                                  "Gasto total: ", turistas * gasto),
                  weight = 1, 
                  color = "#444444",
                  smoothFactor = 0.5,
                  fillOpacity = 0.8,
                  fillColor = ~pal(turistas)) |> 
      addLegend("bottomright", 
                pal = pal, 
                values = ~turistas,
                title = "Número de turistas",
                labFormat = labelFormat(big.mark = " "),
                opacity = 1
      )
  })
  
  mapa_paises <- reactive({
    # req(input$simes)
    # if(nrow(dfmapamun()) > 1){
    #   pal <- colorQuantile(palette = "Oranges", 
    #                        domain = dfmapamun()$turistas,
    #                        n = 4)
    #   
    # }else{
    #   pal <- colorFactor(palette = "Oranges", 
    #                      domain = dfmapamun()$turistas)
    # }
    # dfmapamun() |> 
    #   leaflet() |> 
    #   addTiles() |> 
    #   addPolygons(label = ~name,
    #               popup = ~paste0("Turistas: ", turistas, "<br/>",
    #                               "Gasto total: ", turistas * gasto),
    #               weight = 1, 
    #               color = "#444444",
    #               smoothFactor = 0.5,
    #               fillOpacity = 0.8,
    #               fillColor = ~pal(turistas)) |> 
    #   addLegend("bottomright", 
    #             pal = pal, 
    #             values = ~turistas,
    #             title = "Número de turistas",
    #             labFormat = labelFormat(big.mark = " "),
    #             opacity = 1
    #   )
  })
  
  output$mapa_gasto <- renderLeaflet({
    req(input$picriterio)
    if(input$picriterio == "Origen"){
      mapa_munis()
    }else{
      mapa_paises()
    }
  })
  
  
  
  
  output$serie_gasto <- renderPlotly({
    req(input$picriterio)
    
    if(input$picriterio == "Origen"){
      req(nrow(dfseriespaises()) > 0)
      p <- dfseriespaises() |> 
        ggplot(aes(fecha, Valor, col = cod_pais, group = cod_pais)) +
        geom_line() +
        labs(x = "Trimestre",
             y = "Gasto medio por turista",
             title = paste0("Gasto medio por turista en la Comunidad de Madrid"),
             col = "Origen") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
      
      
    }else{
      
      req(nrow(dfseriesmuni()) > 0)
      p <- dfseriesmuni() |> 
        ggplot(aes(fecha, gasto_total, col = pais_orig)) +
        geom_line(group = 1) +
        labs(x = "Mes",
             y = "Gasto total estimado",
             title = paste0("Gasto total estimado en el municipio de ", input$simunicipio),
             col = "Origen") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    ggplotly(p)
  })
  
  
  
  # output$msgmapapred <- renderText(r$mapapredmsg)
  # output$msgseriepred <- renderText(r$seriepredmsg)
  # mapa_pred_cargado <- 
  #   eventReactive(input$btncargamapapred, 
  #                 {
  #                   if(!is.data.frame(dfmapapred())){
  #                     NULL
  #                   } else{
  #                     pal <- colorNumeric(palette = "Greens", 
  #                                         domain = dfmapapred() |> 
  #                                           pull(input$sipredictor))
  #                     dfmapapred() |> 
  #                       leaflet() |>
  #                       addTiles() |> 
  #                       addPolygons(color = "#444444", 
  #                                   weight = 1, 
  #                                   smoothFactor = 0.5,
  #                                   fillOpacity = 1,
  #                                   fillColor = ~pal(eval(parse(text = input$sipredictor))),
  #                                   highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                                       bringToFront = TRUE),
  #                                   popup = ~paste0(DESBDT, " (", GEOCODIGO, ")"),
  #                                   label = ~paste0(round(eval(parse(text = input$sipredictor)), 2))) |> 
  #                       addLegend("bottomright", 
  #                                 pal = pal, 
  #                                 values = ~eval(parse(text = input$sipredictor)),
  #                                 title = paste0("Predictor: ", input$sipredictor),
  #                                 labFormat = labelFormat(big.mark = " "),
  #                                 opacity = 1
  #                       )
  #                   }
  #                 })
  # output$mapa_pred <- 
  #   renderLeaflet({
  #     mapa_pred_cargado()
  #     
  #   })
  # 
  # ## serie predictores ----
  # output$serie_pred <- renderPlotly({
  #   if(!is.data.frame(dfseriepred())){
  #     NULL
  #   } else{
  #     p <- dfseriepred() |> 
  #       ggplot() +
  #       aes(x = fecha,
  #           y = eval(parse(text = input$sipredictor))) +
  #       geom_line(col = COL1) +
  #       labs(x = "Semana",
  #            y = paste("Media de ", input$sipredictor)) +
  #       scale_x_date(date_breaks = "1 month",
  #                    date_minor_breaks = "1 week",
  #                    labels = function(x) month(x, label = TRUE)) +
  #       theme_bw() +
  #       theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
  #             plot.margin = unit(c(1.2, 1, 1, 1), "cm"))
  #     
  #     ggplotly(p) |> 
  #       layout(title = list(text = paste0("Histórico campaña ", v()$ANO, "/", v()$ANO + 1,
  #                                         "<br><sup>", 
  #                                         if_else(input$sizona == "", 
  #                                                 "Media zonas", 
  #                                                 paste0("Zona ", 
  #                                                        input$sizona,
  #                                                        " (", r$nzona, ")"))),
  #                           x = 0,
  #                           pad = list(b = 90, l = 130, r = 50 )))
  #   }
  # })
  # 
  # ## Tabla predictores ----
  # output$tabla_pred <- DT::renderDataTable({
  #   if (input$sipredictor %in% colnames(dfhistorico())){
  #     dfhistorico() |> 
  #       select(GEOCODIGO, DESBDT, ano, semana, matches(input$sipredictor)) |>
  #       datatable(rownames = FALSE, 
  #                 colnames = c("Zona", "Nombre zona", "Año", "Semana", 
  #                              input$sipredictor)) |> 
  #       formatRound(5, dec.mark = ",", mark = ".", digits = 2) |> 
  #       formatRound(3, digits = 1)
  #     
  #   } else if (input$sipredictor %in% colnames(dfescucha())){
  #     dfescucha() |> 
  #       select(ano, semana, matches(input$sipredictor)) |> 
  #       datatable(rownames = FALSE, 
  #                 colnames = c("Año", "Semana", 
  #                              input$sipredictor))
  #   } else if(input$sipredictor %in% colnames(dfindicadores())){
  #     dfindicadores() |> 
  #       select(GEOCODIGO, DESBDT, matches(input$sipredictor)) |> 
  #       datatable(rownames = FALSE, 
  #                 colnames = c("Zona", "Nombre zona", 
  #                              input$sipredictor)) |> 
  #       formatRound(3, dec.mark = ",", mark = ".", digits = 2) 
  #   }
  # })
}

shinyApp(ui, server)

