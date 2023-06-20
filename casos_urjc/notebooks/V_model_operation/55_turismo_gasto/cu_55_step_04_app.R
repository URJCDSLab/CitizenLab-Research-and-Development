########
# APP PASO 4 (VISUALIZACIÓN MODELO)
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
library(spData)

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(stringr)
library(tidyr)
library(xgboost)

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


fkmes_inv <- Vectorize(function(x, periodo = "M"){
  if(x == 19){
    "01"
  }else if(x == 20){
    ifelse(periodo == "M", "04", "02")
  }else if (x == 21){
    ifelse(periodo == "M", "07", "03")
  }else{
    ifelse(periodo == "M", "10", "04")
  }
})




ui <- function(request) {
  fluidPage(
    #   useShinydashboard(),
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Modelo XGBOOST - CitizenLab CU 55"),
    #   
    #   ## UI sidebar ----
    sidebarLayout(
      #     
      sidebarPanel(width = 2,
                   uiOutput("uipicriterio"),
                   p("Variable objetivo: Gasto medio diario por turista")
      ) ,
      #     ## UI mainpanel ----
      mainPanel(width = 10,
                #               # navbarPage("Visualización", 
                tabPanel(title = "",
                         tabsetPanel(
                           id = "panelmodelo",
                           tabPanel("Modelo",
                                    fluidRow(
                                      column(width = 6,
                                             verbatimTextOutput("vtomodelo")
                                      ),
                                      column(width = 6,
                                             h4("Variable Importance"),
                                             plotOutput("plotimportance", 
                                                        height = "600px")
                                      )
                                    )
                           ),
                           tabPanel("Escenario",
                                    DT::dataTableOutput("tabla_escenario")
                           ),
                           tabPanel("Mapa",
                                    leafletOutput("mapa_prediccion", 
                                                  height = "600px")
                           ),
                           tabPanel("Resultado",
                                    DT::dataTableOutput("tabla_prediccion")
                           )
                         )
                         # 
                )
      )
    )
    #   
    #   
  )
}

server <- function(input, output, session) {
  
  ## Reactives ----
  
  ## . ReactiveValues ----
  
  
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
  
  dfescenario <- reactive({
    req(input$picriterio)
    fescen <- ifelse(input$picriterio == "Origen", "/ESCENARIO_ORIGEN.csv",
                     "/ESCENARIO_DESTINO.csv")
    read_csv(paste0(carpetas()$carpeta_entrada, fescen), 
             show_col_types = FALSE)
  })
  
  modelo <- reactive({
    read_rds(paste0(carpetas()$carpeta_maestros, "/modelo_xgb.rds"))
  })
  
  dfpaises <- reactive({
    read_csv(paste0(carpetas()$carpeta_maestros, "/corresppaisos.csv"),
             skip = 3,
             show_col_types = FALSE) |>
      mutate(INE = as.numeric(INE)) |>
      drop_na(INE) |>
      select(INE, ISO2 = `ISO (alpha2)`) |> 
      left_join(dfgasto() |> 
                  group_by(pais_orig_cod,
                           pais_orig) |> 
                  summarise() |> 
                  mutate(pais_orig_cod = as.numeric(pais_orig_cod)),
                by = c("INE" = "pais_orig_cod")
      )
  })
  
  
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
  
  dfgasto <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/GASTO.csv"), 
             show_col_types = FALSE)
  })
  
  
  dm <- reactive( dfgasto() |> 
                    mutate(nmes = factor(str_sub(mes, 6, 7)),
                           pais_orig = factor(pais_orig)) |> 
                    select(nmes, pais_orig, turistas, gasto) |> 
                    filter(str_detect(pais_orig, "Total", negate = TRUE)))
  
  escenario_x <- reactive({
    escenario.x <- dfescenario() |> 
      mutate(nmes = factor(str_sub(mes, 6, 7), levels = levels(dm()$nmes)),
             pais_orig = factor(pais_orig, levels = levels(dm()$pais_orig))) |> 
      select(nmes, pais_orig, turistas) |> 
      model.matrix(~., data = _)
  })
  
  dfprediccion <- reactive({
    dfescenario() |> 
      mutate(prediccion = predict(modelo(), escenario_x()) )
  })
  
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
  
  
  ## OUTPUTS ----
  
  output$vtomodelo <- renderPrint({
    modelo()
  })
  
  output$plotimportance <- renderPlot({
    importance <- xgb.importance(feature_names = colnames(escenario_x()), 
                                 model = modelo())
    xgb.plot.importance(importance_matrix = head(importance, 20))
    
  })
  
  output$tabla_escenario <- DT::renderDataTable({
    dfescenario() |> 
      datatable(options = list(scrollX = TRUE))
  })
  output$tabla_prediccion <- DT::renderDataTable({
    dfprediccion() |> 
      arrange(desc(prediccion)) |> 
      datatable(options = list(scrollX = TRUE))
  })
  
  
  mapa_munis <- reactive({
    print(dfprediccion())
    if(nrow(dfprediccion()) > 1){
      pal <- colorQuantile(palette = "Oranges",
                           domain = dfprediccion()$prediccion,
                           n = 4)
      
    }else{
      pal <- colorFactor(palette = "Oranges",
                         domain = dfprediccion()$turistas)
    }
    sfmunicipios() |> 
      inner_join(dfprediccion(),
                 by = c("name" = "mun_dest")) |> 
      leaflet() |>
      addTiles() |>
      addPolygons(label = ~name,
                  popup = ~paste0("Predicción gasto: ", round(prediccion, 2)),
                  weight = 1,
                  color = "#444444",
                  smoothFactor = 0.5,
                  fillOpacity = 0.8,
                  fillColor = ~pal(prediccion)) |>
      addLegend("bottomright",
                pal = pal,
                values = ~prediccion,
                title = "Gasto medio",
                labFormat = labelFormat(big.mark = " "),
                opacity = 1
      )
  })
  
  mapa_paises <- reactive({
    print(dfpaises())
    print(dfprediccion())
    dmap <- world |> select(iso_a2, name_long) |>
      right_join(
        dfprediccion() |>
          # mutate(trimestre = fkmes_inv(FK_Periodo, periodo = "T"),
          #        periodo = paste(Anyo, trimestre, sep = "-")) |>
          # filter(periodo == input$sitrimestre) |>
          left_join(dfpaises(),
                    by = "pais_orig"),
        by = c("iso_a2" = "ISO2"))
    
    pal <- colorQuantile(palette = "Oranges",
                         domain = dmap$prediccion,
                         n = 4)
    dmap |>
      leaflet() |>
      addTiles() |>
      addPolygons(label = ~name_long,
                  popup = ~paste0("Predicción gasto medio: ", round(prediccion, 2)),
                  weight = 1,
                  color = "#444444",
                  fillOpacity = 0.8,
                  fillColor =  ~pal(prediccion))
    
    
  })
  
  output$mapa_prediccion <- renderLeaflet({
    req(input$picriterio)
    if(input$picriterio == "Origen"){
      mapa_munis()
    }else{
      mapa_paises()
    }
  })
  # 
  # 
  # 
  # 
  # output$serie_gasto <- renderPlotly({
  #   req(input$picriterio)
  #   
  #   if(input$picriterio == "Origen"){
  #     req(nrow(dfseriespaises()) > 0)
  #     p <- dfseriespaises() |> 
  #       ggplot(aes(fecha, Valor, col = cod_pais, group = cod_pais)) +
  #       geom_line() +
  #       labs(x = "Trimestre",
  #            y = "Gasto medio por turista",
  #            title = paste0("Gasto medio por turista en la Comunidad de Madrid"),
  #            col = "Origen") +
  #       theme_bw() +
  #       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  #     
  #     
  #   }else{
  #     
  #     req(nrow(dfseriesmuni()) > 0)
  #     p <- dfseriesmuni() |> 
  #       ggplot(aes(fecha, gasto_total, col = pais_orig)) +
  #       geom_line(group = 1) +
  #       labs(x = "Mes",
  #            y = "Gasto total estimado",
  #            title = paste0("Gasto total estimado en el municipio de ", input$simunicipio),
  #            col = "Origen") +
  #       theme_bw() +
  #       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #   }
  #   ggplotly(p)
  # })
  # 
  # output$ranking_gasto <- renderPlotly({
  #   
  #   if(input$picriterio == "Origen"){
  #     req(input$simes)
  #     dfrankingdestino <- 
  #       dfgasto ()|> 
  #       mutate(gasto_total = turistas*gasto) |> 
  #       filter(mes == input$simes,
  #              mun_dest == input$simunicipio) |> 
  #       arrange(desc(gasto_total)) 
  #     req(nrow(dfrankingdestino)> 0 )
  #     p <- dfrankingdestino |> 
  #       mutate(pais_orig = rev(forcats::fct_inorder(pais_orig))) |>  
  #       ggplot(aes(y = pais_orig, x = gasto_total)) +
  #       geom_col(fill = "orange") +
  #       labs(x = "Gasto total",
  #            y = "") +
  #       theme_bw()
  #   } else{
  #     req(input$sitrimestre)
  #     paises <- dfgasto() |> 
  #       group_by(pais_orig_cod, pais_orig) |> 
  #       summarise(.groups = "drop") |> 
  #       mutate(pais_orig_cod = as.numeric(pais_orig_cod))
  #     dfrankingcom <- 
  #       dfgastocom() |> 
  #       left_join(paises,
  #                 by = c("cod_pais" = "pais_orig_cod")) |> 
  #       mutate(trimestre = fkmes_inv(FK_Periodo, periodo = "T"),
  #              periodo = paste(Anyo, trimestre, sep = "-")) |> 
  #       filter(periodo == input$sitrimestre) |> 
  #       arrange(desc(Valor))  |> 
  #       tidyr::drop_na()
  #     req(nrow(dfrankingcom)> 0 )
  #     p <- dfrankingcom |> 
  #       mutate(pais_orig = rev(forcats::fct_inorder(pais_orig))) |>  
  #       ggplot(aes(y = pais_orig, x = Valor)) +
  #       geom_col(fill = "orange") +
  #       labs(x = "Gasto medio por turista",
  #            y = "") +
  #       theme_bw()
  #   }
  #   
  #   ggplotly(p)
  #   
  # })
  # 
  # output$tabla_gasto <- DT::renderDataTable({
  #   if(input$picriterio == "Origen"){
  #     tdf <- dfgasto()  
  #   }else{
  #     paises <- dfgasto() |> 
  #       group_by(pais_orig_cod, pais_orig) |> 
  #       summarise(.groups = "drop") |> 
  #       mutate(pais_orig_cod = as.numeric(pais_orig_cod))
  #     tdf <- dfgastocom() |> 
  #       left_join(paises,
  #                 by = c("cod_pais" = "pais_orig_cod")) |> 
  #       mutate(trimestre = fkmes_inv(FK_Periodo, periodo = "T"),
  #              periodo = paste(Anyo, trimestre, sep = "-"))
  #   }
  #   
  #   tdf |> 
  #     datatable(options = list(scrollX = TRUE)) 
  #     
  #   
  # })
  # 
  # 
  
}

shinyApp(ui, server)

