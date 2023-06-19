# APP PASO 4 (VISUALIZACIÓN DE RESULTADOS)
## Paquetes
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(bslib, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
#library(leafem)
library(leaflet)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)
library(gridExtra)
library(mclust)
# if (!require(mapSpain)) {
#     install.packages("mapSpain")
# }
# library(mapSpain)

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

# UI
ui <- function(request) {
  
  fluidPage(
    
    theme = bs_theme(bootswatch = "flatly"),
    
    useShinydashboard(),
    
    titlePanel(title = "Modelos CLUSTER - CitizenLab CU 18"),
    
    # ... Otros elementos de la UI
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Mapa",
                 
                 column(width = 12,
                        uiOutput("uiadp_fecha"),
                        leafletOutput("map"))),
        tabPanel("Gráfico",
                 column(width = 12,
                        plotOutput("pcluster", width = "80%",
                                   height = "600px"))
        ),
        tabPanel("Escenario",
                 column(width = 12,
                        DT::dataTableOutput("tescenario"))),
        tabPanel("Tabla",
                 column(width = 12,
                        DT::dataTableOutput("tcluster")))
        
      )))
  
}

# SERVER
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
  
  ## Read variable data
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })
  
  datos_distritos <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "/CU_18_05_16_distritos_variables.csv"))
  })
  
  datos_infra <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "/CU_18_05_19_01_infraestructuras.csv"))
  })
  
  datos_diario <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "/CU_18_05_20_diario_infra.csv"))
  })
  
  distritos_geojson <- reactive({
    read_sf(file.path(carpetas()$carpeta_entrada, "/CU_18_05_03_distritos_geo.json"))
  })
  
  ## MODELO
  datos_cluster_diario <- reactive({
    read_rds(file.path(carpetas()$carpeta_maestros,"/datos_cluster_diario.rds"))})
  
  datos_cluster_distritos <- reactive({
    read_rds(file.path(carpetas()$carpeta_maestros,"/datos_cluster_distritos.rds"))
  })
  
  modelo_cluster <- reactive({
    if (dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito") {
      read_rds(file.path(carpetas()$carpeta_maestros, "/modelo_cluster_distritos.rds"))
    }else{
      read_rds(file.path(carpetas()$carpeta_maestros, "/modelo_cluster_diario.rds"))
    }
  })
  
  ## ESCENARIO
  
  datos_escenario <- reactive({
    if (dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito") {
      read_csv(file.path(carpetas()$carpeta_entrada, "/ESCENARIO_CLUSTER_DIST.csv"))
    } else{
      read_csv(file.path(carpetas()$carpeta_entrada, "/ESCENARIO_CLUSTER_DIARIO.csv"))
    }
  })
  
  ## Selector de fecha para mapa
  
  output$uiadp_fecha <- renderUI({
    
    ff <- datos_diario() |> pull(fecha)
    
    airDatepickerInput("adp_fecha", label = "Fecha", value = max(ff), 
                       minDate = min(ff),
                       maxDate = max(ff))
  })
  
  
  ## Render leaflet map
  output$map <- renderLeaflet({
    req(input$adp_fecha)
    # map <- leaflet() %>%
    #   addTiles()
    
    if (dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito") {
      
      dmap <- distritos_geojson() |> # slice(1:100) |> 
        inner_join(datos_cluster_distritos() , by = c("CDIS" = "cdis",
                                                      "CMUN" = "cmun"), multiple="all")
      colors <- colorFactor("RdYlBu", unique(dmap$cluster))
      
      dmap |> leaflet() |>
        addTiles() |>
        addPolygons(color="black", 
                    fillColor = ~colors(cluster),
                    
                    fillOpacity = 0.8,
                    
                    weight = 1,
                    popup = leafpop::popupTable(dmap,
                                                c("hospitales", "centros_de_salud",
                                                  "estaciones_de_cercanias")),
                    label = ~paste("Distrito", CDIS, "Municipio", CMUN)
        ) |>
        addLegend("bottomright", values = ~cluster,
                  pal=colors,
                  # colors=~colors(unique(cluster)),
                  
                  title = "Clusters",
                  
                  opacity = 1
                  
        ) 
    }else{ 
      
      dmap <- datos_infra() |>
        inner_join(datos_cluster_diario()|>
                     filter(fecha == input$adp_fecha),
                   by = c("id_inf" = "id_inf"), multiple="all") 
      
      
      
      colors <- colorFactor("RdYlBu", unique(dmap$cluster))
      
      map <- leaflet(dmap) %>%
        addTiles() %>%
        addCircleMarkers(
          ~X, ~Y, # longitude and latitude
          color = ~colors(cluster), # Color based on "puntos" column
          radius = 5, # Adjust this based on your requirements
          stroke = FALSE, fillOpacity = 0.8,
          label = ~paste0(nombre),
          popup = ~paste0("<b>Tipo</b>: ", tipo, "<br/><b>Grupo: </b>", grupo)
        ) %>%
        addLegend("bottomright", pal = colors, values = ~cluster,
                  title = "cluster",
                  opacity = 1)
    }})
  
  
  output$pcluster <- renderPlot({
    if (dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito") {
      gdata <- datos_cluster_distritos()
    } else{
      gdata <- datos_cluster_diario()
    }
    p1 <- gdata |> ggplot(aes(x = Dim.1, y = Dim.2, col = cluster)) +
      geom_point(alpha = 0.5)
    p2 <- gdata |> ggplot(aes(x = Dim.2, y = Dim.3, col = cluster)) +
      geom_point(alpha = 0.5)
    p3 <- gdata |> ggplot(aes(x = Dim.1, y = Dim.3, col = cluster)) +
      geom_point(alpha = 0.5)
    
    p <- grid.arrange(p1, p2, p3)
    
    
  })
  output$tcluster <- DT::renderDataTable({
    if (dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito") {
      tdata <- datos_cluster_distritos()
      tcols <- 9:146
    } else{
      tdata <- datos_cluster_diario()
      tcols <- 9:15
    }
    print(tdata)
    tdata |> 
      group_by(cluster) |> 
      summarise(n = n(),
                across(tcols, ~mean(.x, na.rm = TRUE))) |> 
      datatable(options = list(scrollX = TRUE))
  })
  
  output$tescenario <- DT::renderDataTable({
      datos_escenario() |> 
        mutate(Cluster = predict(modelo_cluster(), datos_escenario())$classification,
               .before = 1) |> 
        datatable(options = list(scrollX = TRUE))
  })
}

shinyApp(ui, server)
