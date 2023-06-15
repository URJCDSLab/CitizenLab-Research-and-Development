########
# APP PASO 4 (PROYECCIÓN)
########

# List of libraries to be checked and installed if necessary
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
library(stringr)


## Server
library(readr)
library(mgcv)
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)
library(purrr)
library(tibble)

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),
    
    titlePanel(title = "Proyección - CitizenLab CU 25"),
    
    fluidRow(
      column(2,
             uiOutput("uinanyos"),             
             uiOutput("uimuni")
      ),
      column(10,
          leafletOutput("plot_data") |>
            withSpinner(4),
          DT::dataTableOutput("table_data") |>
            withSpinner(7)
      )
    )
  )
}


server <- function(input, output, session) {
  ## . carpetas ----
  carpetas <- reactive({
    message("AQUI")
    carpeta_entrada <- getQueryString()$carpeta_entrada
    carpeta_salida <- getQueryString()$carpeta_salida
    if(any(is.null(carpeta_entrada), 
           is.null(carpeta_salida))){
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
                   file.exists(carpeta_salida))){
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
  
    # Resto del código del servidor...
      warning("CARGANDO")
  
      
      return(invisible(list(carpeta_entrada = carpeta_entrada, carpeta_salida = carpeta_salida)))
    }
  })
  dfvaloraciones <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_45_05_02_valoracion_sim.csv"), 
             show_col_types = FALSE)
  })

  dfreceptor <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_45_05_03_receptor.csv"), 
             show_col_types = FALSE)
  })

  ## Read capacity data
  dfmunicipios <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_45_05_05_interno_mun.csv"), 
             show_col_types = FALSE)
  })
  
  ## Read area indicators data
  dfprovincias <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_45_05_04_interno_prov.csv"), 
             show_col_types = FALSE)
  })
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })
  dfclusters <- reactive({
    read_csv(paste0(carpetas()$carpeta_salida, "/cluster_anyos.csv"), 
             show_col_types = FALSE)
  })


  output$uinanyos <- renderUI({
      clusters <- dfclusters()
      selectInput("nanyos", "Años",
                  choices = unique(clusters$anyo), multiple = TRUE,
                  selected = unique(clusters$anyo)[1])
  })
  output$uimuni <- renderUI({
      clusters <- dfclusters()
      selectInput("muni", "Municipio",
                  choices = unique(clusters$mun_dest), multiple = TRUE,
                  selected = unique(clusters$mun_dest)[1])
  })

  output$plot_data <- renderLeaflet({
    if (!is.null(input$nanyos)) {
        clusters <- dfclusters() |> 
                     filter(anyo == input$nanyos)
        datageo <- st_read(paste0(carpetas()$carpeta_entrada, "/CU_45_05_01_municipios_geo.json"))
        clusters <- clusters |>
            full_join(datageo, by = c("mun_dest" = "name"))
        pal <- colorFactor(palette = "viridis", domain = unique(clusters$cluster))
        map <- sf::st_as_sf(clusters) |> 
            leaflet() |> 
            addTiles() |> 
            addPolygons(color = ~pal(cluster),  # fillColor depends on pais_orig
                    weight = 1,
                    smoothFactor = 0.5,
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    label = ~paste0("Cluster: ", cluster)) |> 
            addLegend("bottomright", 
                      pal = pal, 
                      values = ~cluster,  # Changed from valor to pais_orig
                      title = "Cluster",
                      labFormat = labelFormat(big.mark = " "),
                      opacity = 1
            )
        return(map)
    }
  })

  output$table_data <- DT::renderDataTable({
    if (!is.null(input$muni)) {
        clusters <- dfclusters() |> 
                     filter(anyo %in% input$nanyos) |> 
                     filter(mun_dest %in% input$muni)
        clusters <- t(clusters)
        colnames(clusters) <- clusters[2, ]
        return(clusters)
    }
  })
  

  observeEvent(input$abguardar, {


    ## Copiar resto input a output para siguientes pasos
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_03_areasgeo.json"),
              paste0(carpetas()$carpeta_salida, "/CU_25_05_03_areasgeo.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_05_01_hospitales.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_25_05_05_01_hospitales.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_06_indicadores_area.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_25_05_06_indicadores_area.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_07_01_capacidad.csv"),
          paste0(carpetas()$carpeta_salida, "/CU_25_05_07_01_capacidad.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_07_02_lista_espera.csv"),
          paste0(carpetas()$carpeta_salida, "/CU_25_05_07_02_lista_espera.csv"))
        file.copy(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
          paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))

    

    sendSweetAlert(
      session = session,
      title = "¡¡ Éxito !!",
      text = "Se han guardado los ficheros para el siguiente paso del caso.",
      type = "success"
    )
  })

}

shinyApp(ui, server)
