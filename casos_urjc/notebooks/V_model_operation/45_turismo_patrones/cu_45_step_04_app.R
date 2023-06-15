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
    
    titlePanel(title = "Proyección - CitizenLab CU 45"),
    
    fluidRow(
      column(2,
             uiOutput("uinclus"),  
             uiOutput("uinanyos"),             
             uiOutput("uimuni"),             
             uiOutput("uiccaa")
      ),
      column(10,
          leafletOutput("plot_data") |>
            withSpinner(4),
          DT::dataTableOutput("table_data") |>
            withSpinner(7),
           plotlyOutput("points") |> 
             withSpinner(2, color.background = COL1)
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
    receptor <- dfreceptor()
    interno_prov <- dfprovincias()
    interno_muni <- dfmunicipios()
    valoraciones <- dfvaloraciones()

    turistas_origen_mes <- 
      receptor |> 
      filter(pais_orig == "Total") |> 
      select(mes, CMUN, mun_dest, turistas) |> 
      mutate(receptor = turistas) |> 
      select(-turistas) |> 
      inner_join(
        interno_prov |> 
          filter(!is.na(total_ccaa),
                 is.na(provincia)) |> 
          select(mes, cmun, total_ccaa, turistas) |> 
          pivot_wider(names_from = total_ccaa, 
                      values_from = turistas,
                      values_fn = ~sum(.x, na.rm = TRUE)),
        by = c("CMUN" = "cmun", "mes" = "mes"))
    
    val <- valoraciones |> 
      group_by(CMUN, grupo) |> 
      summarise(puntos = median(puntos, na.rm = TRUE),.groups = "drop") |> 
      pivot_wider(names_from = grupo,
                  values_from = puntos) |> 
      mutate(across(2:4, ~replace_na(.x, median(.x, na.rm = TRUE))))
    
    kdata <- turistas_origen_mes |> 
      inner_join(val, by = "CMUN") |> 
      # select(-municipio_destino) |> 
      mutate(anyo = str_sub(mes, 1, 4)) |> 
      group_by(anyo, mun_dest) |> 
      summarise(across(3:(ncol(turistas_origen_mes) - 1), sum),
                across((ncol(turistas_origen_mes) - 1):(ncol(turistas_origen_mes) + 2), median))
    
    by_anyo <- split(kdata, kdata$anyo)
    
    by_anyo[[1]] |> 
      column_to_rownames("mun_dest") |> 
      select(-anyo) |> 
      kmeans(10)
    
    
    ## PARÁMETRO DEL PASO 2
    NCLUS <- 4
    
    cluster_anyos <- map(by_anyo, function(x)  {
      m <- x |> 
        column_to_rownames("mun_dest") |> 
        select(-anyo) |> 
        kmeans(NCLUS)
      res <- x |> 
        mutate(cluster = m$cluster)
    }) |> bind_rows() |> 
      ungroup()
    
    write_csv(cluster_anyos, paste0(carpetas()$carpeta_salida, "/cluster_anyos.csv"))
    return(cluster_anyos)
  })

  output$uinclus <- renderUI({
    NCLUS <- dfvariables() |> 
                     filter(variable == "NCLUS") |> 
                     pull(valor)
    numericInput(
      inputId = "nclus",
      label = "Número de Clusters",
      value = NCLUS,
      min = 2,
      max = 10
    )
  })
    
  output$uinanyos <- renderUI({
      message("UINANYOS")
      clusters <- dfclusters()
      selectInput("nanyos", "Años",
                  choices = unique(clusters$anyo), 
                  selected = unique(clusters$anyo)[1])
  })
  output$uimuni <- renderUI({
      clusters <- dfclusters()
      selectInput("muni", "Municipio",
                  choices = unique(clusters$mun_dest), multiple = TRUE,
                  selected = unique(clusters$mun_dest)[1])
  })
  output$uiccaa <- renderUI({
      selectInput("ccaa", "Comunidad Autonoma origen",
                  choices = c("Andalucía", "Aragón", "Asturias, Principado de", 
                         "Balears, Illes", "Canarias", "Cantabria", 
                         "Castilla - La Mancha", "Castilla y León", "Cataluña", 
                         "Ceuta", "Comunitat Valenciana", "Extremadura", "Galicia", 
                         "Madrid, Comunidad de", "Melilla", "Murcia, Región de", 
                         "Navarra, Comunidad Foral de", "País Vasco", "Rioja, La"),
                  selected = "Andalucía")
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
  

  output$points <- renderPlotly({
    req(input$nanyos, input$ccaa, dfclusters())
    clusters <- dfclusters() 
    p <- ggplot(clusters, aes(x = clusters[[input$ccaa]], y = receptor, colour = cluster)) +
            geom_point() +
            labs(x = "CCAA", y = "Receptor", colour = "Cluster", title = "Dispersion plot of CCAA and Receptor colored by Cluster") +
            theme_minimal()
    return(ggplotly(p))
  })

  observeEvent(input$abguardar, {


    ## Copiar resto input a output para siguientes pasos

    file.copy(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
              paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_02_valoracion_sim.json"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_02_valoracion_sim.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_01_municipios_geo.json"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_01_municipios_geo.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_04_interno_prov.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_04_interno_prov.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_05_interno_mun.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_05_interno_mun.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_03_receptor.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_03_receptor.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_REG.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO_REG.csv"))
    

    sendSweetAlert(
      session = session,
      title = "¡¡ Éxito !!",
      text = "Se han guardado los ficheros para el siguiente paso del caso.",
      type = "success"
    )
  })

}

shinyApp(ui, server)
