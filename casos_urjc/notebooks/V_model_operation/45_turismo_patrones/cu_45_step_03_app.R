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
library(leafem)
library(leaflet)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)
if (!require(mapSpain)) {
    install.packages("mapSpain")
}
library(mapSpain)

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
    titlePanel(title = "Visualización - CitizenLab CU 45"),
    
    ## UI mainpanel ----
    mainPanel(width = 12,
              navbarPage("Visualización",
                         tabPanel(title = "Table data",
                                  selectInput("dataset", "Seleccionar conjunto de datos",
                                              choices = c("Valoraciones", "Datos receptor", "Datos de municipio", 
                                                          "Datos de provincia")),
                                  DT::dataTableOutput("table_data") |>
                                    withSpinner(7)
                         ),
                         tabPanel(title = "Plot data",
                                  selectInput("dataset_plot", "Seleccionar conjunto de datos",
                                              choices = c("Valoraciones", "Datos recepción", "Datos emisión")),
                                  leafletOutput("plot_data") |>
                                    withSpinner(4)
                         ),

              )
    )
  )
}


server <- function(input, output, session) {


  ## . carpetas ----
  carpetas <- reactive({
    
    carpeta_entrada <- getQueryString()$carpeta_entrada
    if(any(is.null(carpeta_entrada))){
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
    } else if(!all(file.exists(carpeta_entrada))){
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
      return(invisible(list(carpeta_entrada = carpeta_entrada)))
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
  
  ## Render table data
  output$table_data <- DT::renderDataTable({
    if (input$dataset == "Datos receptor") {
      dfreceptor()
    } else if (input$dataset == "Datos de municipios") {
      dfmunicipios()
    } else if (input$dataset == "Datos de provincias") {
      dfprovincias()
    } else if (input$dataset == "Valoraciones") {
      dfvaloraciones()
    } 
  })
 
  ## Render plot based on selected dataset and numeric variable
  output$plot_data <- renderLeaflet({
    if (!is.null(input$dataset_plot)) {
      message(input$dataset_plot)
      datageo <- st_read(paste0(carpetas()$carpeta_entrada, "/CU_45_05_01_municipios_geo.json"))
      if (input$dataset_plot == "Datos recepción") {
        message("Datos receptor")
        df <- dfreceptor()
        df <- df |> 
            filter(!grepl("Total", pais_orig))
        df <- df |>
            full_join(datageo, by = c("CMUN" = "cmun"))
        
        mdata <- df |> 
            group_by(geometry, pais_orig) |>  # Added grouping by pais_orig
            summarise(valor = max(turistas, na.rm = TRUE)) 
        pal <- colorFactor(palette = "viridis", domain = unique(mdata$pais_orig))
        map <- sf::st_as_sf(mdata) |> 
            leaflet() |> 
            addTiles() |> 
            addPolygons(color = ~pal(pais_orig),  # fillColor depends on pais_orig
                    weight = 1,
                    smoothFactor = 0.5,
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    label = ~paste0("Pais origen: ", pais_orig)) |> 
            addLegend("bottomright", 
                      pal = pal, 
                      values = ~pais_orig,  # Changed from valor to pais_orig
                      title = "Pais de origen mayoritario",
                      labFormat = labelFormat(big.mark = " "),
                      opacity = 1
            )
        return(map)
      } else if (input$dataset_plot == "Datos emisión") {
        df <- dfmunicipios()
        sfmuni <- esp_get_munic()
        df <- df |>
            full_join(sfmuni, by = c("mun_orig_cod" = "LAU_CODE"))
        
        mdata <- df |> 
            group_by(geometry) |>  # Added grouping by pais_orig
            summarise(valor = sum(turistas, na.rm = TRUE)) 

        pal <- colorNumeric(palette = "Blues",
                    domain = mdata$valor)
        map <- sf::st_as_sf(mdata) |> 
            leaflet() |> 
            addTiles() |> 
            addPolygons(color = ~pal(valor),  # fillColor depends on pais_orig
                    weight = 1,
                    smoothFactor = 0.5,
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    label = ~paste0(valor, " Turistas")) |>
            addLegend("bottomright", 
                      pal = pal, 
                      values = ~valor,
                      title = "Número de turistas que visitan Madrid",
                      labFormat = labelFormat(big.mark = " "),
                      opacity = 1
            )
      
        return(map)
      } else if (input$dataset_plot == "Valoraciones") {
        message("Datos de provincias")
        df <- dfvaloraciones()
        pal <- colorNumeric(palette = "viridis", domain = df$puntos)
        
        map <- leaflet(df) %>%
            addTiles() %>%
            addCircleMarkers(
                ~X, ~Y, # longitude and latitude
                color = ~pal(puntos), # Color based on "puntos" column
                radius = 5, # Adjust this based on your requirements
                stroke = FALSE, fillOpacity = 0.8,
                label = ~paste0(nombre, " Turistas")
            ) %>%
            addLegend("bottomright", pal = pal, values = df$puntos,
                      title = "Puntos",
                      opacity = 1)
        return(map)
      }
      
    }
  })

}

shinyApp(ui, server)