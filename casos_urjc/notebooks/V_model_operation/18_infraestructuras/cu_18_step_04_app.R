# APP PASO 3 (VISUALIZACIÓN DE RESULTADOS)
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

    titlePanel(title = "Visualización - CitizenLab CU 18"),




    # ... Otros elementos de la UI

   

  



      mainPanel(

          tabPanel("Visualización",

                   column(width = 12,

                          leafletOutput("map")


          

        

      

    ))))

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
    print(carpetas()$carpeta_entrada)
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
    read_rds(file.path(carpetas()$carpeta_maestros,"/datos_cluster_distritos.rds"))})


  ## Render leaflet map
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles()
    
    if (dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito") {
# print("puto eugenio")
# print(names(datos_cluster_distritos()))

dmap <- distritos_geojson() |> # slice(1:100) |>
inner_join(datos_cluster_distritos(), by = c("CDIS" = "cdis"), multiple="all") 


colors <- colorFactor("RdYlBu", unique(dmap$cluster))

dmap |> leaflet() |>
  addTiles() |>
  addPolygons(color="black", 
    fillColor = ~colors(cluster),

              fillOpacity = 0.8,

              weight = 1,

              popup = ~paste("Distrito", CDIS) 
  ) |>
  addLegend("bottomright", values = ~cluster,
  pal=colors,
        # colors=~colors(unique(cluster)),

            title = "Clusters",

            opacity = 1

  ) 
  }else{ 
    
    dmap <- datos_infra() |>
    inner_join(datos_cluster_diario(), by = c("id_inf" = "id_inf"), multiple="all") 

    colors <- colorFactor("RdYlBu", unique(dmap$cluster))

    map <- leaflet(dmap) %>%
            addTiles() %>%
            addCircleMarkers(
                ~X, ~Y, # longitude and latitude
                color = ~colors(cluster), # Color based on "puntos" column
                radius = 5, # Adjust this based on your requirements
                stroke = FALSE, fillOpacity = 0.8
                # label = ~paste0(nombre, " Turistas")
            ) %>%
            addLegend("bottomright", pal = colors, values = ~cluster,
                      title = "cluster",
                      opacity = 1)
  }})


}

shinyApp(ui, server)
