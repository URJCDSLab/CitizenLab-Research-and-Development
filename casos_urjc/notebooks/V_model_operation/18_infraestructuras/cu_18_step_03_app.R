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

   

    sidebarLayout(

      sidebarPanel(

        column(width = 6,

               uiOutput("uivariable_plot"),
               uiOutput("date_selector")

        )

      ),

      mainPanel(

        tabsetPanel(

                    tabPanel("Tabla",

                   column(width = 12,

                
                                  DT::dataTableOutput("table_data")

                         

                   )

          ),

          tabPanel("Visualización",

                   column(width = 12,

                          leafletOutput("map")

                   )

          )

        )

      )

    )

  )

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
  

  df_summarised_diario <- reactive ({
    req(input$dates)

    datos_diario_filter <- datos_diario() %>% filter(fecha >= input$dates[1] & fecha <= input$dates[2])

    df_summarised <- datos_diario_filter %>%
    group_by(id_inf) %>%
    summarise(across(where(is.numeric) & !c(tmed, velmedia, presMax), sum, na.rm = TRUE),
    tmed = mean(tmed, na.rm = TRUE),
    velmedia = mean(velmedia, na.rm = TRUE),
    presMax = mean(presMax, na.rm = TRUE))
  })


output$date_selector <- renderUI({
    if (!(dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito")){
    dateRangeInput("dates", "Date range:", 
                   start = min(datos_diario()$fecha), 
                   end = max(datos_diario()$fecha))}
  })

output$uivariable_plot <- renderUI({
    
    datos_plot <- if (dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito") {
        datos_distritos()
    } else {
        datos_diario()
    }
    
    selectInput(
      inputId = "variable_plot",
      label = "Seleccionar variable numérica",
      choices = colnames(datos_plot %>% select(where(is.numeric))),
      selected = colnames(datos_plot %>% select(where(is.numeric)))[1]
    )
})

  output$table_data <- DT::renderDataTable({




    if (dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito") {

      datatable(datos_distritos(), options=list(scrollX = TRUE))

    } else{
      


      datatable(df_summarised_diario(), options=list(scrollX = TRUE))

    } 

  })
  ## Render leaflet map
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles()
    
    if (dfvariables() |> filter(variable == "NIVEL") |> pull(valor) == "Distrito") {



dmap <- distritos_geojson() |> inner_join(datos_distritos(), by = c("CDIS" = "cdis"), multiple="all") #|> slice(1:4000)

qpal <- colorNumeric(palette="Blues", domain = dmap[[input$variable_plot]])  

dmap |> leaflet() |>
  addTiles() |>
  addPolygons(color="black", 
    fillColor = ~qpal(dmap[[input$variable_plot]]),

              fillOpacity = 0.8,

              weight = 1

              # popup = ~paste(round(input$variable_plot), "distritos"))  
  ) |>
  addLegend("bottomright", pal = qpal, values = dmap[[input$variable_plot]],

            title = "Distritos",

            opacity = 1

  ) 
  }else{ 
    
    
    dmap <- df_summarised_diario() |> inner_join(datos_infra(), by = c("id_inf" = "id_inf"), multiple="all") |> slice(1:4000)
    



    gpal <- colorNumeric(palette="Blues", domain = dmap[[input$variable_plot]]) 

    map <- leaflet(dmap) %>%
            addTiles() %>%
            addCircleMarkers(
                ~X, ~Y, # longitude and latitude
                color = ~gpal(dmap[[input$variable_plot]]), # Color based on "puntos" column
                radius = 5, # Adjust this based on your requirements
                stroke = FALSE, fillOpacity = 0.8
                # label = ~paste0(nombre, " Turistas")
            ) %>%
            addLegend("bottomright", pal = gpal, values = dmap[[input$variable_plot]],
                      title = input$variable_plot,
                      opacity = 1)
  }})


}

shinyApp(ui, server)
