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

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

COL1 <- rgb(33/255, 150/255, 243/255)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    
    titlePanel(title = "SIMULACION 05 - CitizenLab CU 25"),

    # ... Otros elementos de la UI
    
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(condition="input.tabselected==1",
        uiOutput("uisemana"),
        uiOutput("uiespecialidad")),                                  
        selectInput("dataset_plot", "Seleccionar conjunto de datos",
                    choices = c("Datos lista de espera", "Datos de capacidad", 
                                "Indicadores de área")),
            conditionalPanel(condition="input.tabselected==1",
        selectInput("variable_plot", "Seleccionar variable numérica",
                    choices = NULL)),
        

      ),
      
      mainPanel(
        tabsetPanel(id = "tabselected",
          tabPanel("Visualización", value = 1,

                   # Add the conditional block to show the leaflet map
                   conditionalPanel(
                     condition = "input.horizonte != 'Todas'",
                     h1("Mapa"),
                     leafletOutput("mapa")
                   ),
                           # Add the conditional block to show the time series plot
                conditionalPanel(
                  condition = "input.horizonte == 'Todas'",
                  h1("Plot"),
                  plotlyOutput("time_series_plot")
                )
          ),

                         tabPanel(title = "Tabla de Datos", value = 2,
                                  DT::dataTableOutput("table_data")
                         )

        )
      )
    )
  )
}


server <- function(input, output,session) {


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
  

  dfhistorico <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_25_05_07_02_lista_espera.csv"), 
             show_col_types = FALSE)
  })
  
  ## Read capacity data
  dfcapacidad <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_25_05_07_01_capacidad.csv"), 
             show_col_types = FALSE)
  })
  
  ## Read area indicators data
  dfindicadores <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_25_05_06_indicadores_area.csv"), 
             show_col_types = FALSE)
  })
  
  ## Read hospitals data
  dfhospitales <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_25_05_05_01_hospitales.csv"), 
             show_col_types = FALSE)
  })

      df_zonas <- reactive({
    st_read(paste0(carpetas()$carpeta_entrada, 
                    "/CU_25_05_03_areasgeo.json"))
  })

  output$uiespecialidad <- renderUI({
    selectInput(
      inputId = "especialidad",
      label = "Especialidad",
      choices = unique(dfhistorico()$Especialidad),
      selected = "Angiología y Cirugía Vascular"
    )
  })

  output$uisemana <- renderUI({
    choices_b = c("Todas",seq(1, 52))
    if (input$dataset_plot != "Datos lista de espera") {
      choices_b = seq(1, 52)
    } 
    selectInput(
      inputId = "horizonte",
      label = "Semana",
      choices = choices_b,
      selected = 1
    )
  })


  output$variable <- renderUI({
    selectInput(
      inputId = "horizonte",
      label = "Semana",
      choices = c("","",""),
      selected = 1
    )
  })

  output$table_data <- DT::renderDataTable({

    if (input$dataset_plot == "Datos lista de espera") {
      dfhistorico()
    } else if (input$dataset_plot == "Datos de capacidad") {
      dfcapacidad()
    } else if (input$dataset_plot == "Indicadores de área") {
      dfindicadores()
    } 
  })


  observeEvent(input$dataset_plot, {
    if (input$dataset_plot == "Datos lista de espera") {
      updateSelectInput(session, "variable_plot",
                        choices = colnames(dfhistorico() %>% select(where(is.numeric))))
    } else if (input$dataset_plot == "Datos de capacidad") {
      updateSelectInput(session, "variable_plot",
                        choices = colnames(dfcapacidad() %>% select(where(is.numeric))))
    } else if (input$dataset_plot == "Indicadores de área") {
      updateSelectInput(session, "variable_plot",
                        choices = colnames(dfindicadores() %>% select(where(is.numeric))))
    } 
  })

  output$mapa <- renderLeaflet({
    
zonas <- df_zonas()
esp <- input$especialidad
h <- as.numeric(input$horizonte)

      if (input$dataset_plot == "Datos lista de espera") {
        datos <- dfhistorico()
          dmap <- zonas |> 
  full_join(datos |> 
              filter(Especialidad == esp,
                     semana == h),
            by = c("DESBDT" = "nombre_area")) 

qpal <- colorQuantile("Blues",  dmap[[input$variable_plot]], n = 4)  


leaflet(data = dmap) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric(
      palette = "Blues",  # Color palette for the polygons
      domain = dmap[[input$variable_plot]]
    )(dmap[[input$variable_plot]]),  # Color polygons based on selected input variable
    color = "#FFFFFF",  # Border color of polygons
    opacity = 1,  # Border opacity of polygons
    fillOpacity = 0.8,  # Fill opacity of polygons
    highlight = highlightOptions(
      weight = 2,  # Border weight of highlighted polygons
      color = "#666666",  # Border color of highlighted polygons
      fillOpacity = 0.8  # Fill opacity of highlighted polygons
    ),
    label = ~Especialidad,  # Labels of the polygons
    group = "Media Tiempo Días"  # Group name for layer control
  ) %>%
  addLegend(
    position = "bottomright",  # Legend position
    pal = colorNumeric(
      palette = "Blues",  # Color palette for the legend
      domain = dmap[[input$variable_plot]]  # Range of values for the legend
    ),
    values = dmap[[input$variable_plot]],  # Values to display in the legend
    title = input$variable_plot  # Title of the legend based on the selected input variable
  )

      } else if (input$dataset_plot == "Datos de capacidad") {
        datos <- dfcapacidad()
          dmap <- zonas |> 
  full_join(datos |> 
              filter(Especialidad == esp),
            by = c("DESBDT" = "nombre_area")) 


qpal <- colorQuantile("Blues",  dmap[[input$variable_plot]], n = 4)  


leaflet(data = dmap) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric(
      palette = "Blues",  # Color palette for the polygons
      domain = dmap[[input$variable_plot]]
    )(dmap[[input$variable_plot]]),  # Color polygons based on selected input variable
    color = "#FFFFFF",  # Border color of polygons
    opacity = 1,  # Border opacity of polygons
    fillOpacity = 0.8,  # Fill opacity of polygons
    highlight = highlightOptions(
      weight = 2,  # Border weight of highlighted polygons
      color = "#666666",  # Border color of highlighted polygons
      fillOpacity = 0.8  # Fill opacity of highlighted polygons
    ),
    label = ~Especialidad,  # Labels of the polygons
    group = "Media Tiempo Días"  # Group name for layer control
  ) %>%
  addLegend(
    position = "bottomright",  # Legend position
    pal = colorNumeric(
      palette = "Blues",  # Color palette for the legend
      domain = dmap[[input$variable_plot]]  # Range of values for the legend
    ),
    values = dmap[[input$variable_plot]],  # Values to display in the legend
    title = input$variable_plot  # Title of the legend based on the selected input variable
  )

      } else if (input$dataset_plot == "Indicadores de área") {
        datos <- dfindicadores()
          dmap <- zonas |> 
  full_join(datos,
            by = c("DESBDT" = "nombre_area")) 

qpal <- colorQuantile("Blues",  dmap[[input$variable_plot]], n = 4)  


leaflet(data = dmap) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric(
      palette = "Blues",  # Color palette for the polygons
      domain = dmap[[input$variable_plot]]
    )(dmap[[input$variable_plot]]),  # Color polygons based on selected input variable
    color = "#FFFFFF",  # Border color of polygons
    opacity = 1,  # Border opacity of polygons
    fillOpacity = 0.8,  # Fill opacity of polygons
    highlight = highlightOptions(
      weight = 2,  # Border weight of highlighted polygons
      color = "#666666",  # Border color of highlighted polygons
      fillOpacity = 0.8  # Fill opacity of highlighted polygons
    ),
    label = "indicador",  # Labels of the polygons
    group = "Media Tiempo Días"  # Group name for layer control
  ) %>%
  addLegend(
    position = "bottomright",  # Legend position
    pal = colorNumeric(
      palette = "Blues",  # Color palette for the legend
      domain = dmap[[input$variable_plot]]  # Range of values for the legend
    ),
    values = dmap[[input$variable_plot]],  # Values to display in the legend
    title = input$variable_plot  # Title of the legend based on the selected input variable
  )


      } 

  
  })


  # Render the time series plot. Group info by the selected especialidad and sum the values of input$variable_plot
  output$time_series_plot <- renderPlotly({
    datos <- dfhistorico()
    esp <- input$especialidad
    variable <- input$variable_plot

    datos_filter <- datos |> 
      filter(Especialidad == esp) |> 
      group_by(semana) |> 
      summarise(across(where(is.numeric), sum))

    print(esp)
    print(datos_filter)
    datos_filter = na.omit(datos_filter)
    p<- ggplot(datos_filter, aes(x = semana, y = get(variable))) +
      geom_line() +
      labs(x = "Semana", y = variable, title = "Serie temporal") +
      theme_minimal()
    return(p)

  })

}

shinyApp(ui, server)
  
  
