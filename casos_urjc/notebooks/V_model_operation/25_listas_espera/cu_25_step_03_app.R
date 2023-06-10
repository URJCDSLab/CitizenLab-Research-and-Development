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

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Visualización - CitizenLab CU 25"),
    
    ## UI mainpanel ----
    mainPanel(width = 12,
              navbarPage("Visualización",
                         tabPanel(title = "Table data",
                                  selectInput("dataset", "Seleccionar conjunto de datos",
                                              choices = c("Datos históricos", "Datos de capacidad", 
                                                          "Indicadores de área", "Hospitales")),
                                  DT::dataTableOutput("table_data") |>
                                    withSpinner(7)
                         ),
                         tabPanel(title = "Plot data",
                                  selectInput("dataset_plot", "Seleccionar conjunto de datos",
                                              choices = c("Datos históricos", "Datos de capacidad", 
                                                          "Indicadores de área", "Hospitales")),
                                  selectInput("variable_plot", "Seleccionar variable numérica",
                                              choices = NULL),
                                  plotOutput("plot_data") |>
                                    withSpinner(4)
                         ),    # fluidRow(          

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
  
  ## Render table data
  output$table_data <- DT::renderDataTable({
    if (input$dataset == "Datos históricos") {
      dfhistorico()
    } else if (input$dataset == "Datos de capacidad") {
      dfcapacidad()
    } else if (input$dataset == "Indicadores de área") {
      dfindicadores()
    } else if (input$dataset == "Hospitales") {
      dfhospitales()
    }
  })


  ## Update variable options based on selected dataset for plotting
  observeEvent(input$dataset_plot, {
    if (input$dataset_plot == "Datos históricos") {
      updateSelectInput(session, "variable_plot",
                        choices = colnames(dfhistorico() %>% select(where(is.numeric))))
    } else if (input$dataset_plot == "Datos de capacidad") {
      updateSelectInput(session, "variable_plot",
                        choices = colnames(dfcapacidad() %>% select(where(is.numeric))))
    } else if (input$dataset_plot == "Indicadores de área") {
      updateSelectInput(session, "variable_plot",
                        choices = colnames(dfindicadores() %>% select(where(is.numeric))))
    } else if (input$dataset_plot == "Hospitales") {
      updateSelectInput(session, "variable_plot",
                        choices = colnames(dfhospitales() %>% select(where(is.numeric))))
    }
  })
  
  ## Render plot based on selected dataset and numeric variable
  output$plot_data <- renderPlot({
    if (!is.null(input$dataset_plot) && !is.null(input$variable_plot)) {
      if (input$dataset_plot == "Datos históricos") {
        df <- dfhistorico()
      } else if (input$dataset_plot == "Datos de capacidad") {
        df <- dfcapacidad()
      } else if (input$dataset_plot == "Indicadores de área") {
        df <- dfindicadores()
      } else if (input$dataset_plot == "Hospitales") {
        df <- dfhospitales()
      }
      
      ggplot(data = df, aes(x = !!sym(input$variable_plot))) +
        geom_histogram() +
        labs(x = input$variable_plot, y = "Count")
    }
  })


output$grouped_time_series_plot <- renderPlot({
  if (!is.null(input$group_variable) && !is.null(input$numeric_variable)) {
    if (input$categorical_value == "TODAS") {
      plot_data <- aggregate(dfhistorico()[, input$numeric_variable],
                             by = list(dfhistorico()[[input$group_variable]]),
                             FUN = function(x) c(Sum = sum(x), Mean = mean(x)))
      plot(plot_data[, input$numeric_variable], type = "n", main = "Grouped Time Series Plot")
      for (i in 1:nrow(plot_data)) {
        lines(plot_data[i, 2:length(plot_data)], col = i)
      }
      legend("topright", legend = plot_data[, 1], col = 1:nrow(plot_data), lty = 1)
    } else {
      subset_data <- dfhistorico()[dfhistorico()[[input$categorical_variable]] == input$categorical_value, ]
      plot(subset_data$ano + subset_data$semana/52, subset_data[[input$numeric_variable]],
           xlab = "Year", ylab = "Numeric Variable", main = "Time Series Plot")
    }
  }
})

}

shinyApp(ui, server)
  
  