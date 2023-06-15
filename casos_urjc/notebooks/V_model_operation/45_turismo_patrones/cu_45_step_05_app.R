########
# APP PASO 4 (PROYECCIÓN)
########

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

library(readr)
library(dplyr)
library(tidyr)
library(nnet)
library(janitor)
library(purrr)
library(effects)

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),
    
    titlePanel(title = "SIMULACION 05 - CitizenLab CU 45"),

    # ... Otros elementos de la UI
    
    
    fluidRow(
          # fluidRow(
    br(),br(),

      column(width = 6,
            uiOutput("uinanyos"), 
            h3("Coeficientes"),
            DT::dataTableOutput("coef") |>
                withSpinner(7),
            h3("Cluster predicho"),
            uiOutput("clustersmap"),
          leafletOutput("plot_data") |>
            withSpinner(4)
      )
    ),
    mainPanel(
      tableOutput("capacidad_table")
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
  
    # Resto del código del servidor...
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
    read_csv(paste0(carpetas()$carpeta_entrada, "/cluster_anyos.csv"), 
             show_col_types = FALSE)
  })
  dfescenarios <- reactive({  
    read_csv(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_REG.csv"), 
             show_col_types = FALSE)
  })
    
  output$uinanyos <- renderUI({
      message("UINANYOS")
      clusters <- dfclusters()
      selectInput("nanyos", "Años",
                  choices = unique(clusters$anyo), 
                  selected = unique(clusters$anyo)[1])
  })

  model <- reactive({
    req(input$nanyos)
    cluster_anyos <- dfclusters()
    
    options(contrasts = c("contr.sum", "contr.poly")) 
    
    ANYO <- input$nanyos
    cluster_anyos %>%
      filter(anyo == ANYO) %>% 
      select(-c(anyo, mun_dest)) %>%
      clean_names() %>% 
      multinom(cluster ~ ., data = .)
  })

  pred <- reactive({
    req(model, dfescenarios)
    escenario <- dfescenarios()
    names(escenario) <- names(escenario) %>%
      tolower() %>%  # Convert to lower case
      iconv("UTF-8", "ASCII//TRANSLIT") %>%  # Remove special characters
      gsub("[, -]+", "_", .) %>%  # Replace spaces and commas with underscores
      gsub("_+", "_", .) %>%  # Replace two or more consecutive underscores with a single underscore
      gsub("\\.", "", .)  # Remove periods
    predict(model(), escenario)
  })

  output$coef <- DT::renderDataTable({
    req(model)
    
    ## Coeficientes
    coefs <- as.data.frame(coef(model()))

    # Transpose the dataframe as coef returns terms as columns
    coefs <- t(coefs)
    
    # Convert row names into a column
    coefs <- data.frame(Term = rownames(coefs), coefs)
    rownames(coefs) <- NULL
    return(coefs)
  })

  output$clustersmap <- renderText({
    req(pred)
    paste0("Cluster predicho: ", pred())
  })  
  output$plot_data <- renderLeaflet({
    clusters <- dfclusters() |> 
                 filter(cluster == pred())
    datageo <- st_read(paste0(carpetas()$carpeta_entrada, "/CU_45_05_01_municipios_geo.json"))
    clusters <- clusters |>
        inner_join(datageo, by = c("mun_dest" = "name"))
    map <- sf::st_as_sf(clusters) |> 
        leaflet() |> 
        addTiles() |> 
        addPolygons(color = "yellow",
                weight = 1,
                smoothFactor = 0.5,
                fillOpacity = 1,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label = ~paste0("Municipio: ", mun_dest)) 
    return(map)
  })
}

shinyApp(ui, server)