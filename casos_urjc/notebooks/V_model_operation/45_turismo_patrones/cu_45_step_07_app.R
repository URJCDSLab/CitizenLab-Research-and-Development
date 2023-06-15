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



library(stars)
library(gstat)
library(leaflet)
library(leafem)
## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),
    
    titlePanel(title = "Oportunidades - CitizenLab CU 45"),

    # ... Otros elementos de la UI
    
    
    fluidRow(
          # fluidRow(
    br(),br(),

      column(width = 12,
              leafletOutput("map") |>
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
    
  rdsdata <- reactive({
      readRDS("cu_45_step_07_output/krige_oportunidad.rds")
  })
    
  output$map <- renderLeaflet({
    b.punt <- rdsdata()
    pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#EE2088"), b.punt$var1.pred,
                    na.color = "transparent")

    map <- leaflet() |> 
      addTiles() |> 
      addStarsImage(b.punt, colors = pal, opacity = 0.8) |> 
      addLegend(pal = pal, values = b.punt$var1.pred,
                title = "Oportunidades por valoraciones")
    return(map)
  })
}

shinyApp(ui, server)