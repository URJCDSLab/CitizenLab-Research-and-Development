########
# APP PASO 8 (VISUALIZACIÓN MAPA DE RIESGO)
########

library(shiny)
library(bslib, warn.conflicts = FALSE)
library(shinyWidgets)
# library(shinydashboard)
library(leaflet)
library(leafem)

library(dplyr, warn.conflicts = FALSE)
library(readr)
library(stars)


## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)



ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    # useShinydashboard(),
    titlePanel(title = "Mapa de riesgo - CitizenLab CU 04"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("uipimodelo")
      ),
      mainPanel(
        leafletOutput("mapariesgo")
      )
    )
  )}

server <- function(input, output, session) {
  
  ## reactives ----
  
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
  
  ## observers ----
  
  observeEvent(input$error_carpetas_faltan,{
    stopApp()
  })

    observeEvent(input$error_carpetas_existen,{
    stopApp()
  })
  
    
  ## reactives ----
  
  b <- reactive({
    read_rds(paste0(carpetas()$carpeta_entrada, "/", input$pimodelo))
  })  
    
      
  ## ui outputs ----
  
  modelos_disponibles <- reactive({
    modelos <- setNames(c("krige_pred.rds", "krige_proy.rds"), 
             c("Según predicción resto campaña actual",
               "Según proyección próxima campaña ")) 
    modelos[modelos %in% dir(carpetas()$carpeta_entrada, "^krige")]
  })
  
  output$uipimodelo <- renderUI({
    modelos <- modelos_disponibles()
    pickerInput(inputId = "pimodelo", 
                label = "Seleccione el modelo",
                choices = c(`Seleccione un modelo de riesgo` = "",
                            modelos))
  })
  
  output$mapariesgo <- renderLeaflet({
    req(input$pimodelo)
    pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#EE2088"), b()$var1.pred,
                        na.color = "transparent")
    
    leaflet() |> 
      addTiles() |> 
      addStarsImage(b(), colors = pal, opacity = 0.8) |> 
      addLegend(pal = pal, values = b()$var1.pred,
                title = "Riesgo de saturación")
  })
  
}

shinyApp(ui, server)