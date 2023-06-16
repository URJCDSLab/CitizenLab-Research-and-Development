########
# APP PASO 7 

library(shiny)
library(modeltime)
## UI
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
library(xgboost)
library(prophet)
library(tidymodels)
library(modeltime)
library(timetk)   
library(lubridate)
library(tidyverse)
## Server
library(readr)
library(mgcv)
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(modeltime)
library(simmer)
library(simmer.plot)
library(simmer)

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),
    
    titlePanel(title = "MAPA 07 - CitizenLab CU 25"),

    # ... Otros elementos de la UI
    
    sidebarLayout(
      sidebarPanel(
        # h3("Guardar datos para el siguiente paso"),
        # actionBttn("abguardar",
        #            "Guardar datos",
        #            size = "md",
        #            icon = icon("floppy-disk")),
        # br(), br(),
        uiOutput("uipt"),
        uiOutput("uipredsim"),
        uiOutput("uiespecialidad"),
        uiOutput("uihorizonte"),
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Mapa", 
                   h1("Mapa de Calor"),
                   leafletOutput("mapa_calor")
          )
        )
      )
    )
  )
}


server <- function(input, output, session) {
  
  #   observeEvent(input$abguardar, {

    

  #   sendSweetAlert(
  #     session = session,
  #     title = "¡¡ Éxito !!",
  #     text = "Se han guardado los ficheros para el siguiente paso del caso.",
  #     type = "success"
  #   )
  # })


  
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

  output$mapa_calor <- renderLeaflet({

esp <- input$especialidad
f <- as.numeric(input$horizonte)
    zonas <- df_zonas()
if (input$predsim == "Pred"){

    pred_h_personas <- pred_h_personas()
    pred_h_tiempo <- pred_h_tiempo()

    if (input$pt == "Personas"){
      datos <- pred_h_personas
      popup_legend <- "Personas"
    }else{
      datos <- pred_h_tiempo
      popup_legend <- "Tiempo"
    }
    print("_______________")
    print(datos)
    f <- datos$.index[f]
dmap <- zonas |> 
  full_join(datos |> 
              tidyr::separate(id, into = c("zona", "especialidad"), sep = "\\.") |> 
              filter(especialidad == esp,
                     .index == f),
            by = c("DESBDT" = "zona")) 
qpal <- colorQuantile("Blues", dmap$.value, n = 4)  
print(dmap)
a <- dmap |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons(color = ~qpal(.value), 
              fillOpacity = 0.8,
              weight = 1,
              popup = ~paste(round(.value), popup_legend),
              label = ~DESBDT)  |> 
  addLegend("bottomright", pal = qpal, values = ~.value,
            title = popup_legend,
            opacity = 1
  ) 
}else{
      sim_ultimo_llegadas <- sim_ultimo_llegadas()
      sim_ultimo_recursos <- sim_ultimo_recursos()
      sim_h_llegadas <- sim_h_llegadas()
      sim_h_recursos <- sim_h_recursos()
      if (f ==0 && input$pt == "Personas"){
      datos <- sim_ultimo_llegadas
      popup_legend <- "Llegadas"
      ## Este join tarda un poco, hago el filter primero
## Filtro valores menores que time, y después me quedo solo con el último valor de cada zona
dfsim <- sim_h_llegadas |> 
  tidyr::separate(id, into = c("zona", "especialidad"), sep = "\\.") |> 
  filter(especialidad == esp,
         start_time <= f) |> 
  group_by(zona) |> 
  summarise(cola = sum(finished))

dmap <- zonas |> 
  full_join(dfsim,
            by = c("DESBDT" = "zona"))  

qpal <- colorQuantile("Blues", dmap$cola, n = 4)  
a<- dmap |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons(color = ~qpal(cola), 
              fillOpacity = 0.8,
              weight = 1,
              popup = ~paste(round(cola), "personas"),
              label = ~DESBDT)  |> 
  addLegend("bottomright", pal = qpal, values = ~cola,
            title = "Cola",
            opacity = 1
  ) 


      
    }else if (f ==0 && input$pt == "Tiempo"){
      datos <- sim_ultimo_recursos
    popup_legend <- "Recursos"

    ## Este join tarda un poco, hago el filter primero
## Filtro valores menores que time, y después me quedo solo con el último valor de cada zona
dfsim <- sim_h_recursos |> 
  tidyr::separate(id, into = c("zona", "especialidad"), sep = "\\.") |> 
  filter(especialidad == esp,
         time <= f) |> 
  group_by(zona) |> 
  slice_tail(n = 1)

dmap <- zonas |> 
  full_join(dfsim,
            by = c("DESBDT" = "zona"))  

qpal <- colorQuantile("Blues", dmap$system, n = 4)  
a<-dmap |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons(color = ~qpal(system), 
              fillOpacity = 0.8,
              weight = 1,
              popup = ~paste(round(system), "personas"),
              label = ~DESBDT)  |> 
  addLegend("bottomright", pal = qpal, values = ~system,
            title = "Sistema",
            opacity = 1
  ) 


    }else if (input$pt == "Personas"){
      datos <- sim_h_llegadas
    popup_legend <- "Llegadas"

## Este join tarda un poco, hago el filter primero
## Filtro valores menores que time, y después me quedo solo con el último valor de cada zona
dfsim <- sim_h_llegadas |> 
  tidyr::separate(id, into = c("zona", "especialidad"), sep = "\\.") |> 
  filter(especialidad == esp,
         start_time <= f) |> 
  group_by(zona) |> 
  summarise(cola = sum(finished))

dmap <- zonas |> 
  full_join(dfsim,
            by = c("DESBDT" = "zona"))  

qpal <- colorQuantile("Blues", dmap$cola, n = 4)  
a<- dmap |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons(color = ~qpal(cola), 
              fillOpacity = 0.8,
              weight = 1,
              popup = ~paste(round(cola), "personas"),
              label = ~DESBDT)  |> 
  addLegend("bottomright", pal = qpal, values = ~cola,
            title = "Cola",
            opacity = 1
  ) 

    
    }else if (input$pt == "Tiempo"){
              datos <- sim_h_recursos
            popup_legend <- "Recursos"

        ## Este join tarda un poco, hago el filter primero
        ## Filtro valores menores que time, y después me quedo solo con el último valor de cada zona
        dfsim <- sim_h_recursos |> 
          tidyr::separate(id, into = c("zona", "especialidad"), sep = "\\.") |> 
          filter(especialidad == esp,
                time <= f) |> 
          group_by(zona) |> 
          slice_tail(n = 1)

        dmap <- zonas |> 
          full_join(dfsim,
                    by = c("DESBDT" = "zona"))  

        qpal <- colorQuantile("Blues", dmap$system, n = 4)  
        a <- dmap |> 
          leaflet() |> 
          addTiles() |> 
          addPolygons(color = ~qpal(system), 
                      fillOpacity = 0.8,
                      weight = 1,
                      popup = ~paste(round(system), "personas"),
                      label = ~DESBDT)  |> 
          addLegend("bottomright", pal = qpal, values = ~system,
                    title = "Sistema",
                    opacity = 1
          ) 




    }



  }

return(a)
  })



  output$uipt <- renderUI({
    selectInput(
      inputId = "pt",
      label = "Tipo",
      choices = c("Personas","Tiempo"),
      selected = "Personas"
    )
  })

  
  output$uipredsim <- renderUI({
    selectInput(
      inputId = "predsim",
      label = "Predicción o Simulación",
      choices = c("Pred","Sim"),
      selected = "Predicción"
    )
  })



  output$uiespecialidad <- renderUI({
    selectInput(
      inputId = "especialidad",
      label = "Especialidad",
      choices = unique(dfhistorico()$Especialidad),
      selected = "Angiología y Cirugía Vascular"
    )
  })

  pred_h_personas <- reactive({
    read_rds(paste0(carpetas()$carpeta_entrada, 
                    "/pred_h_personas.rds"))
  })

  pred_h_tiempo <- reactive({
    read_rds(paste0(carpetas()$carpeta_entrada, 
                    "/pred_h_tiempo.rds"))
  })
    sim_h_recursos <- reactive({
    read_rds(paste0(carpetas()$carpeta_entrada, 
                    "/sim_h_recursos.rds"))
  })

    sim_h_llegadas <- reactive({
    read_rds(paste0(carpetas()$carpeta_entrada, 
                    "/sim_h_llegadas.rds"))
  })

    sim_ultimo_llegadas <- reactive({
    read_rds(paste0(carpetas()$carpeta_entrada, 
                    "/sim_ultimo_llegadas.rds"))
  })
    sim_ultimo_recursos <- reactive({
    read_rds(paste0(carpetas()$carpeta_entrada, 
                    "/sim_ultimo_recursos.rds"))
  })

    df_zonas <- reactive({
    st_read(paste0(carpetas()$carpeta_entrada, 
                    "/CU_25_05_03_areasgeo.json"))
  })

  dfhistorico <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_25_05_07_02_lista_espera.csv"), 
             show_col_types = FALSE)
  })

    dfhospitales <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_25_05_05_01_hospitales.csv"), 
             show_col_types = FALSE)
  })

      dfcapacidad <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_25_05_07_01_capacidad.csv"), 
             show_col_types = FALSE)
  })

      dfindicadores <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/CU_25_05_06_indicadores_area.csv"), 
             show_col_types = FALSE)
  })

    dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })


  # Render the 'Horizonte' input
  output$uihorizonte <- renderUI({
    selectInput(
      inputId = "horizonte",
      label = "Horizonte temporal (semanas). Si es Cero se utilizara el último valor conocido.",
      choices = seq(0, 52),
      selected = 1
    )
  })

}

shinyApp(ui, server)
