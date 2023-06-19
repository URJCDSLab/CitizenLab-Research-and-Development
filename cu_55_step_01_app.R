########
# APP PASO 1 (SELECCIÓN DE FICHEROS) CU 45
########

## Paquetes ----

## UI
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)

# SERVER
library(sf)
library(readr)

## Funciones -----






ui <- function(request){
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Carga de datos - CitizenLab CU 55"),
    ## fichero zonas ----
    fluidRow(
      p("Seleccione los ficheros para el caso de uso. Existen ficheros de ejemplo, y próximamente se podrán usar ficheros de usuario. Se muestran las 1.000 primeras filas del fichero"),
      h3("Fichero de zonas"),
      column(4,
             tabsetPanel(id = "tszonas",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pizonas",
                                    label = "Fichero de areas geográficas",
                                    choices = c("CU_45_05_01_municipios_geo.json"),
                                    options = list(
                                      title = "Seleccione un fichero")
                                  )
                         ),
                         tabPanel("Fichero de usuario",
                                  p("Disponible en versiones post-prototipo"))
                         # fileInput("fzonas", 
                         #           label = "Seleccione nuevo fichero",
                         #           accept = ".json",
                         #           buttonLabel = "Explorar")),
             )
      ),
      column(8,
             dataTableOutput("tzonas")),
    ),
    hr(),
    ## fichero histórico ----
    fluidRow(
      h3("Fichero histórico de gasto"),
      column(4,
             tabsetPanel(id = "tsgasto",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pigasto",
                                    label = "Fichero histórico",
                                    choices = c("CU_55_05_02_gasto_municipio.csv"),
                                    options = list(
                                      title = "Seleccione un fichero")
                                  )
                         ),
                         tabPanel("Fichero de usuario",
                                  p("Disponible en versiones post-prototipo"))
                         # fileInput("fzonas", 
                         #           label = "Seleccione nuevo fichero",
                         #           accept = ".csv",
                         #           buttonLabel = "Explorar")),
             )
      ),
      column(8,
             dataTableOutput("tgasto")),
    ),
    hr(), 

        ## fichero origen ----
    fluidRow(
      h3("Fichero histórico de destino de turistas"),
      column(4,
             tabsetPanel(id = "tsturistadestino",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pituristadestino",
                                    label = "Fichero histórico turistas destino",
                                    choices = c("ESCENARIO_DESTINO.csv"),
                                    options = list(
                                      title = "Seleccione un fichero")
                                  )
                         ),
                         tabPanel("Fichero de usuario",
                                  p("Disponible en versiones post-prototipo"))
                         # fileInput("fzonas", 
                         #           label = "Seleccione nuevo fichero",
                         #           accept = ".csv",
                         #           buttonLabel = "Explorar")),
             )
      ),
      column(8,
             dataTableOutput("tturistadestino")),
    ),
    hr(), 
        ## fichero histórico ----
    fluidRow(
      h3("Fichero histórico de origen de turista"),
      column(4,
             tabsetPanel(id = "tsturistaorigen",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pituristaorigen",
                                    label = "Fichero histórico",
                                    choices = c("ESCENARIO_ORIGEN.csv"),
                                    options = list(
                                      title = "Seleccione un fichero")
                                  )
                         ),
                         tabPanel("Fichero de usuario",
                                  p("Disponible en versiones post-prototipo"))
                         # fileInput("fzonas", 
                         #           label = "Seleccione nuevo fichero",
                         #           accept = ".csv",
                         #           buttonLabel = "Explorar")),
             )
      ),
      column(8,
             dataTableOutput("tturistaorigen")),
    ),
    hr(), 
    # fluidRow(
    h3("Guardar datos para el siguiente paso"),
    actionBttn("abguardar",
               "Guardar datos",
               size = "md",
               icon = icon("floppy-disk")),
    br(),br()
    # )
  )
}

server <- function(input, output, session) {
  
  ## Reactives ----
  carpetas <- reactive({
    print("AASDFASDA")
    carpeta_entrada <- getQueryString()$carpeta_entrada
    carpeta_salida <- getQueryString()$carpeta_salida
    print(carpeta_entrada)
    print(carpeta_salida)
    print("============")
    if(any(is.null(carpeta_entrada), 
           is.null(carpeta_salida))){
      confirmSweetAlert(
        session = session,
        inputId = "error_carpetas_faltan",
        title = "Error",
        text = "Revise la url, alguna carpeta requerida en el caso no se ha especificado (carpeta_entrada, carpeta_salida). La aplicación se cerrará.",
        type = "error",
        btn_labels = c("", "Cerrar"),
        btn_colors = c("white", "red")
      )
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
      return(invisible(list(carpeta_entrada = carpeta_entrada, 
                            carpeta_salida = carpeta_salida)))
    }
  })
  
  ## observers ----
  
  observeEvent(input$error_carpetas_faltan,{
    stopApp()
  })
  
  observeEvent(input$error_carpetas_existen,{
    stopApp()
  })
  
  sfzonas <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pizonas != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pizonas)
      st_read(file_name, quiet = TRUE) 
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
  dfgasto <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pigasto != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pigasto)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
dfdestino <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pituristadestino != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pituristadestino)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
dforigen <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pituristaorigen != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pituristaorigen)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
# dfmunicipios <- reactive({
#    if(!is.list(carpetas())){
#      sendSweetAlert(
#        session = session,
#        title = "Error",
#        text = "No se ha encontrado alguna carpeta necesaria para el caso",
#        type = "error"
#      )
#      stopApp()
#    }
#    if(input$pimunicipios != ""){
#      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pimunicipios)
#      read_csv(file_name, show_col_types = FALSE)
#    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
#      
#    } else{
#      NA
#    }
#  })
  
  observeEvent(carpetas(), {
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Revise la url, alguna carpeta requerida en el caso no se ha especificado (carpeta_entrada, carpeta_salida).",
        type = "error"
      )
       stopApp() 
    }
  })
  
  observeEvent(input$abguardar, {



    ## Copiar resto input a output para siguientes pasos

    file.copy(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
             paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))
#    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_02_valoracion_sim.json"),
#              paste0(carpetas()$carpeta_salida, "/CU_45_05_02_valoracion_sim.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_01_municipios_geo.json"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_01_municipios_geo.json"))
 #   file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_04_interno_prov.csv"),
#              paste0(carpetas()$carpeta_salida, "/CU_45_05_04_interno_prov.csv"))
#    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_05_interno_mun.csv"),
#              paste0(carpetas()$carpeta_salida, "/CU_45_05_05_interno_mun.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_55_05_02_gasto_municipio.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_55_05_02_gasto_municipio.csv"))
 file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_ORIGEN.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO_ORIGEN.csv"))
      file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO_DESTINO.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO_DESTINO.csv"))
      
      
    sendSweetAlert(
        session = session,
        title = "¡¡ Éxito !!",
        text = "Se han guardado los ficheros para el siguiente paso del caso.",
        type = "success"
    )
      
    
    
  })
  
  ## Output rendering ----
  
  output$tzonas <- DT::renderDataTable({
    # req(carpetas$carpeta_entrada)
    req(input$pizonas)
    datatable(sfzonas(), 
              options = list(autoWidth = TRUE))
  })
  
  output$tgasto <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$pigasto)
    datatable(dfgasto(),
              options = list(scrollX = TRUE))
  })

output$tturistadestino <- renderDataTable({
   # req(carpetas_sesion$carpeta_entrada)
    req(input$pituristadestino)
    datatable(dfdestino(),
              options = list(scrollX = TRUE))
    })

output$tturistaorigen <- renderDataTable({
  #  req(carpetas_sesion$carpeta_entrada)
    req(input$pituristaorigen)
    datatable(dforigen(),
              options = list(scrollX = TRUE))
    })
#  output$tmunicipios <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
#    req(input$pimunicipios)
#    datatable(dfmunicipios(), 
#              options = list(paging = TRUE))
#  })
  
}

shinyApp(ui, server, enableBookmarking = "url")


