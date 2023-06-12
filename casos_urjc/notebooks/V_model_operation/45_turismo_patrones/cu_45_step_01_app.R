########
# APP PASO 1 (SELECCIÓN DE FICHEROS) CU 04 (GESTIÓN VACUNAS GRIPE)
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
    titlePanel(title = "Carga de datos - CitizenLab CU 25"),
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
      h3("Fichero histórico de recepción"),
      column(4,
             tabsetPanel(id = "recepcion",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pirecepcion",
                                    label = "Fichero histórico",
                                    choices = c("CU_45_05_03_receptor.csv"),
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
             dataTableOutput("trecepcion")),
    ),
    hr(),
    ## fichero provincias ----
    fluidRow(
      h3("Fichero por municipios"),
      column(4,
             tabsetPanel(id = "municipios",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pimunicipios",
                                    label = "Fichero por municipios",
                                    choices = c("CU_45_05_05_interno_mun.csv"),
                                    options = list(
                                      title = "Seleccione un fichero")
                                  ),
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
             tabsetPanel(id = "tsmunicipios",
                         tabPanel("Datos",
                                  dataTableOutput("tmunicipios")))
      ),
    ),
    hr(),
    ## fichero histórico ----
    fluidRow(
      h3("Fichero por provincias"),
      column(4,
             tabsetPanel(id = "provincias",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piprovincias",
                                    label = "Fichero por provincias",
                                    choices = c("CU_45_05_04_interno_prov.csv"),
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
             dataTableOutput("tprovincias")
      ),
    ),
    hr(),

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
  
  dfrecepcion <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pirecepcion != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pirecepcion)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
  dfprovincias <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piprovincias != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piprovincias)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
  
  dfmunicipios <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pimunicipios != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pimunicipios)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
  observeEvent(carpetas(), {
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Revise la url, alguna carpeta requerida en el caso no se ha especificado (carpeta_entrada, carpeta_salida).",
        type = "error"
      )
      # stopApp() 
    }
  })
  
  observeEvent(input$abguardar, {



    ## Copiar resto input a output para siguientes pasos

    file.copy(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
              paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_02_valoracion_sim.json"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_02_valoracion_sim.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_01_municipios_geo.json"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_01_municipios_geo.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_04_interno_prov.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_04_interno_prov.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_05_interno_mun.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_05_interno_mun.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_45_05_03_receptor.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_45_05_03_receptor.csv"))
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
  
  output$trecepcion <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$pirecepcion)
    datatable(dfrecepcion(),
              options = list(scrollX = TRUE))
  })
  output$tprovincias <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$piprovincias)
    datatable(dfprovincias(),
              options = list(scrollX = TRUE))
  })
  output$tmunicipios <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$pimunicipios)
    datatable(dfmunicipios(), 
              options = list(paging = FALSE))
  })
  
}

shinyApp(ui, server, enableBookmarking = "url")


