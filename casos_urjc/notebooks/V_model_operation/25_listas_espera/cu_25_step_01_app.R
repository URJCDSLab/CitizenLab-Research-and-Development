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
                                    choices = c("CU_25_05_03_areasgeo.json"),
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
      h3("Fichero de histórico listas de espera"),
      column(4,
             tabsetPanel(id = "tshistorico",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pihistorico",
                                    label = "Fichero histórico",
                                    choices = c("CU_25_05_07_02_lista_espera.csv"),
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
             dataTableOutput("thistorico")),
    ),
    hr(),
    ## fichero indicadores ----
    fluidRow(
      h3("Fichero de hospitales"),
      column(4,
             tabsetPanel(id = "tsindicadores",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piindicadores",
                                    label = "Fichero hospitales",
                                    choices = c("CU_25_05_05_01_hospitales.csv"),
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
             tabsetPanel(id = "tsindicadoresdatos",
                         tabPanel("Datos",
                                  dataTableOutput("tindicadores")),)
      ),
    ),
    hr(),
    ## fichero histórico ----
    fluidRow(
      h3("Fichero datos de indicadores area"),
      column(4,
             tabsetPanel(id = "tsescucha",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piescucha",
                                    label = "Fichero datos de indicadores de area",
                                    choices = c("CU_25_05_06_indicadores_area.csv"),
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
             dataTableOutput("tescucha")
      ),
    ),
    hr(),
    ## fichero escenario ----
    fluidRow(
      h3("Fichero de capacidad"),
      column(4,
             tabsetPanel(id = "tsescenario",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piescenario",
                                    label = "Fichero de capacidad",
                                    choices = c("CU_25_05_07_01_capacidad.csv"),
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
             dataTableOutput("tescenario")
      ),
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
    
    carpeta_entrada <- getQueryString()$carpeta_entrada
    carpeta_salida <- getQueryString()$carpeta_salida
    carpeta_maestros <- getQueryString()$carpeta_maestros
    print(carpeta_entrada)
    print(carpeta_maestros)
    print(carpeta_salida)
    print("============")
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
  
  dfhistorico <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pihistorico != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pihistorico)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
  dfindicadores <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piindicadores != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piindicadores)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
  
  dfescucha <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piescucha != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piescucha)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  dfescenario <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piescenario != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piescenario)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  dfnuevacampana <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piescenario != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pinuevacampana)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
  dfnuevacapacidad <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piescenario != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pinuevacapacidad)
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
        text = "Revise la url, alguna carpeta requerida en el caso no se ha especificado (carpeta_entrada, carpeta_salida, carpeta_maestros).",
        type = "error"
      )
      # stopApp() 
    }
  })
  
  observeEvent(input$abguardar, {



    ## Copiar resto input a output para siguientes pasos
      file.copy(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
              paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_03_areasgeo.json"),
              paste0(carpetas()$carpeta_salida, "/CU_25_05_03_areasgeo.json"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_05_01_hospitales.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_25_05_05_01_hospitales.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_06_indicadores_area.csv"),
              paste0(carpetas()$carpeta_salida, "/CU_25_05_06_indicadores_area.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_07_01_capacidad.csv"),
          paste0(carpetas()$carpeta_salida, "/CU_25_05_07_01_capacidad.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CU_25_05_07_02_lista_espera.csv"),
          paste0(carpetas()$carpeta_salida, "/CU_25_05_07_02_lista_espera.csv"))
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
  
  output$thistorico <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$pihistorico)
    datatable(dfhistorico(),
              options = list(scrollX = TRUE))
  })
  output$tindicadores <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$piindicadores)
    datatable(dfindicadores(),
              options = list(scrollX = TRUE))
  })
  output$tindicadoresmeta <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$piindicadoresmeta)
    datatable(dfindicadoresmeta(), 
              options = list(paging = FALSE))
  })
  output$tescucha <- renderDataTable({
    req(input$piescucha)
    datatable(dfescucha())
  })
  output$tescenario <- renderDataTable({
    req(input$piescenario)
    datatable(dfescenario())
  })

  
}

shinyApp(ui, server, enableBookmarking = "url")


