########
# APP PASO 1 (SELECCIÓN DE FICHEROS) CU 18 (Comportamiento Infra. Eventos extremos)
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
    titlePanel(title = "Carga de datos - CitizenLab CU 18"),
    ## fichero zonas ----
    fluidRow(
      p("Seleccione los ficheros para el caso de uso. Existen ficheros de ejemplo, y próximamente se podrán usar ficheros de usuario. Se muestran las 1.000 primeras filas del fichero"),
      h3("Fichero de zonas"),
      column(4,
             tabsetPanel(id = "tszonas",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pizonas",
                                    label = "Fichero de zonas",
                                    choices = c("CU_18_05_03_distritos_geo.json"),
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
      h3("Fichero de infraestructuras diario"),
      column(4,
             tabsetPanel(id = "tshistorico",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pihistorico",
                                    label = "Fichero histórico",
                                    choices = c("CU_18_05_20_diario_infra.csv"),
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
    ## fichero infraestructuras ----
    fluidRow(
      h3("Fichero de infraestructuras"),
      column(4,
             tabsetPanel(id = "tsinfraestructuras",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piinfraestructuras",
                                    label = "Fichero infraestructuras",
                                    choices = c("CU_18_05_19_01_infraestructuras.csv"),
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
             tabsetPanel(id = "tsinfraestructurasdatos",
                         tabPanel("Datos",
                                  dataTableOutput("tinfraestructuras")))
      ),
    ),
    hr(),
    ## fichero histórico ----
    fluidRow(
      h3("Fichero datos de distritos"),
      column(4,
             tabsetPanel(id = "tsdistritos",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pidistritos",
                                    label = "Fichero datos de distritos",
                                    choices = c("CU_18_05_16_distritos_variables.csv"),
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
             dataTableOutput("tdistritos")
      ),
    ),
    hr(),
    ## fichero escenario ----
    fluidRow(
      h3("Escenario cluster diario"),
      column(4,
             tabsetPanel(id = "tsescenario",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piescenario",
                                    label = "Fichero escenario cluster diario",
                                    choices = c("ESCENARIO_CLUSTER_DIARIO.csv"),
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
    ## fichero nueva cluster ----
    fluidRow(
      h3("Escenario cluster distrito"),
      column(4,
             tabsetPanel(id = "tsclusterdistrito",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piclusterdistrito",
                                    label = "Fichero escenario cluster distrito",
                                    choices = c("ESCENARIO_CLUSTER_DIST.csv"),
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
             dataTableOutput("tclusterdistrito")
      ),
    ),
    hr(),
    ## fichero nueva capacidad ----
    fluidRow(
      h3("Escenario regresión"),
      column(4,
             tabsetPanel(id = "tsregresion",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piregresion",
                                    label = "Fichero escenario regresión",
                                    choices = c("ESCENARIO_REGRESION.csv"),
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
             dataTableOutput("tregresion")
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
  
  dfinfraestructuras <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piinfraestructuras != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piinfraestructuras)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
  dfdistritos <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pidistritos != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pidistritos)
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
  dfclusterdistrito <- reactive({
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
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piclusterdistrito)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO
      
    } else{
      NA
    }
  })
  
  dfregresion <- reactive({
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
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piregresion)
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
    cond <- is.na(list(sfzonas(), 
                       dfhistorico(), 
                       dfinfraestructuras(), 
                       dfdistritos(),
                       dfescenario(),
                       dfclusterdistrito()))
    if(any(cond)){
      sendSweetAlert(
        session = session,
        title = "No se pudo completar",
        text = "No se han seleccionado todos los ficheros de datos requeridos",
        type = "warning"
      )
      
    } else{
      ## Guardar archivos de este caso
      st_write(obj = sfzonas(), 
               dsn = paste0(carpetas()$carpeta_salida, "/CU_18_05_03_distritos_geo.json"), 
               driver = "GeoJSON",
               delete_dsn = TRUE)
      write_csv(dfhistorico(), paste0(carpetas()$carpeta_salida, "/CU_18_05_20_diario_infra.csv"))
      write_csv(dfinfraestructuras(), paste0(carpetas()$carpeta_salida, "/CU_18_05_19_01_infraestructuras.csv"))
      write_csv(dfdistritos(), paste0(carpetas()$carpeta_salida, "/CU_18_05_16_distritos_variables.csv"))
      write_csv(dfescenario(), paste0(carpetas()$carpeta_salida, "/ESCENARIO_CLUSTER_DIARIO.csv
.csv"))
      write_csv(dfclusterdistrito(), paste0(carpetas()$carpeta_salida, "/ESCENARIO_CLUSTER_DIST.csv"))
      write_csv(dfregresion(), paste0(carpetas()$carpeta_salida, "/ESCENARIO_REGRESION.csv"))
      ## Mover archivos de los siguientes casos
      file.copy(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
                paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"),
                overwrite = TRUE)
      sendSweetAlert(
        session = session,
        title = "¡¡ Éxito !!",
        text = "Se han guardado los ficheros para el siguiente paso del caso.",
        type = "success"
      )
      
    }
    
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
  output$tinfraestructuras <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$piinfraestructuras)
    datatable(dfinfraestructuras(),
              options = list(scrollX = TRUE))
  })

  output$tdistritos <- renderDataTable({
    req(input$pidistritos)
    datatable(dfdistritos())
  })
  output$tescenario <- renderDataTable({
    req(input$piescenario)
    datatable(dfescenario())
  })
  output$tclusterdistrito <- renderDataTable({
    req(input$piclusterdistrito)
    datatable(dfclusterdistrito())
  })
  output$tregresion <- renderDataTable({
    req(input$piregresion)
    datatable(dfregresion())
  })
  
}

shinyApp(ui, server, enableBookmarking = "url")


