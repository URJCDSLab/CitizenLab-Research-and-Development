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
    titlePanel(title = "Carga de datos - CitizenLab CU 34"),
    ## fichero zonas ----
    fluidRow(
      p("Seleccione los ficheros para el caso de uso. Existen ficheros de ejemplo, y próximamente se podrán usar ficheros de usuario. Se muestran las 1.000 primeras filas del fichero"),
      h3("Fichero de secciones"),
      column(4,
             tabsetPanel(id = "tssecciones",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pisecciones",
                                    label = "Fichero de secciones",
                                    choices = c("CU_34_05_01_secciones_geo.json"),
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
             dataTableOutput("tsecciones")),
    ),
    hr(),
    ## fichero histórico ----
    fluidRow(
      h3("Fichero de servicios"),
      column(4,
             tabsetPanel(id = "tsservicios",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piservicios",
                                    label = "Fichero servicios",
                                    choices = c("CU_34_05_05_servicios_completo.csv"),
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
             dataTableOutput("tservicios")),
    ),
    hr(),
    ## fichero infraestructuras ----
    fluidRow(
      h3("Fichero de escenarios"),
      column(4,
             tabsetPanel(id = "tsescenarios",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piescenarios",
                                    label = "Fichero escenarios",
                                    choices = c("ESCENARIO_SERVICIOS.csv"),
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
      column(8, dataTableOutput("tescenarios")
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

  sfsecciones <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pisecciones != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pisecciones)
      st_read(file_name, quiet = TRUE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO

    } else{
      NA
    }
  })

  dfservicios <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piservicios != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piservicios)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO

    } else{
      NA
    }
  })

  dfescenarios <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piescenarios != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piescenarios)
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
    cond <- is.na(list(sfsecciones(),
                       dfservicios(),
                       dfescenarios()))
    if(any(cond)){
      sendSweetAlert(
        session = session,
        title = "No se pudo completar",
        text = "No se han seleccionado todos los ficheros de datos requeridos",
        type = "warning"
      )

    } else{
      ## Guardar archivos de este caso
      st_write(obj = sfsecciones(),
               dsn = paste0(carpetas()$carpeta_salida, "/SECCIONES.json"),
               driver = "GeoJSON",
               delete_dsn = TRUE)
      write_csv(dfservicios(), paste0(carpetas()$carpeta_salida, "/SERVICIOS.csv"))
      write_csv(dfescenarios(), paste0(carpetas()$carpeta_salida, "/ESCENARIOS.csv"))
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

  output$tsecciones <- DT::renderDataTable({
    # req(carpetas$carpeta_entrada)
    req(input$pisecciones)
    datatable(sfsecciones(),
              options = list(scrollX = TRUE))
  })

  output$tservicios <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$piservicios)
    datatable(dfservicios(),
              options = list(scrollX = TRUE))
  })
  output$tindicadores <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$piinfraestructuras)
    datatable(dfindicadores(),
              options = list(scrollX = TRUE))
  })
  output$tescenarios <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$piescenarios)
    datatable(dfescenarios(),
              options = list(scrollX = TRUE))
  })
}

shinyApp(ui, server, enableBookmarking = "url")