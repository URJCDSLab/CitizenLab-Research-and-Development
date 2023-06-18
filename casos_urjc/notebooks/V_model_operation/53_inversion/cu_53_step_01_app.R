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
    titlePanel(title = "Carga de datos - CitizenLab CU 53"),
    ## fichero zonas ----
    hr(),
    hr(),
    hr(),
    fluidRow(
      p("Seleccione los ficheros para el caso de uso. Existen ficheros de ejemplo, y próximamente se podrán usar ficheros de usuario. Se muestran las 1.000 primeras filas del fichero"),
      h3("Archivo spi"),
      column(4,
             tabsetPanel(id = "tsspi",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pispi",
                                    label = "Archivo spi",
                                    choices = c("CU_53_05_02_01_spi.csv"),
                                    options = list(
                                      title = "Seleccione un fichero")
                                  )
                         ),
                         tabPanel("Fichero de usuario",
                                  p("Disponible en versiones post-prototipo"))
                         # fileInput("fzonas",
                         #           label = "Selesfpaiccione nuevo fichero",
                         #           accept = ".json",
                         #           buttonLabel = "Explorar")),
             )
      ),
      column(8,
             dataTableOutput("tspi")),
    ),
    hr(),
    ## fichero histórico ----
    fluidRow(
      h3("Archivo spi metadatos"),
      column(4,
             tabsetPanel(id = "tsspimeta",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pispimeta",
                                    label = "Archivo spi meta",
                                    choices = c("CU_53_05_02_02_spi_metadata.csv"),
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
             dataTableOutput("tspimeta")),
    ),
    hr(),
    fluidRow(
      h3("Paises geo"),
      column(4,
             tabsetPanel(id = "tsspaisgeo",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "pipaisesgeo",
                                    label = "Paises geo",
                                    choices = c("CU_53_05_03_paisesgeo.json"),
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
             dataTableOutput("tspaises")),
    ),
    hr(),
    fluidRow(
      h3("Archivo inversiones paises"),
      column(4,
             tabsetPanel(id = "tssinversiones",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piinversiones",
                                    label = "Archivo inversiones",
                                    choices = c("ESCENARIO_INVERSIONES_PAISES.csv"),
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
             dataTableOutput("tsinversiones")),
    ),
    hr(),


  fluidRow(
      h3("Archivo inversiones CM"),
      column(4,
             tabsetPanel(id = "tssinversionescm",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piinversionescm",
                                    label = "Archivo inversiones",
                                    choices = c("ESCENARIO_INVERSIONES_REGION.csv"),
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
             dataTableOutput("tsinversionescm")),
    ),
    hr(),

    fluidRow(
      h3("Archivo inversiones CM detallado"),
      column(4,
             tabsetPanel(id = "tssinversionescmdetail",
                         tabPanel("Fichero de ejemplo",
                                  pickerInput(
                                    inputId = "piinversionescmdetail",
                                    label = "Archivo inversiones detallado",
                                    choices = c("CU_53_05_05_inversiones_cm.csv"),
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
             dataTableOutput("tsinversionescmdetail")),
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

  dfspi <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pispi != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pispi)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO

    } else{
      NA
    }
  })

  dftsspimeta <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pispimeta != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pispimeta)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO

    } else{
      NA
    }
  })

  dfinversiones <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piinversiones != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piinversiones)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO

    } else{
      NA
    }
  })

  dfinversionescm <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piinversionescm != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piinversionescm)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO

    } else{
      NA
    }
  })


  dfinversionescmdetail <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$piinversionescmdetail != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$piinversionescmdetail)
      read_csv(file_name, show_col_types = FALSE)
    } else if (FALSE){ ## AÑADIR POSIBILIDAD DE FICHERO

    } else{
      NA
    }
  })

  dftspaises <- reactive({
    if(!is.list(carpetas())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No se ha encontrado alguna carpeta necesaria para el caso",
        type = "error"
      )
      stopApp()
    }
    if(input$pipaisesgeo != ""){
      file_name <- paste0(carpetas()$carpeta_entrada, "/", input$pipaisesgeo)
      st_read(file_name, quiet = TRUE)
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
    cond <- is.na(list(dfspi(),
                       dftsspimeta(),
                       dfinversiones(),
                       dftspaises(),
                       dfinversionescm(),
                       dfinversionescmdetail()))
    if(any(cond)){
      sendSweetAlert(
        session = session,
        title = "No se pudo completar",
        text = "No se han seleccionado todos los ficheros de datos requeridos",
        type = "warning"
      )

    } else{
      ## Guardar archivos de este caso
      st_write(obj = dftspaises(),
               dsn = paste0(carpetas()$carpeta_salida, "/PAISES.json"),
               driver = "GeoJSON",
               delete_dsn = TRUE)
      write_csv(dfspi(), paste0(carpetas()$carpeta_salida, "/SPI.csv"))
      write_csv(dftsspimeta(), paste0(carpetas()$carpeta_salida, "/SPI_META.csv"))
      write_csv(dfinversiones(), paste0(carpetas()$carpeta_salida, "/INVERSIONES_PAISES.csv"))
      write_csv(dfinversionescm(), paste0(carpetas()$carpeta_salida, "/INVERSIONES_REGION.csv"))
      write_csv(dfinversionescm(), paste0(carpetas()$carpeta_salida, "/INVERSIONES_REGION_DETAIL.csv"))

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
  output$tspi <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$pispi)
    datatable(dfspi(),
              options = list(scrollX = TRUE))
  })

  output$tspimeta <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$pispimeta)
    datatable(dftsspimeta(),
              options = list(scrollX = TRUE))
  })

  output$tindicadoresmeta <- renderDataTable({
    # req(carpetas_sesion$carpeta_entrada)
    req(input$piindicadoresmeta)
    datatable(dfinversiones(),
              options = list(paging = FALSE))
  })

  output$tsinversiones <- renderDataTable({
    req(input$piinversiones)
    datatable(dfinversiones())
  })

  output$tsinversionescm <- renderDataTable({
    req(input$piinversionescm)
    datatable(dfinversionescm())
  })

  output$tsinversionescmdetail <- renderDataTable({
    req(input$piinversionescmdetail)
    datatable(dfinversionescmdetail())
  })

  output$tspaises <- renderDataTable({
    req(input$pipaisesgeo)
    datatable(dftspaises(), options = list(autoWidth = TRUE))
  })

  output$tnuevacampana <- renderDataTable({
    req(input$pinuevacampana)
    datatable(dfnuevacampana())
  })
  output$tnuevacapacidad <- renderDataTable({
    req(input$pinuevacapacidad)
    datatable(dfnuevacapacidad())
  })

}

shinyApp(ui, server, enableBookmarking = "url")


