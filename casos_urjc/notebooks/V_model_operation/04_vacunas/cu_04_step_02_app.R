########
# APP PASO 2 (PARÁMETROS DE USUARIO) CU 04 (GESTIÓN VACUNAS GRIPE)
########

## Paquetes ----

## UI
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)


ui <- function(request){
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(title = "Parámetros de usuario - CitizenLab CU 04"),
    fluidRow(
      column(2,
             uiOutput("uirgbano"),
      ),
      column(2,
             uiOutput("uipizona"),
      ),
      column(2,
             uiOutput("uisinicio"),
      ),
      column(2,
             uiOutput("uisfin"),
      ),
      column(2,
             uiOutput("uipipredictor"),
      ),
      column(2,
             uiOutput("uisnivelcon"),
      )
      
    ),
    fluidRow(
      column(2,
             textOutput("textano")
      ),
      column(2,
             textOutput("textzona")
      ),
      column(2,
             textOutput("textsinicio")
      ),
      column(2,
             textOutput("textsfin")
      ),
      column(2,
             textOutput("textpredictor")
      ),
      column(2,
             textOutput("textnivelcon")
      )
      
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
  
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })
  
  dfhistorico <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/HISTORICO.csv"), 
             show_col_types = FALSE)
  })
  
  dfindicadoresmeta <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/INDICADORES_META.csv"), 
             show_col_types = FALSE)
  })
  
  dfindicadores <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/INDICADORES.csv"), 
             show_col_types = FALSE)
  })
  dfescucha <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/ESCUCHA.csv"), 
             show_col_types = FALSE)
  })
  
  dfpredictores <- reactive({
    data.frame(predictor = colnames(dfhistorico()),
               fichero = "Histórico") |> 
      bind_rows(data.frame(predictor = colnames(dfindicadores()),
                           fichero = "Indicadores")) |> 
      bind_rows(data.frame(predictor = colnames(dfescucha()),
                           fichero = "Escucha")) |> 
      left_join(dfindicadoresmeta(),
                by = c("predictor" = "Tabla")) |> 
      mutate(Indicador = if_else(is.na(Indicador), 
                                 predictor, 
                                 Indicador)) |> 
      filter(!(predictor %in% c("GEOCODIGO", "DESBDT", "ano", "semana", "n_vacunas")))
  })
  
  sfzonas <- reactive({
    read_sf(paste0(carpetas()$carpeta_entrada, "/ZONAS.json"))
  })
  
  observeEvent(input$abguardar, {
    ## Guardar variables en output
    VARIABLES <- dfvariables()
    VARIABLES$valor[VARIABLES$variable == "ANO"] <- input$rgbano
    VARIABLES$valor[VARIABLES$variable == "ZONA"] <- input$sizona
    VARIABLES$valor[VARIABLES$variable == "SEMANA_INICIO"] <- input$sinicio
    VARIABLES$valor[VARIABLES$variable == "SEMANA_FIN"] <- input$sfin
    VARIABLES$valor[VARIABLES$variable == "PREDICTOR"] <- input$sipredictor
    write_csv(VARIABLES, paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))
    st_write(obj = sfzonas(), 
             dsn = paste0(carpetas()$carpeta_salida, "/ZONAS.json"), 
             driver = "GeoJSON",
             delete_dsn = TRUE)
    write_csv(dfhistorico(), paste0(carpetas()$carpeta_salida, "/HISTORICO.csv"))
    write_csv(dfindicadores(), paste0(carpetas()$carpeta_salida, "/INDICADORES.csv"))
    write_csv(dfindicadoresmeta(), paste0(carpetas()$carpeta_salida, "/INDICADORES_META.csv"))
    write_csv(dfescucha(), paste0(carpetas()$carpeta_salida, "/ESCUCHA.csv"))
    ## Copiar resto input a output para siguientes pasos
    file.copy(paste0(carpetas()$carpeta_entrada, "/ESCENARIO.csv"),
              paste0(carpetas()$carpeta_salida, "/ESCENARIO.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/NUEVACAMPANA.csv"),
              paste0(carpetas()$carpeta_salida, "/NUEVACAMPANA.csv"))
    file.copy(paste0(carpetas()$carpeta_entrada, "/CAPACIDAD.csv"),
              paste0(carpetas()$carpeta_salida, "/CAPACIDAD.csv"))
    
    sendSweetAlert(
      session = session,
      title = "¡¡ Éxito !!",
      text = "Se han guardado los ficheros para el siguiente paso del caso.",
      type = "success"
    )
  })
  
  ## render outputs ----
  
  ## dynamic UI ----
  
  output$uirgbano <- renderUI({
    radioGroupButtons(
      inputId = "rgbano",
      label = "Campaña (año inicio)",
      choices = sort(unique(dfhistorico()$ano)),
      selected = dfvariables() |> filter(variable == "ANO") |> pull(valor),
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"))
    )
  })
  output$textano <- renderText({
    dfvariables() |> filter(variable == "ANO") |> pull(descripcion)
  })
  
  output$uipizona <- renderUI({
    selectizeInput("sizona",
                   label = "Zona de referencia",
                   choices = c(`Seleccione una (opcional)` = "", 
                               setNames(sfzonas()$GEOCODIGO,
                                        sfzonas()$DESBDT)),
                   selected = dfvariables() |> 
                     filter(variable == "ZONA") |> 
                     pull(valor),
                   options = list(
                     `live-search` = TRUE)
    )
  })
  
  output$textzona <- renderText({
    dfvariables() |> filter(variable == "ZONA") |> pull(descripcion)
  })
  
  
  output$uipipredictor <- renderUI({
    lista_pred <- list(`Seleccione uno (opcional)` = "", 
                       `Histórico` = setNames(dfpredictores() |> 
                                                filter(fichero == "Histórico") |> 
                                                pull(predictor),
                                              dfpredictores() |> 
                                                filter(fichero == "Histórico") |> 
                                                pull(Indicador)),
                       Indicadores = setNames(dfpredictores() |> 
                                                filter(fichero == "Indicadores") |> 
                                                pull(predictor),
                                              dfpredictores() |> 
                                                filter(fichero == "Indicadores") |> 
                                                pull(Indicador)),
                       Escucha = setNames(dfpredictores() |> 
                                            filter(fichero == "Escucha") |> 
                                            pull(predictor),
                                          dfpredictores() |> 
                                            filter(fichero == "Escucha") |> 
                                            pull(Indicador))
    )
    selectizeInput("sipredictor",
                   label = "Predictor",
                   choices = lista_pred,
                   selected = dfvariables() |> 
                     filter(variable == "PREDICTOR") |> 
                     pull(valor),
                   options = list(
                     `live-search` = TRUE
                   )
    )
  })
  
  output$textpredictor <- renderText({
    dfvariables() |> filter(variable == "PREDICTOR") |> pull(descripcion)
  })
  
  
  output$uisinicio <- renderUI({
    sliderTextInput("sinicio",
                    "Semana inicio campaña",
                    choices = seq(from = 25,
                                  to = 52,
                                  by = 1),
                    selected = dfvariables() |> 
                      filter(variable == "SEMANA_INICIO") |> 
                      pull(valor),
                    grid = TRUE)
  })
  
  output$textsinicio <- renderText({
    dfvariables() |> filter(variable == "SEMANA_INICIO") |> pull(descripcion)
  })
  
  output$uisfin <- renderUI({
    sliderTextInput("sfin",
                    "Semana fin campaña",
                    choices = seq(from = 1,
                                  to = 24,
                                  by = 1),
                    selected = dfvariables() |> 
                      filter(variable == "SEMANA_FIN") |> 
                      pull(valor),
                    grid = TRUE)
  })
  
  output$textsfin <- renderText({
    dfvariables() |> filter(variable == "SEMANA_FIN") |> pull(descripcion)
  })
  
  output$uisnivelcon <- renderUI(
    sliderTextInput("snivelcon",
                    "Nivel de confianza (%)",
                    choices = seq(from = 80,
                                  to = 99,
                                  by = 1),
                    selected = dfvariables() |> 
                      filter(variable == "NIVEL_CONFIANZA") |> 
                      pull(valor),
                    grid = TRUE)
  )
  
  output$textnivelcon <- renderText({
    dfvariables() |> filter(variable == "NIVEL_CONFIANZA") |> pull(descripcion)
  })
  
}

shinyApp(ui, server)