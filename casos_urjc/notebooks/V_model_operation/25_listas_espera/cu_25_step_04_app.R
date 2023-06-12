########
# APP PASO 4 (PROYECCIÓN)
########

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

## Config

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

ui <- function(request) {
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    useShinydashboard(),
    useWaiter(),
    
    titlePanel(title = "Proyección - CitizenLab CU 25"),

    # ... Otros elementos de la UI
    
    sidebarLayout(
      sidebarPanel(
        h3("Guardar datos para el siguiente paso"),
        actionBttn("abguardar",
                   "Guardar datos",
                   size = "md",
                   icon = icon("floppy-disk")),
        br(), br(),
        
        column(width = 6,
               uiOutput("uihorizonte")
        ),
        column(width = 6,
               uiOutput("uiespecialidad")
        ),
        column(width = 6,
               uiOutput("uizona")
        ),
        column(width = 6,
               uiOutput("uiparametro")
        )
      ),
      
      mainPanel(
        tabsetPanel(
                    tabPanel("Tabla",
                   column(width = 12,
                          tableOutput("tabla_preds")
                   )
          ),
          tabPanel("Visualización",
                   column(width = 12,
                          plotlyOutput("serieTemporalPlot")
                   )
          )
        )
      )
    )
  )
}


server <- function(input, output, session) {
  

  
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
  
    # Resto del código del servidor...
  
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
  
  # Cargar modelos Prophet/XGBoost

  modelos_xgboost <- reactive({
    read_rds(paste0(carpetas()$carpeta_maestros, 
                    "/modelos_tiempo_xgboost.rds"))
  })

  
  output$uihorizonte <- renderUI({
  selectInput(
    inputId = "horizonte",
    label = "Horizonte temporal (semanas)",
    choices = seq(1, 52),
    selected = dfvariables()$valor[dfvariables()$variable == "HORIZONTE"]
  )
})

  output$uizona <- renderUI({
  selectInput(
    inputId = "zona",
    label = "Zona",
    choices = c("Centro-Norte","Centro-Oeste","Este","Norte","Oeste","Sur I","Sur Ii","Sur-Este","Sur-Oeste I","Sur-Oeste Ii"),
    selected = "Centro-Norte"
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
  

  output$uiparametro <- renderUI({
    selectInput(
      inputId = "parametro",
      label = "Parámetro a predecir",
      choices = c("media_tiempo_dias"),
      selected = "media_tiempo_dias"
    )
  })



  output$serieTemporalPlot <- renderPlotly({

  indicadores <- dfindicadores()
  capacidad <- dfcapacidad()
  lista <- dfhistorico()
  hospitales <- dfhospitales()

lista <- lista |> 
  mutate(fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))

h <- "HOSPITAL UNIVERSITARIO LA PAZ"
a <- "05"
e <- "Angiología y Cirugía Vascular"

## Valores perdidos: fill (solo hay una semana)

lzona_esp_1 <- lista |> 
  left_join(hospitales) |> 
  filter(Especialidad == e,
         id_area == a) |> 
  fill(total_pacientes, media_tiempo_dias) |> 
  group_by(nombre_area, Especialidad, fecha) |> 
  summarise(total_pacientes = sum(total_pacientes, na.rm = TRUE),
            media_tiempo_dias = mean(media_tiempo_dias, na.rm = TRUE)) 

lzona_esp_1 |> plot_time_series(fecha, total_pacientes)

## XBGoost con series temporales

## División conjuntos de datos
splits <- lzona_esp_1 %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

## Visualización
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(fecha, total_pacientes, .interactive = FALSE)

## Tidymodels workflow

recipe_spec <- recipe(total_pacientes ~ fecha, training(splits)) %>%
  step_timeseries_signature(fecha) %>%
  step_rm(
    # contains("am.pm"), contains("hour"), contains("minute"),
    #       contains("second"),
    contains("week"),
    contains("xts")) %>%
  step_fourier(fecha, period = 365, K = 5) %>%
  step_dummy(all_nominal())  |>
  step_zv()

recipe_spec %>% prep() %>% juice()

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(fecha)) %>%
  fit(training(splits))


model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE,
                                          seasonality_weekly = TRUE,
                                          seasonality_daily = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost

model_table <- modeltime_table(
  # model_fit_arima, 
  # model_fit_prophet,
  workflow_fit_glmnet,
  # workflow_fit_rf,
  workflow_fit_prophet_boost
) 
model_table

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table

calibration_table %>%
  modeltime_forecast(actual_data = lzona_esp_1) %>%
  plot_modeltime_forecast(.interactive = FALSE)

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


## Refit and forecast
calibration_table |> 
  filter(.model_id == 2) |> 
  modeltime_refit(lzona_esp_1) %>%
  modeltime_forecast(h = "8 weeks", actual_data = lzona_esp_1) %>%
  plot_modeltime_forecast(.interactive = FALSE)






## Ajustar todos los modelos a lo bestia

lzona_esp <- lista |> 
  left_join(hospitales) |> 
  # filter(Especialidad == e,
  #        id_area == a) |> 
  fill(total_pacientes, media_tiempo_dias) |> 
  group_by(nombre_area, Especialidad, fecha) |> 
  summarise(total_pacientes = sum(total_pacientes, na.rm = TRUE),
            media_tiempo_dias = mean(media_tiempo_dias, na.rm = TRUE)) 

dfs <- split(lzona_esp, ~nombre_area + Especialidad)

res_tiempo <- modelos_xgboost()


  a <- paste0(input$zona,".",input$especialidad)
  h = as.integer(input$horizonte)

  print(a)
  print(h)
  print(res_tiempo[[a]])
  ## Predicción
  prediccion <- res_tiempo[[a]] |>
    modeltime_forecast(h = h , actual_data = dfs[[a]])
  print(prediccion)
  print("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
  ## Visualización
  dfs[[a]] |> 
    plot_time_series(fecha, total_pacientes)
  prediccion |> plot_modeltime_forecast()
  # write_csv(prediccion, paste0(carpetas()$carpeta_salida, "/PREDICCIONES.csv"))


  })
  



    output$tabla_preds <- renderTable({

  indicadores <- dfindicadores()
  capacidad <- dfcapacidad()
  lista <- dfhistorico()
  hospitales <- dfhospitales()

lista <- lista |> 
  mutate(fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))

h <- "HOSPITAL UNIVERSITARIO LA PAZ"
a <- "05"
e <- "Angiología y Cirugía Vascular"

## Valores perdidos: fill (solo hay una semana)

lzona_esp_1 <- lista |> 
  left_join(hospitales) |> 
  filter(Especialidad == e,
         id_area == a) |> 
  fill(total_pacientes, media_tiempo_dias) |> 
  group_by(nombre_area, Especialidad, fecha) |> 
  summarise(total_pacientes = sum(total_pacientes, na.rm = TRUE),
            media_tiempo_dias = mean(media_tiempo_dias, na.rm = TRUE)) 

lzona_esp_1 |> plot_time_series(fecha, total_pacientes)

## XBGoost con series temporales

## División conjuntos de datos
splits <- lzona_esp_1 %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

## Visualización
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(fecha, total_pacientes, .interactive = FALSE)

## Tidymodels workflow

recipe_spec <- recipe(total_pacientes ~ fecha, training(splits)) %>%
  step_timeseries_signature(fecha) %>%
  step_rm(
    # contains("am.pm"), contains("hour"), contains("minute"),
    #       contains("second"),
    contains("week"),
    contains("xts")) %>%
  step_fourier(fecha, period = 365, K = 5) %>%
  step_dummy(all_nominal())  |>
  step_zv()

recipe_spec %>% prep() %>% juice()

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(fecha)) %>%
  fit(training(splits))


model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE,
                                          seasonality_weekly = TRUE,
                                          seasonality_daily = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost

model_table <- modeltime_table(
  # model_fit_arima, 
  # model_fit_prophet,
  workflow_fit_glmnet,
  # workflow_fit_rf,
  workflow_fit_prophet_boost
) 
model_table

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table

calibration_table %>%
  modeltime_forecast(actual_data = lzona_esp_1) %>%
  plot_modeltime_forecast(.interactive = FALSE)

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


## Refit and forecast
calibration_table |> 
  filter(.model_id == 2) |> 
  modeltime_refit(lzona_esp_1) %>%
  modeltime_forecast(h = "8 weeks", actual_data = lzona_esp_1) %>%
  plot_modeltime_forecast(.interactive = FALSE)






## Ajustar todos los modelos a lo bestia

lzona_esp <- lista |> 
  left_join(hospitales) |> 
  # filter(Especialidad == e,
  #        id_area == a) |> 
  fill(total_pacientes, media_tiempo_dias) |> 
  group_by(nombre_area, Especialidad, fecha) |> 
  summarise(total_pacientes = sum(total_pacientes, na.rm = TRUE),
            media_tiempo_dias = mean(media_tiempo_dias, na.rm = TRUE)) 

dfs <- split(lzona_esp, ~nombre_area + Especialidad)

res_tiempo <- modelos_xgboost()


  a <- paste0(input$zona,".",input$especialidad)
  h = as.integer(input$horizonte)

  print(a)
  print(h)
  print(res_tiempo[[a]])
  ## Predicción
  prediccion <- res_tiempo[[a]] |>
    modeltime_forecast(h = h , actual_data = dfs[[a]])
  })
  

    observeEvent(input$abguardar, {


    ## Copiar resto input a output para siguientes pasos
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
        file.copy(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
          paste0(carpetas()$carpeta_salida, "/VARIABLES.csv"))

    

    sendSweetAlert(
      session = session,
      title = "¡¡ Éxito !!",
      text = "Se han guardado los ficheros para el siguiente paso del caso.",
      type = "success"
    )
  })

}

shinyApp(ui, server)
