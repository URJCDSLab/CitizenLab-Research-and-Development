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
    
    titlePanel(title = "SIMULACION 05 - CitizenLab CU 25"),

    # ... Otros elementos de la UI
    
    sidebarLayout(
      sidebarPanel(
        h3("Guardar datos para el siguiente paso"),
        actionBttn("abguardar",
                   "Guardar datos",
                   size = "md",
                   icon = icon("floppy-disk")),
        br(), br(),
        
        uiOutput("uiespecialidad"),
        uiOutput("uizona"),
        uiOutput("uihorizonte")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Capacidad", 
                   h1("Capacidad"),
                   tableOutput("capacidad_table")
          ),
          tabPanel("Simulaciones",
                   h1("Simulaciones"),
                   tableOutput("simus")
          ),
          tabPanel("Visualizaciones",
                   fluidRow(
                     column(width = 6,
                            h1("Visualización Simulaciones"),
                            plotOutput("simus_visu1")
                     ),
                     column(width = 6,
                            h1("Visualización Simulaciones"),
                            plotOutput("simus_visu2")
                     )
                   ),
                   fluidRow(
                     column(width = 12,
                            h1("Visualización Simulaciones"),
                            plotOutput("simus_visu3")
                     )
                   )
          )
        )
      )
    )
  )
}


server <- function(input, output, session) {
  
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


  # Calcular y mostrar la capacidad seleccionada
  output$capacidad_table <- renderTable({
      capacidad_seleccionada <- dfcapacidad() %>%
        filter(Especialidad == input$especialidad, nombre_area == input$zona)
      capacidad_seleccionada
    
  })


  # Calcular y mostrar la capacidad seleccionada
output$simus_visu3 <- renderPlot({
modelo_pacientes <- modelos_xgboost_pacientes()
modelo_tiempo <- modelos_xgboost()



capacidad <- dfcapacidad()


e <- input$especialidad
z <- input$zona
NPER <- dfvariables() |> 
  filter(variable == "NPER") |> 
  pull(valor)  
h <- dfvariables() |> 
  filter(variable == "HORIZONTE") |> 
  pull(valor)

s <- paste(z, e, sep = ".")


cap <- capacidad |> 
  filter(nombre_area == z,
         Especialidad == e) |> 
  pull(capacidad)


  if (input$horizonte == 0){
      ## Si se elige último
pacientes_en_cola <- modelo_pacientes |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)

tiempo_medio_en_cola <- modelo_tiempo |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)


  }else{
    h = as.integer(input$horizonte)
    ## Si se elige predicción 

  pacientes_en_cola <- modelo_pacientes |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    slice_tail(n = 1) |> pull(.value)

  tiempo_medio_en_cola <- modelo_tiempo |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    slice_tail(n = 1) |> pull(.value)

  }



## Parámetros
## Llegadas

lambda <- pacientes_en_cola / tiempo_medio_en_cola

## Tiempo de servicio
mu <- lambda/pacientes_en_cola

## Capacidad es cap



library(simmer)
#set.seed(1) no usar en la app para que salgan cosas distintas al cambiar

env <- simmer("listasSim")
# env

paciente <- trajectory("Trayectoria del paciente") %>%
  ## Operación
  seize("quirofano", 1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("quirofano", 1)

env %>%
  add_resource("quirofano", cap) %>%
  add_generator("inicial", paciente, at(rep(0, pacientes_en_cola))) |> 
  add_generator("paciente", paciente, function() rpois(1, lambda))


env %>% 
  run(NPER)

# env %>% peek(3)

#Esto de momento no
# library(parallel)
# envs <- mclapply(1:100, function(i) {
#   simmer("listassim100") %>%
#     add_resource("quirofano", cap) %>%
#     add_generator("inicial", paciente, at(rep(0, pacientes_en_cola))) |> 
#     add_generator("paciente", paciente, function() rpois(1, lambda)) |> 
#     run(NPER) %>%
#     wrap()
# })
# 
# resources <- get_mon_resources(envs)
# plot(resources, metric = "utilization")
# plot(resources, metric = "usage", c("quirofano"))


## Mostrar con datatable
recursos <- get_mon_resources(env)
recursos

llegadas <- get_mon_arrivals(env, ongoing = TRUE)
llegadas

    plot(recursos, metric = "utilization")
  })


  # Calcular y mostrar la capacidad seleccionada
output$simus <- renderTable({
modelo_pacientes <- modelos_xgboost_pacientes()
modelo_tiempo <- modelos_xgboost()



capacidad <- dfcapacidad()


e <- input$especialidad
z <- input$zona
NPER <- dfvariables() |> 
  filter(variable == "NPER") |> 
  pull(valor)  
h <- dfvariables() |> 
  filter(variable == "HORIZONTE") |> 
  pull(valor)

s <- paste(z, e, sep = ".")


cap <- capacidad |> 
  filter(nombre_area == z,
         Especialidad == e) |> 
  pull(capacidad)


  if (input$horizonte == 0){
      ## Si se elige último
pacientes_en_cola <- modelo_pacientes |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)

tiempo_medio_en_cola <- modelo_tiempo |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)


  }else{
    h = as.integer(input$horizonte)
    ## Si se elige predicción 

  pacientes_en_cola <- modelo_pacientes |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    slice_tail(n = 1) |> pull(.value)

  tiempo_medio_en_cola <- modelo_tiempo |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    slice_tail(n = 1) |> pull(.value)

  }



## Parámetros
## Llegadas

lambda <- pacientes_en_cola / tiempo_medio_en_cola

## Tiempo de servicio
mu <- lambda/pacientes_en_cola

## Capacidad es cap



library(simmer)
#set.seed(1) no usar en la app para que salgan cosas distintas al cambiar

env <- simmer("listasSim")
# env

paciente <- trajectory("Trayectoria del paciente") %>%
  ## Operación
  seize("quirofano", 1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("quirofano", 1)

env %>%
  add_resource("quirofano", cap) %>%
  add_generator("inicial", paciente, at(rep(0, pacientes_en_cola))) |> 
  add_generator("paciente", paciente, function() rpois(1, lambda))


env %>% 
  run(NPER)

# env %>% peek(3)

#Esto de momento no
# library(parallel)
# envs <- mclapply(1:100, function(i) {
#   simmer("listassim100") %>%
#     add_resource("quirofano", cap) %>%
#     add_generator("inicial", paciente, at(rep(0, pacientes_en_cola))) |> 
#     add_generator("paciente", paciente, function() rpois(1, lambda)) |> 
#     run(NPER) %>%
#     wrap()
# })
# 
# resources <- get_mon_resources(envs)
# plot(resources, metric = "utilization")
# plot(resources, metric = "usage", c("quirofano"))


## Mostrar con datatable
recursos <- get_mon_resources(env)
recursos

llegadas <- get_mon_arrivals(env, ongoing = TRUE)
write_csv(llegadas, paste0(carpetas()$carpeta_salida, "/SIMULACIONES_LLEGADAS.csv"))
llegadas

  })


  # Calcular y mostrar la capacidad seleccionada
output$simus_visu2 <- renderPlot({
modelo_pacientes <- modelos_xgboost_pacientes()
modelo_tiempo <- modelos_xgboost()



capacidad <- dfcapacidad()


e <- input$especialidad
z <- input$zona
NPER <- dfvariables() |> 
  filter(variable == "NPER") |> 
  pull(valor)  
h <- dfvariables() |> 
  filter(variable == "HORIZONTE") |> 
  pull(valor)

s <- paste(z, e, sep = ".")


cap <- capacidad |> 
  filter(nombre_area == z,
         Especialidad == e) |> 
  pull(capacidad)


  if (input$horizonte == 0){
      ## Si se elige último
pacientes_en_cola <- modelo_pacientes |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)

tiempo_medio_en_cola <- modelo_tiempo |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)


  }else{
    h = as.integer(input$horizonte)
    ## Si se elige predicción 

  pacientes_en_cola <- modelo_pacientes |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    slice_tail(n = 1) |> pull(.value)

  tiempo_medio_en_cola <- modelo_tiempo |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    slice_tail(n = 1) |> pull(.value)

  }



## Parámetros
## Llegadas

lambda <- pacientes_en_cola / tiempo_medio_en_cola

## Tiempo de servicio
mu <- lambda/pacientes_en_cola

## Capacidad es cap



library(simmer)
#set.seed(1) no usar en la app para que salgan cosas distintas al cambiar

env <- simmer("listasSim")
# env

paciente <- trajectory("Trayectoria del paciente") %>%
  ## Operación
  seize("quirofano", 1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("quirofano", 1)

env %>%
  add_resource("quirofano", cap) %>%
  add_generator("inicial", paciente, at(rep(0, pacientes_en_cola))) |> 
  add_generator("paciente", paciente, function() rpois(1, lambda))


env %>% 
  run(NPER)

# env %>% peek(3)

#Esto de momento no
# library(parallel)
# envs <- mclapply(1:100, function(i) {
#   simmer("listassim100") %>%
#     add_resource("quirofano", cap) %>%
#     add_generator("inicial", paciente, at(rep(0, pacientes_en_cola))) |> 
#     add_generator("paciente", paciente, function() rpois(1, lambda)) |> 
#     run(NPER) %>%
#     wrap()
# })
# 
# resources <- get_mon_resources(envs)
# plot(resources, metric = "utilization")
# plot(resources, metric = "usage", c("quirofano"))


## Mostrar con datatable
recursos <- get_mon_resources(env)
recursos

llegadas <- get_mon_arrivals(env, ongoing = TRUE)
llegadas
    plot(recursos, metric = "usage", c("quirofano"))
  })


  # Calcular y mostrar la capacidad seleccionada
output$simus_visu1 <- renderPlot({
modelo_pacientes <- modelos_xgboost_pacientes()
modelo_tiempo <- modelos_xgboost()



capacidad <- dfcapacidad()


e <- input$especialidad
z <- input$zona
NPER <- dfvariables() |> 
  filter(variable == "NPER") |> 
  pull(valor)  
h <- dfvariables() |> 
  filter(variable == "HORIZONTE") |> 
  pull(valor)

s <- paste(z, e, sep = ".")


cap <- capacidad |> 
  filter(nombre_area == z,
         Especialidad == e) |> 
  pull(capacidad)


  if (input$horizonte == 0){
      ## Si se elige último
pacientes_en_cola <- modelo_pacientes |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)

tiempo_medio_en_cola <- modelo_tiempo |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)


  }else{
    h = as.integer(input$horizonte)
    ## Si se elige predicción 

  pacientes_en_cola <- modelo_pacientes |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    slice_tail(n = 1) |> pull(.value)

  tiempo_medio_en_cola <- modelo_tiempo |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    slice_tail(n = 1) |> pull(.value)

  }



## Parámetros
## Llegadas

lambda <- pacientes_en_cola / tiempo_medio_en_cola

## Tiempo de servicio
mu <- lambda/pacientes_en_cola

## Capacidad es cap



library(simmer)
#set.seed(1) no usar en la app para que salgan cosas distintas al cambiar

env <- simmer("listasSim")
# env

paciente <- trajectory("Trayectoria del paciente") %>%
  ## Operación
  seize("quirofano", 1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("quirofano", 1)

env %>%
  add_resource("quirofano", cap) %>%
  add_generator("inicial", paciente, at(rep(0, pacientes_en_cola))) |> 
  add_generator("paciente", paciente, function() rpois(1, lambda))


env %>% 
  run(NPER)

# env %>% peek(3)

#Esto de momento no
# library(parallel)
# envs <- mclapply(1:100, function(i) {
#   simmer("listassim100") %>%
#     add_resource("quirofano", cap) %>%
#     add_generator("inicial", paciente, at(rep(0, pacientes_en_cola))) |> 
#     add_generator("paciente", paciente, function() rpois(1, lambda)) |> 
#     run(NPER) %>%
#     wrap()
# })
# 
# resources <- get_mon_resources(envs)
# plot(resources, metric = "utilization")
# plot(resources, metric = "usage", c("quirofano"))


## Mostrar con datatable
recursos <- get_mon_resources(env)
recursos

llegadas <- get_mon_arrivals(env, ongoing = TRUE)
plot(llegadas)


    
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

  modelos_xgboost <- reactive({
    read_rds(paste0(carpetas()$carpeta_maestros, 
                    "/modelos_tiempo_xgboost.rds"))
  })

    modelos_xgboost_pacientes <- reactive({
    read_rds(paste0(carpetas()$carpeta_maestros, 
                    "/modelos_pacientes_xgboost.rds"))
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