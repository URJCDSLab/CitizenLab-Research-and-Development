## PROCESO PREDICCIÓN Y SIMULACIÓN MASIVA


library(readr)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(modeltime)
library(simmer)
library(simmer.plot)
library(tidyr)

## EN CASO DE QUE LA PREDICCIÓN AL FINAL DEL HORIZONTE RESULTE EN UN NÚMERO NEGATIVO, SE TOMA EL
## ÚLTIMO VALOR POSITIVO (O LO QUE ES LO MISMO, SE ELIMINAN LOS NEGATIVOS)

#s especialidad y zona
#periodo
#h horizonte para predicción serie
params_sim <- function(s, periodo, h){
  pred_h_personas <- modelo_pacientes |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    mutate(id = "personas")
  
  pred_h_tiempo <- modelo_tiempo |> 
    pluck(s) |>
    modeltime_forecast(h = h) |> 
    mutate(id = "tiempo")
  
  if(periodo == "ultimo"){
    
    pacientes_en_cola <- modelo_pacientes |> 
      pluck(s, ".calibration_data", 1) |> 
      slice_max(fecha) |> 
      pull(.actual)
    
    tiempo_medio_en_cola <- modelo_tiempo |> 
      pluck(s, ".calibration_data", 1) |> 
      slice_max(fecha) |> 
      pull(.actual)
    
    
    
  } else{
    
    pacientes_en_cola <- pred_h_personas |> 
      filter(.value > 0) |> 
      slice_tail(n = 1) |> pull(.value)
    
    tiempo_medio_en_cola <- pred_h_tiempo |> 
      filter(.value > 0) |> 
      slice_tail(n = 1) |> pull(.value)
  }
  
  if(pacientes_en_cola > 0 & tiempo_medio_en_cola > 0){
    lambda <- pacientes_en_cola / tiempo_medio_en_cola
    mu <- lambda/pacientes_en_cola
  } else{
    lambda <- mu <- NA
  }
  
  return(list(pacientes_en_cola = pacientes_en_cola,
              tiempo_medio_en_cola = tiempo_medio_en_cola,
              lambda = lambda,
              mu = mu,
              pred_h_tiempo = pred_h_tiempo,
              pred_h_personas = pred_h_personas))
}


## Datos y modelos

modelo_pacientes <- read_rds("cu_25_maestros/modelos_pacientes_xgboost.rds")
modelo_tiempo <- read_rds("cu_25_maestros/modelos_tiempo_xgboost.rds")

capacidad <- read_csv("cu_25_step_01_input/CU_25_05_07_01_capacidad.csv")

variables <- read_csv("cu_25_step_01_input/VARIABLES.csv") 

df <- data.frame(s = names(modelo_pacientes)) |> 
  tidyr::separate(s, c("nombre_area", "Especialidad"), "\\.", remove = FALSE) |> 
  left_join(capacidad)



## COGER DE VARIABLES !!
H <- variables |> 
  filter(variable == "HORIZONTE") |> 
  pull(valor) |> 
  as.numeric()

# H <- 2
NPER <- variables |> 
  filter(variable == "NPER") |> 
  pull(valor)

#simulación
l_sim_ultimo <- df |> 
  # slice(1:2) |>
  pull(s) |> 
  map(~{
    pars <- params_sim(s = .x, periodo = "ultimo", h = H)
    cap <- df |> filter(s == .x) |> 
      pull(capacidad)
    env <- simmer("listasSim")
    
    paciente <- trajectory("Trayectoria del paciente") %>%
      ## Operación
      seize("quirofano", 1) %>%
      timeout(function() rexp(1, pars$mu)) %>%
      release("quirofano", 1)
    
    env %>%
      add_resource("quirofano", cap) %>%
      add_generator("inicial", paciente, at(rep(0, pars$pacientes_en_cola))) |>
      add_generator("paciente", paciente, function() rpois(1, pars$lambda))
    
    env %>%
      run(NPER)
    
    recursos <- get_mon_resources(env)
    llegadas <- get_mon_arrivals(env, ongoing = TRUE)
    return(list(recursos = recursos |> mutate(id = .x),
                llegadas = llegadas |> mutate(id = .x)))
    
  }) 
l_sim_h <- df |> 
  # slice(1:2) |>
  pull(s) |> 
  map(~{
    pars <- params_sim(.x, periodo = "pred", h = H)
    cap <- df |> filter(s == .x) |> 
      pull(capacidad)
    env <- simmer("listasSim")
    
    paciente <- trajectory("Trayectoria del paciente") %>%
      ## Operación
      seize("quirofano", 1) %>%
      timeout(function() rexp(1, pars$mu)) %>%
      release("quirofano", 1)
    
    env %>%
      add_resource("quirofano", cap) %>%
      add_generator("inicial", paciente, at(rep(0, pars$pacientes_en_cola))) |>
      add_generator("paciente", paciente, function() rpois(1, pars$lambda))
    
    env %>%
      run(NPER)
    
    recursos <- get_mon_resources(env)
    llegadas <- get_mon_arrivals(env, ongoing = TRUE)
    return(list(recursos = recursos |> mutate(id = .x),
                llegadas = llegadas |> mutate(id = .x),
                pred_h_personas = pars$pred_h_personas,
                pred_h_tiempo = pars$pred_h_tiempo))
    
  }) 




sim_ultimo_recursos <- l_sim_ultimo |> map_dfr(~.x |> pluck("recursos"))
sim_ultimo_llegadas <- l_sim_ultimo |> map_dfr(~.x |> pluck("llegadas"))

sim_h_recursos <- l_sim_h |> map_dfr(~.x |> pluck("recursos"))
sim_h_llegadas <- l_sim_h |> map_dfr(~.x |> pluck("llegadas"))

pred_h_tiempo <- l_sim_h |> map_dfr(~.x |> pluck("pred_h_tiempo"))
pred_h_personas <- l_sim_h |> map_dfr(~.x |> pluck("pred_h_personas"))


write_rds(sim_ultimo_recursos, "cu_25_step_06_output/sim_ultimo_recursos.rds")
write_rds(sim_ultimo_llegadas, "cu_25_step_06_output/sim_ultimo_llegadas.rds")
write_rds(sim_h_recursos, "cu_25_step_06_output/sim_h_recursos.rds")
write_rds(pred_h_tiempo, "cu_25_step_06_output/pred_h_tiempo.rds")
write_rds(pred_h_personas, "cu_25_step_06_output/pred_h_personas.rds")