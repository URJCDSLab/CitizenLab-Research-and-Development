## PROCESO PREDICCIÓN Y SIMULACIÓN MASIVA

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/25_listas_espera/")

library(readr)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(modeltime)
library(simmer)
library(simmer.plot)
library(tidyr)

params_sim <- function(s, periodo, h){
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
    pred_h_personas <- modelo_pacientes |> 
      pluck(s) |>
      modeltime_forecast(h = h) |> 
      mutate(id = "personas")

    pacientes_en_cola <- pred_h_personas |> 
      slice_tail(n = 1) |> pull(.value)
    
    pred_h_tiempo <- modelo_tiempo |> 
      pluck(s) |>
      modeltime_forecast(h = h) |> 
      mutate(id = "tiempo")
    
    tiempo_medio_en_cola <- pred_h_tiempo |> 
      slice_tail(n = 1) |> pull(.value)
    }
  
  lambda <- pacientes_en_cola / tiempo_medio_en_cola
  mu <- lambda/pacientes_en_cola
  
  return(list(pacientes_en_cola = pacientes_en_cola,
              tiempo_medio_en_cola = tiempo_medio_en_cola,
              lambda = lambda,
              mu = mu,
              pred_h_tiempo = pred_h_tiempo,
              pred_h_personas = pred_h_personas))
}

modelo_pacientes <- read_rds("cu_25_maestros/modelos_pacientes_xgboost.rds")
modelo_tiempo <- read_rds("cu_25_maestros/modelos_tiempo_xgboost.rds")

capacidad <- read_csv("cu_25_step_01_input/CU_25_05_07_01_capacidad.csv")

df <- data.frame(s = names(modelo_pacientes)) |> 
  tidyr::separate(s, c("nombre_area", "Especialidad"), "\\.", remove = FALSE) |> 
  left_join(capacidad)

## COGER DE VARIABLES !!
NPER <- 365
h <- 2

#simulación
l_sim_ultimo <- df |> 
  slice(1:2) |>
  pull(s) |> 
  map(~{
    pars <- params_sim(.x, periodo = "ultimo")
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
  slice(1:2) |>
  pull(s) |> 
  map(~{
    pars <- params_sim(.x, periodo = "pred", h = h)
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



