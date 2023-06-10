#https://r-simmer.org/articles/simmer-06-queueing.html

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/25_listas_espera/")

library(tidyverse)
library(tidymodels)
library(lubridate)
library(modeltime)
library(simmer)
library(simmer.plot)

modelo_pacientes <- read_rds("cu_25_maestros/modelos_pacientes_xgboost.rds")
modelo_tiempo <- read_rds("cu_25_maestros/modelos_tiempo_xgboost.rds")



capacidad <- read_csv("cu_25_step_01_input/CU_25_05_07_01_capacidad.csv")

## Ejemplo base

# h <- "HOSPITAL UNIVERSITARIO LA PAZ"
# a <- "05"
e <- "Angiología y Cirugía Vascular"
z <- "Centro-Norte"

s <- paste(z, e, sep = ".")
h <- 2
cap <- capacidad |> 
  filter(nombre_area == z,
         Especialidad == e) |> 
  pull(capacidad)

NPER <- 365  
  
## Si se elige último
pacientes_en_cola <- modelo_pacientes |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)

tiempo_medio_en_cola <- modelo_tiempo |> 
  pluck(s, ".calibration_data", 1) |> 
  slice_max(fecha) |> 
  pull(.actual)

## Si se elige predicción 

pacientes_en_cola <- modelo_pacientes |> 
  pluck(s) |>
  modeltime_forecast(h = h) |> 
  slice_tail(n = 1) |> pull(.value)

tiempo_medio_en_cola <- modelo_tiempo |> 
  pluck(s) |>
  modeltime_forecast(h = h) |> 
  slice_tail(n = 1) |> pull(.value)


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

## Mostrar estos gráficos

plot(recursos, metric = "utilization")
plot(recursos, metric = "usage", c("quirofano"))

plot(llegadas)






