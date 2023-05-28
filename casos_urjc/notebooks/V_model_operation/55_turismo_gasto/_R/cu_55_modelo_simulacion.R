setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/55_turismo_gasto/")

## simulación turistas

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

escenario <- read_csv("cu_55_step_01_input/ESCENARIO_DESTINO.csv")

# pfutbol <- sum(escenario$Futbol == 1) / nrow(escenario)
# rate_nservicios <- mean(escenario$nservicios)
# rate_capacidad <- mean(escenario$capacidad)
# m_cont <- apply(escenario[,4:25], 2, mean)
# s_cont <- apply(escenario[,4:25], 2, sd)

## de la configuración
nsim <- 100

simulacion <- escenario |> 
  mutate(sim_turistas = list(rpois(nsim, turistas))) |> 
  unnest(sim_turistas) |> 
  select(-turistas)
  
simulacion.x <- simulacion |> 
  mutate(nmes = factor(str_sub(mes, 6, 7), levels = levels(dm$nmes)),
         pais_orig = factor(pais_orig, levels = levels(dm$pais_orig))) |> 
  select(nmes, pais_orig, turistas = sim_turistas) |> 
  model.matrix(~., data = _)

modelo <- read_rds("cu_55_maestros/modelo_xgb.rds")
predict(modelo, simulacion.x)




