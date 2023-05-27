## Caso 45 regresión

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/45_turismo_patrones")


library(readr)
library(dplyr)
library(tidyr)
library(nnet)
library(janitor)
library(purrr)
library(effects)
# library(stringr)
# library(purrr)
# library(tibble)

cluster_anyos <- read_csv("cu_45_step_04_output/cluster_anyos.csv")
escenario <- read_csv("cu_45_step_01_input/ESCENARIO_REG.csv")

ANYO <- max(cluster_anyos$anyo)

options(contrasts = c("contr.sum", "contr.poly")) 
m <- cluster_anyos |>
  filter(anyo == ANYO) |> 
  select(-c(anyo, mun_dest)) |> 
  clean_names() |> 
  multinom(cluster ~ ., data = _)

## Coeficientes

coef(m)
# m$coefnames[-1] |> 
#   set_names() |> 
#   map_dfc(~effect(.x, m) |> 
#         pluck("x"))

# kk <- effects::effect("receptor", m) 

## Predicción escenario
predict(m, escenario, type = "prob")
predict(m, escenario)

