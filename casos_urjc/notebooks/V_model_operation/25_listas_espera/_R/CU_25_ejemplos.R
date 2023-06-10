setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/25_listas_espera/")


library(readr)
library(dplyr)
library(tidyr)
library(stringr)


indicadores <- read_csv("cu_25_step_01_input/CU_25_05_06_indicadores_area.csv")
capacidad <- read_csv("cu_25_step_01_input/CU_25_05_07_01_capacidad.csv")
lista <- read_csv("cu_25_step_01_input/CU_25_05_07_02_lista_espera.csv")
hospitales <- read_csv("cu_25_step_01_input/CU_25_05_05_01_hospitales.csv")


