## Generador de escenarios

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/55_turismo_gasto/")

library(readr)
library(dplyr)

gasto_municipio <- read_csv("cu_55_step_01_input/CU_55_05_02_gasto_municipio.csv")

## Escenario origen

gasto_municipio |> 
  filter(mes %in% c("2021-08", "2021-09", "2021-10"),
         pais_orig == "Francia",
         str_starts(mun_dest, "M")) |> 
  select(-c(pais_orig_cod, mun_dest_cod, CMUN, gasto)) |> 
  write_csv("cu_55_step_01_input/ESCENARIO_ORIGEN.csv")

## Escenario destino

gasto_municipio |> 
  filter(mes %in% c("2021-08"),
         str_starts(pais_orig, "Total", negate = TRUE),
         mun_dest == "Alcalá de Henares") |> 
  select(-c(pais_orig_cod, mun_dest_cod, CMUN, gasto))  |> 
  write_csv("cu_55_step_01_input/ESCENARIO_DESTINO.csv")
