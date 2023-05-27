setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/45_turismo_patrones")


library(readr)
library(dplyr)
library(tidyr)
library(stringr)
receptor <- read_csv("cu_45_step_01_input/CU_45_05_03_receptor.csv")
interno_prov <- read_csv("cu_45_step_01_input/CU_45_05_04_interno_prov.csv")
interno_muni <- read_csv("cu_45_step_01_input/CU_45_05_05_interno_mun.csv")
valoraciones <- read_csv("cu_45_step_01_input/CU_45_05_02_valoracion_sim.csv")

turistas_origen_mes <- 
receptor |> 
  filter(pais_orig == "Total") |> 
  select(mes, CMUN, mun_dest, turistas) |> 
  mutate(receptor = turistas) |> 
  select(-turistas) |> 
  inner_join(
    interno_prov |> 
      filter(!is.na(total_ccaa),
             is.na(provincia)) |> 
      select(mes, cmun, municipio_destino, total_ccaa, turistas) |> 
      pivot_wider(names_from = total_ccaa, 
                  values_from = turistas,
                  values_fn = ~sum(.x, na.rm = TRUE)),
    by = c("CMUN" = "cmun", "mes" = "mes"))

anyo <- "2022"
turistas_origen_anyo <-
  turistas_origen_mes |> 
  filter(str_sub(mes, 1, 4) == anyo)

# group_by(total_ccaa) |> 
# summarise(turistas = sum(turistas, na.rm = TRUE))

