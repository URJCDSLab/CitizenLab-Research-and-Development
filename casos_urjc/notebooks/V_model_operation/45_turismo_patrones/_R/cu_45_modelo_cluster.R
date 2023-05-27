## Caso 45 cluster

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/45_turismo_patrones")


library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)

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
      select(mes, cmun, total_ccaa, turistas) |> 
      pivot_wider(names_from = total_ccaa, 
                  values_from = turistas,
                  values_fn = ~sum(.x, na.rm = TRUE)),
    by = c("CMUN" = "cmun", "mes" = "mes"))

val <- valoraciones |> 
  group_by(CMUN, grupo) |> 
  summarise(puntos = median(puntos, na.rm = TRUE),.groups = "drop") |> 
  pivot_wider(names_from = grupo,
              values_from = puntos) |> 
  mutate(across(2:4, ~replace_na(.x, median(.x, na.rm = TRUE))))

kdata <- turistas_origen_mes |> 
  inner_join(val, by = "CMUN") |> 
  # select(-municipio_destino) |> 
  mutate(anyo = str_sub(mes, 1, 4)) |> 
  group_by(anyo, mun_dest) |> 
  summarise(across(3:(ncol(turistas_origen_mes) - 1), sum),
            across((ncol(turistas_origen_mes) - 1):(ncol(turistas_origen_mes) + 2), median))

by_anyo <- split(kdata, kdata$anyo)

by_anyo[[1]] |> 
  column_to_rownames("mun_dest") |> 
  select(-anyo) |> 
  kmeans(10)


## PARÁMETRO DEL PASO 2
NCLUS <- 4

cluster_anyos <- map(by_anyo, function(x)  {
  m <- x |> 
    column_to_rownames("mun_dest") |> 
    select(-anyo) |> 
    kmeans(NCLUS)
  res <- x |> 
    mutate(cluster = m$cluster)
}) |> bind_rows() |> 
  ungroup()

write_csv(cluster_anyos, "cu_45_step_04_output/cluster_anyos.csv")

