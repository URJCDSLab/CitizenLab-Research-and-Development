library(readr)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(tidyr)
library(janitor)
library(cluster)
library(DT)

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/34_demanda_servicios/")

datos <- read_csv("cu_34_step_01_input/CU_34_05_05_servicios_completo.csv")

# fechaclus <- "2022-01-01"
# set.seed(1)
# municlus <- sample(datos$CMUN, 1)

# apply(datos, 2, \(x) sum(is.na(x)))

datosclust <- datos |> 
  # filter(Fecha == as.Date(fechaclus)) |>
  # filter(CMUN == municlus) |>
  drop_na() |> 
  # select(-c(Fecha:CSEC, ccaa, CPRO, NSEC)) |> 
  remove_constant() 

zdatosclust <- datosclust |> 
  select(-c(Fecha:CSEC, NSEC)) |>
  scale()

# km <- kmeans(datosclust, 4)
km <- clara(zdatosclust, 10)
# plot(km)
fviz_cluster(km, geom = "point", alpha = 0.5) +
  theme_bw()

datos2 <-  datosclust |> 
  mutate(cluster = km$clustering) |> 
  relocate(cluster, NSEC, .before = 1)

## TABLA CARACTERIZACIÓN
datos2 |> 
  group_by(cluster) |> 
  summarise(across(Futbol:densidad_hab_km2, median)) |> 
  datatable()

## TABLA DETALLE MUNICIPIO
datos2 |> 
  filter(CMUN == "079")

## DATOS PARA MAPA

datos2 |> 
  group_by(NSEC, CMUN, CDIS, CSEC, cluster) |>
  summarise(n = n()) |> 
  rowwise() |> 
  slice_max(n) |> 
  ungroup() |> 
  datatable()

## guardar datos

write_rds(datos2, "cu_34_step_04_output/output_cluster.rds")
write_rds(km, "cu_34_step_04_output/modelo_cluster.rds")
