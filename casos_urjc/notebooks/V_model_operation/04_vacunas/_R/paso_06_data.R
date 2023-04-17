## MAPA DE RIESGO

setwd("/Users/emilio.lcano/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/04_vacunas")

source("LIBRERIAS.R")

source("VARIABLES.R")


## DATOS

data_04_completo <- read_csv("CU_04_05_19_vacunacion_gripe_completo.csv") 

capacidad <- data_04_completo |> 
  group_by(GEOCODIGO) |> 
  summarise(capacidad = mean(capacidad_zona, na.rm = TRUE))

write_csv(capacidad, "CAPACIDAD.csv")



## GEO
mapSpain::esp_get_prov("28") |> 
  st_bbox() |> 
  st_as_stars()
st_bbox(RIESGO, crs = "EPSG:4258") |> 
  st_as_stars(dx = 100)
