## Ejemplo mapa 

library(readr)
library(tidyr)
library(dplyr)
library(sf)
library(leaflet)

pred_h_personas <- read_rds("cu_25_step_06_output/pred_h_personas.rds")
zonas <- st_read("cu_25_step_01_input/CU_25_05_03_areasgeo.json")

## Filtros de la app

esp <- "Traumatología"
f <- sample(pred_h_personas$.index, size = 1)

dmap <- zonas |> 
  full_join(pred_h_personas |> 
              tidyr::separate(id, into = c("zona", "especialidad"), sep = "\\.") |> 
              filter(especialidad == esp,
                     .index == f),
            by = c("DESBDT" = "zona")) 

qpal <- colorQuantile("Blues", dmap$.value, n = 4)  
dmap |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons(color = ~qpal(.value), 
              fillOpacity = 0.8,
              weight = 1,
              popup = ~paste(round(.value), "personas"),
              label = ~DESBDT)  |> 
  addLegend("bottomright", pal = qpal, values = ~.value,
            title = "Pacientes (cuartil)",
            opacity = 1
  ) 
