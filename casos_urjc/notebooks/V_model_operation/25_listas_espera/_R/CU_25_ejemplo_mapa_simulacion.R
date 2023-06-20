## Ejemplo mapa simulación

library(readr)
library(tidyr)
library(dplyr)
library(sf)
library(leaflet)

sim_h_recursos <- read_rds("cu_25_step_06_output/sim_h_recursos.rds")
sim_h_llegadas <- read_rds("cu_25_step_06_output/sim_h_llegadas.rds")

zonas <- st_read("cu_25_step_01_input/CU_25_05_03_areasgeo.json")

## Filtros de la app

esp <- "Traumatología"

## Aquí será un tiempo el que seleccionamos. 
## Como el tiempo es muy largo, con un slider


## Mapa recursos

f <- sample(sim_h_recursos$time, size = 1)


## Este join tarda un poco, hago el filter primero
## Filtro valores menores que time, y después me quedo solo con el último valor de cada zona
dfsim <- sim_h_recursos |> 
  tidyr::separate(id, into = c("zona", "especialidad"), sep = "\\.") |> 
  filter(especialidad == esp,
         time <= f) |> 
  group_by(zona) |> 
  slice_tail(n = 1)

dmap <- zonas |> 
  full_join(dfsim,
            by = c("DESBDT" = "zona"))  

qpal <- colorQuantile("Blues", dmap$system, n = 4)  
dmap |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons(color = ~qpal(system), 
              fillOpacity = 0.8,
              weight = 1,
              popup = ~paste(round(system), "personas"),
              label = ~DESBDT)  |> 
  addLegend("bottomright", pal = qpal, values = ~system,
            title = "Sistema",
            opacity = 1
  ) 




## Mapa llegadas

f <- sample(sim_h_llegadas$start_time, size = 1)


## Este join tarda un poco, hago el filter primero
## Filtro valores menores que time, y después me quedo solo con el último valor de cada zona
dfsim <- sim_h_llegadas |> 
  tidyr::separate(id, into = c("zona", "especialidad"), sep = "\\.") |> 
  filter(especialidad == esp,
         start_time <= f) |> 
  group_by(zona) |> 
  summarise(cola = sum(finished))

dmap <- zonas |> 
  full_join(dfsim,
            by = c("DESBDT" = "zona"))  

qpal <- colorQuantile("Blues", dmap$cola, n = 4)  
dmap |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons(color = ~qpal(cola), 
              fillOpacity = 0.8,
              weight = 1,
              popup = ~paste(round(cola), "personas"),
              label = ~DESBDT)  |> 
  addLegend("bottomright", pal = qpal, values = ~cola,
            title = "Cola",
            opacity = 1
  ) 




