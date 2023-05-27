## Caso 45 krigging

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/45_turismo_patrones")

## Estaba previsto hacerlo con la ocupación. Como no la tenemos, lo hago
## con la valoración


library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(readr)
library(sf, quietly = TRUE)
library(stars)
library(gstat)
library(leaflet)
library(leafem)

## Datos necesarios

valoraciones <- read_csv("cu_45_step_01_input/CU_45_05_02_valoracion_sim.csv")

##. parámetros

KRIG_OBJ <- "Turismo"

## Arreglar datos

sfmun <- st_read("cu_45_step_01_input/CU_45_05_01_municipios_geo.json")

sfval <- st_as_sf(valoraciones |> 
                    filter(grupo == "turismo"), 
                  coords = c("X", "Y"),
                  crs = st_crs(4326))

ini <- Sys.time()
grd <- st_bbox(sfmun) |> 
  st_as_stars() |> 
  st_crop(sfmun)
Sys.time() - ini


## Variograma empírico robusto
v0 <-  variogram(puntos ~ 1, 
                 sfval |> drop_na(), 
                 cressie = TRUE)

## Modelo variograma
v.m <- fit.variogram(v0, vgm(c("Exp", "Mat", "Sph", "Ste", "Gau")), 
                     fit.kappa = TRUE)

## kriging
b.punt  <- krige(puntos ~ 1, 
                 locations = sfval |> drop_na(), 
                 newdata = grd,
                 model = v.m)

## Guardar modelo

write_rds(b.punt, "cu_45_step_07_output/krige_oportunidad.rds")


## Lo de arriba paso 6, como script
## Lo siguiente paso 7, app


pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#EE2088"), b.punt$var1.pred,
                    na.color = "transparent")

leaflet() |> 
  addTiles() |> 
  addStarsImage(b.punt, colors = pal, opacity = 0.8) |> 
  addLegend(pal = pal, values = b.punt$var1.pred,
            title = "Oportunidades por valoraciones")

