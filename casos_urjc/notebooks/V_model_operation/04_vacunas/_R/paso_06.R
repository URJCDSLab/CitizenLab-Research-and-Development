## MAPA DE RIESGO

setwd("/Users/emilio.lcano/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/04_vacunas")

source("LIBRERIAS.R")

source("VARIABLES.R")



## ZONAS
file_name <- "ZONAS.json"
ZONAS <- st_read(file_name)

## CAPACIDAD
file_name <- "CAPACIDAD.csv"
CAPACIDAD <- read_csv(file_name)

## PROYECCIÓN CAMPAÑA ANTERIOR
file_name <- "PREDICTION.csv"
PROYECCION <- read_csv(file_name)

## PREDICCIÓN CAMPAÑA EN CURSO
file_name <- "PREDICCION_TS.csv"
PREDICCION <- read_csv(file_name)

## UNIÓN DE DATOS Y CÁLCULO DEL RIESGO
RIESGO <-  ZONAS |> 
  inner_join(
    PROYECCION |> 
      group_by(GEOCODIGO) |> 
      summarise(fit = sum(fit, na.rm = TRUE)), 
    by = "GEOCODIGO") |> 
  inner_join(
    PREDICCION |> 
      group_by(GEOCODIGO) |> 
      summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE)), 
    by = "GEOCODIGO") |> 
  inner_join(
    CAPACIDAD, by = "GEOCODIGO"
  ) |> 
  mutate(saturacion_proy = fit/capacidad,
         saturacion_pred = n_vacunas / capacidad)

RIESGO.cent <- st_centroid(RIESGO)

# RIESGO.cent |> 
#   ggplot(aes(col = saturacion_proy)) +
#   geom_sf()

## VARIOGRAMA Y KRIGING

# Malla

grd <- st_bbox(RIESGO) |> 
  st_as_stars() |> 
  st_crop(RIESGO)
# grd

## Variograma empírico robusto
v0 <-  variogram(saturacion_proy ~ 1, 
                 RIESGO.cent |> drop_na(), 
                 cressie = TRUE)

## Modelo variograma
v.m <- fit.variogram(v0, vgm(c("Exp", "Mat", "Sph", "Ste", "Gau")), 
                     fit.kappa = TRUE)

## kriging
b  <- krige(saturacion_proy ~ 1, 
            locations = RIESGO.cent |> drop_na(), 
            newdata = grd,
            model = v.m)

## VISUALIZACIÓN

pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#EE2088"), b$var1.pred,
                    na.color = "transparent")

leaflet() |> 
  addTiles() |> 
  addStarsImage(b, colors = pal, opacity = 0.8) |> 
  addLegend(pal = pal, values = b$var1.pred,
            title = "Riesgo de saturación")
  

write_stars(b, "kk.tif")
