t0 <- Sys.time()

args <- commandArgs(trailingOnly = TRUE)

if(length(args) < 2){
  stop("Se deben especificar tres argumentos: el primero para la carpeta de entrada, el segundo para la carpeta de salida y el tercero para la carpeta de maestros")
} else if(!all(file.exists(args[1]),
               file.exists(args[2]))){
  stop("Alguna de las carpetas especificadas no existe, compruebe por favor.")
} else{
  carpeta_entrada <- args[1]
  carpeta_salida <- args[2]
  message("\n1: Carpetas correctas: ", 
          paste0(carpeta_entrada, carpeta_salida,
                 collapse = ", "),
          "\n")
}


library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(readr)
library(sf, quietly = TRUE)
library(stars)
library(gstat)
library(leafem)

## Datos necesarios

valoraciones <- read_csv(paste0(carpeta_entrada, "/CU_45_05_02_valoracion_sim.csv"))
variables <- read_csv(paste0(carpeta_entrada, "/VARIABLES.csv"))

##. parámetros

KRIG_OBJ <- variables |> 
                     filter(variable == "KRIG_OBJ") |> 
                     pull(valor)

## Arreglar datos

sfmun <- st_read(paste0(carpeta_entrada, "/CU_45_05_01_municipios_geo.json"))

sfval <- st_as_sf(valoraciones |> 
                    filter(grupo == KRIG_OBJ), 
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

write_rds(b.punt, paste0(carpeta_salida, "/krige_oportunidad.rds"))