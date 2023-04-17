#!/usr/bin/env Rscript

########
# SCRIPT PASO 7 (ESTIMACIÓN MODELO KRIGING) CU 04 (GESTIÓN VACUNAS GRIPE)
# ENTRADA: SALIDA DEL PASO 5
########


args <- commandArgs(trailingOnly = TRUE)

if(length(args) < 3){
  stop("Se deben especificar tres argumentos: el primero para la carpeta de entrada, el segundo para la carpeta de salida y el tercero para la carpeta de maestros")
} else if(!all(file.exists(args[1]),
               file.exists(args[2]),
               file.exists(args[3]))){
  stop("Alguna de las carpetas especificadas no existe, compruebe por favor.")
} else{
  carpeta_entrada <- args[1]
  carpeta_salida <- args[2]
  carpeta_maestros <- args[3]
  message("\n1: Carpetas correctas: ", 
          paste0(carpeta_entrada, carpeta_salida, carpeta_maestros,
                 collapse = ", "),
          "\n")
}

t0 <- Sys.time()

## Paquetes

library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(readr)
library(sf, quietly = TRUE)
library(stars)
library(gstat)

## Datos necesarios


sfzonas <- read_sf(paste0(carpeta_entrada, 
                          "/ZONAS.json"), 
                   quiet = TRUE)

dfcapacidad <- read_csv(paste0(carpeta_entrada, "/CAPACIDAD.csv"), 
                        show_col_types = FALSE)

fpred <- paste0(carpeta_entrada, "/PREDICCION_TS.csv")

if(file.exists(fpred)){
  dfprediccion_ts <- read_csv(fpred, 
                              show_col_types = FALSE)
} else{
  dfprediccion_ts <- NA
  warning("No hay archivo de predicción resto campaña")
}

fproy <- paste0(carpeta_entrada, "/PROYECCION.csv")

if(file.exists(fproy)){
  dfproyeccion <- read_csv(fproy, 
                           show_col_types = FALSE)
} else{
  dfproyeccion <- NA
  warning("No hay archivo de proyección próxima campaña")
}

## Transormación de datos

## UNIÓN DE TABLAS
sfriesgo <-  sfzonas |> 
  inner_join(
    dfproyeccion |> 
      group_by(GEOCODIGO) |> 
      summarise(fit = sum(fit, na.rm = TRUE)), 
    by = "GEOCODIGO")

if(is.data.frame(dfprediccion_ts)){
  
  sfriesgo <- sfriesgo |> 
    inner_join(
      dfprediccion_ts |> 
        group_by(GEOCODIGO) |> 
        summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE)), 
      by = "GEOCODIGO") 
}

if(is.data.frame(dfproyeccion)){
  sfriesgo <- sfriesgo |> 
    inner_join(
      dfcapacidad, by = "GEOCODIGO"
    ) |> 
    mutate(saturacion_proy = fit/capacidad,
           saturacion_pred = n_vacunas / capacidad)
}  

t1 <- Sys.time()
message("2: Ficheros cargados ", round(t1-t0, 3), " s \n")

message("3: Inicio estimación modelos espaciales e interpolación (puede tardar)\n")

## DISCRETIZACIÓN DE ZONAS POR CENTROIDE
sfriesgo.cent <- st_centroid(sfriesgo)

## MALLA DE DATOS PARA KRIGING
grd <- st_bbox(sfriesgo) |> 
  st_as_stars() |> 
  st_crop(sfriesgo)

### MODELO PARA PROYECCIÓN ----

## Variograma empírico robusto
v0 <-  variogram(saturacion_proy ~ 1, 
                 sfriesgo.cent |> drop_na(), 
                 cressie = TRUE)

## Modelo variograma
v.m <- fit.variogram(v0, vgm(c("Exp", "Mat", "Sph", "Ste", "Gau")), 
                     fit.kappa = TRUE)

## kriging
b.proy  <- krige(saturacion_proy ~ 1, 
            locations = sfriesgo.cent |> drop_na(), 
            newdata = grd,
            model = v.m)


### MODELO PARA PREDICCIÓN ----

## Variograma empírico robusto
v0 <-  variogram(saturacion_pred ~ 1, 
                 sfriesgo.cent |> drop_na(), 
                 cressie = TRUE)

## Modelo variograma
v.m <- fit.variogram(v0, vgm(c("Exp", "Mat", "Sph", "Ste", "Gau")), 
                     fit.kappa = TRUE)

## kriging
b.pred  <- krige(saturacion_pred ~ 1, 
            locations = sfriesgo.cent |> drop_na(), 
            newdata = grd,
            model = v.m)




t2 <- Sys.time()
message("\n4: Modelos estimados e interpolación realizada ", round(t2-t1, 3), " s\n")

 
## Guardar archivos

message("5: Guardando archivos en output.\n")

write_rds(b.proy, paste0(carpeta_salida, "/krige_proy.rds"))
write_rds(b.pred, paste0(carpeta_salida, "/krige_pred.rds"))

# f1 <- file.copy(from = paste0(carpeta_entrada, "/VARIABLES.csv"), 
#                 to = carpeta_salida, overwrite = TRUE)
# f2 <- file.copy(from = paste0(carpeta_entrada, "/NUEVACAMPANA.csv"),
#                 to = carpeta_salida, overwrite = TRUE)
# f3 <- file.copy(from = paste0(carpeta_entrada, "/VARIABLES.csv"),
#                 to = carpeta_salida, overwrite = TRUE)
# f4 <- file.copy(from = paste0(carpeta_entrada, "/ZONAS.json"),
#                 to = carpeta_salida, overwrite = TRUE)
# if(!all(f1, f2, f3, f4)){
#   stop("Error al copiar los ficheros en la carpeta de salida")
# }
# 

message("Fin del proceso. Tiempo total: ", round(t2-t0, 2), "s\n")
