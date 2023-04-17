#!/usr/bin/env Rscript

########
# SCRIPT PASO 5 (CÁLCULO PREDICCIÓN CAMPAÑA RESTANTE) CU 04 (GESTIÓN VACUNAS GRIPE)
########

t0 <- Sys.time()

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

## Paquetes

library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(mgcv, quietly = TRUE, warn.conflicts = FALSE)
library(tsibble, quietly = TRUE, warn.conflicts = FALSE)
library(fable, quietly = TRUE, warn.conflicts = FALSE)




dfnuevacampana <- read_csv(paste0(carpeta_entrada, "/NUEVACAMPANA.csv"), 
                           show_col_types = FALSE)
actualts <- dfnuevacampana |> 
  mutate(tsweek = make_yearweek(ano, semana)) |>
  as_tsibble(key = GEOCODIGO, index = tsweek) |>
  fill_gaps()

t1 <- Sys.time()
message("2: Fichero de escenario cargado ", round(t1-t0, 3), " s \n")

message("3: Inicio estimación modelos ARIMA (puede tardar)\n")

# Ajuste modelo ARIMA
mod_04_ts <- actualts |>  
  model(arima = ARIMA(n_vacunas))

t2 <- Sys.time()
message("4: Modelos estimados ", round(t2-t1,3), " s\n")

message("5: Inicio predicción ARIMA\n")

dfvariables <- read_csv(paste0(carpeta_entrada, "/VARIABLES.csv"), 
                        show_col_types = FALSE)


h <- as.numeric(
  make_yearweek(
    min(actualts$ano, na.rm = TRUE) + 1, 
    dfvariables |> 
      filter(variable == "SEMANA_FIN") |> 
      pull(valor) |> as.numeric()) - min(actualts$tsweek, na.rm = TRUE))


prediccion_ts <- mod_04_ts |> 
  forecast(h = h) |> 
  hilo(level = 90) 

dfprediccion <- prediccion_ts |>  
  mutate(across(last_col(), ~paste(.x$lower, .x$upper, sep = ";"))) |>
  separate(`90%`, into = c("lower", "upper"), sep = ";") |>
  as_tibble() |>
  mutate(n_vacunas = .mean,
         ano = year(tsweek),
         semana = isoweek(tsweek),
         dato = "Predicción") |>
  select(GEOCODIGO, ano, semana, n_vacunas, lower, upper, dato) |>
  bind_rows(dfnuevacampana |>
              mutate(dato = "Actual"))

t3 <- Sys.time()
message("6: Predicción ARIMA realizada. ", round(t3-t2,2), "s\n")

## Guardar archivos

message("7: Guardando archivos en output.\n")

write_rds(mod_04_ts, paste0(carpeta_salida, "/mod_04_ts.rds"))
write_csv(dfprediccion, paste0(carpeta_salida, "/PREDICCION_TS.csv"))
f1 <- file.copy(from = paste0(carpeta_entrada, "/VARIABLES.csv"), 
                to = carpeta_salida, overwrite = TRUE)
f2 <- file.copy(from = paste0(carpeta_entrada, "/NUEVACAMPANA.csv"),
                to = carpeta_salida, overwrite = TRUE)
f3 <- file.copy(from = paste0(carpeta_entrada, "/VARIABLES.csv"),
                to = carpeta_salida, overwrite = TRUE)
f4 <- file.copy(from = paste0(carpeta_entrada, "/ZONAS.json"),
                to = carpeta_salida, overwrite = TRUE)
f5 <- file.copy(from = paste0(carpeta_entrada, "/CAPACIDAD.csv"),
                to = carpeta_salida, overwrite = TRUE)
f6 <- file.copy(from = paste0(carpeta_entrada, "/PROYECCION.csv"),
                to = carpeta_salida, overwrite = TRUE)
if(!all(f1, f2, f3, f4, f5, f6)){
  stop("Error al copiar los ficheros en la carpeta de salida")
}

message("Fin del proceso. Tiempo total: ", round(t3-t0, 2), "s\n")
