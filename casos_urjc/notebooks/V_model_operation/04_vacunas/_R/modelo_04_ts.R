## MODELO

## MEJORAR: 
## - AÑADIR COVARIABLES
## - PODER USAR EL MODELO DEL AÑO ANTERIOR

setwd("/Users/emilio.lcano/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/04_vacunas")

## Paquetes
library(readr)
library(dplyr)

## DATOS

data_04_completo <- read_csv("CU_04_05_19_vacunacion_gripe_completo.csv") 

library(tsibble)  

## NOTA: ajusto el modelo con las semanas que llevamos de campaña
# tsdata <- data_04_completo |>
#   filter(campana == 2021) |>
#   mutate(tsweek = make_yearweek(ano, semana)) |>
#   select(GEOCODIGO, n_vacunas, tsweek, tuits_gripe, interes_gripe, tmed, prec) |>
#   as_tsibble(key = GEOCODIGO,
#              index = tsweek)

## VISUALIZACIÓN

# library(ggplot2)
# library(fable)
# tsdata |> 
#   group_by(GEOCODIGO) |> 
#   autoplot(n_vacunas) +
#   theme(legend.position = "none")

## MODELO

# Sin regresores
# mod_04_ts <- tsdata |> 
#   model(arima = ARIMA(n_vacunas))

# mod_04_ts <- tsdata |> 
#   model(arima = ARIMA(n_vacunas))
# 
# write_rds(mod_04_ts, "mod_04_ts.rds")

# mod_04_ts |> coef() |> View()
# mod_04_ts |> glance() |> View()
# mod_04_ts |> filter(GEOCODIGO == "030") |> report()
# mod_04_ts |> augment()
# mod_04_ts |> accuracy() |> arrange(MAE)

## NUEVOS DATOS: campaña iniciada 10 semanas

# actual <- data_04_completo |>
#   filter(ano == 2022, semana < 45, semana >= 35) |>
#   select(GEOCODIGO, campana, ano, semana, n_vacunas, tuits_gripe, interes_gripe)

# write_csv(actual, "ACTUAL_TS.csv")

actual <- read_csv("ACTUAL_TS.csv")

library(fable)
# actualts <- actual |> 
#   mutate(tsweek = make_yearweek(ano, semana)) |> 
#   as_tsibble(key = GEOCODIGO, index = tsweek) |> 
#   fill_gaps()


# mod_04_ts_new <- mod_04_ts |> 
#   refit(newtsdata)

actualts <- actual |>
  mutate(tsweek = make_yearweek(ano, semana)) |>
  as_tsibble(key = GEOCODIGO, index = tsweek) |>
  fill_gaps()

mod_04_ts <- actualts |>  
  model(arima = ARIMA(n_vacunas))


## Esto lo cogerá el notebook de las variables:

SEMANA_INICIO <- 36
SEMANA_FIN <- 5

h <- as.numeric(
  make_yearweek(min(actualts$ano, na.rm = TRUE) + 1, SEMANA_FIN) - min(actualts$tsweek, na.rm = TRUE))

prediction <- mod_04_ts |> forecast(h = h)


prediction |> filter(GEOCODIGO == "030") |> autoplot(actualts)





  
