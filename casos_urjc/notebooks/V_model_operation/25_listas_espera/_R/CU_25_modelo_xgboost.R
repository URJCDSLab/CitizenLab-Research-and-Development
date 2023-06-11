# https://www.business-science.io/code-tools/2020/06/29/introducing-modeltime.html

## ESTE MODELO SE GUARDA EN MAESTROS PARA DESPUÉS CARGARLO Y HACER PREDICCIÓN
## Para la predicción solo hace falta el horizonte

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/25_listas_espera/")


library(tidymodels)
library(modeltime)
library(timetk)   
library(lubridate)
library(tidyverse)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
indicadores <- read_csv("cu_25_step_01_input/CU_25_05_06_indicadores_area.csv")
capacidad <- read_csv("cu_25_step_01_input/CU_25_05_07_01_capacidad.csv")
lista <- read_csv("cu_25_step_01_input/CU_25_05_07_02_lista_espera.csv")
hospitales <- read_csv("cu_25_step_01_input/CU_25_05_05_01_hospitales.csv")

lista <- lista |> 
  mutate(fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))

h <- "HOSPITAL UNIVERSITARIO LA PAZ"
a <- "05"
e <- "Angiología y Cirugía Vascular"

## Valores perdidos: fill (solo hay una semana)

lzona_esp_1 <- lista |> 
  left_join(hospitales) |> 
  filter(Especialidad == e,
         id_area == a) |> 
  fill(total_pacientes, media_tiempo_dias) |> 
  group_by(nombre_area, Especialidad, fecha) |> 
  summarise(total_pacientes = sum(total_pacientes, na.rm = TRUE),
            media_tiempo_dias = mean(media_tiempo_dias, na.rm = TRUE)) 

lzona_esp_1 |> plot_time_series(fecha, total_pacientes)

## XBGoost con series temporales

## División conjuntos de datos
splits <- lzona_esp_1 %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

## Visualización
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(fecha, total_pacientes, .interactive = FALSE)

## Tidymodels workflow

recipe_spec <- recipe(total_pacientes ~ fecha, training(splits)) %>%
  step_timeseries_signature(fecha) %>%
  step_rm(
    # contains("am.pm"), contains("hour"), contains("minute"),
    #       contains("second"),
    contains("week"),
    contains("xts")) %>%
  step_fourier(fecha, period = 365, K = 5) %>%
  step_dummy(all_nominal())  |>
  step_zv()

recipe_spec %>% prep() %>% juice()

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(fecha)) %>%
  fit(training(splits))


model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE,
                                          seasonality_weekly = TRUE,
                                          seasonality_daily = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost

model_table <- modeltime_table(
  # model_fit_arima, 
  # model_fit_prophet,
  workflow_fit_glmnet,
  # workflow_fit_rf,
  workflow_fit_prophet_boost
) 
model_table

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table

calibration_table %>%
  modeltime_forecast(actual_data = lzona_esp_1) %>%
  plot_modeltime_forecast(.interactive = FALSE)

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


## Refit and forecast
calibration_table |> 
  filter(.model_id == 2) |> 
  modeltime_refit(lzona_esp_1) %>%
  modeltime_forecast(h = "8 weeks", actual_data = lzona_esp_1) %>%
  plot_modeltime_forecast(.interactive = FALSE)






## Ajustar todos los modelos a lo bestia

lzona_esp <- lista |> 
  left_join(hospitales) |> 
  # filter(Especialidad == e,
  #        id_area == a) |> 
  fill(total_pacientes, media_tiempo_dias) |> 
  group_by(nombre_area, Especialidad, fecha) |> 
  summarise(total_pacientes = sum(total_pacientes, na.rm = TRUE),
            media_tiempo_dias = mean(media_tiempo_dias, na.rm = TRUE)) 

dfs <- split(lzona_esp, ~nombre_area + Especialidad)

res_pacientes <- map(dfs, function(x){
  recipe_spec <- recipe(total_pacientes ~ fecha, x) %>%
    step_timeseries_signature(fecha) %>%
    step_rm(
      # contains("am.pm"), contains("hour"), contains("minute"),
      #       contains("second"),
      contains("week"),
      contains("xts")) %>%
    step_fourier(fecha, period = 365, K = 5) %>%
    step_dummy(all_nominal())  |>
    step_zv()
  recipe_spec %>% prep() %>% juice()
  model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE,
                                            seasonality_daily = FALSE,
                                            seasonality_weekly = TRUE) %>%
    set_engine("prophet_xgboost") 
  workflow_fit_prophet_boost <- workflow() %>%
    add_model(model_spec_prophet_boost) %>%
    add_recipe(recipe_spec) %>%
    fit(x)
  model_table <- modeltime_table(
    workflow_fit_prophet_boost
  ) 
  calibration_table <- model_table %>%
    modeltime_calibrate(x)
})

res_tiempo <- map(dfs, function(x){
  recipe_spec <- recipe(media_tiempo_dias ~ fecha, x) %>%
    step_timeseries_signature(fecha) %>%
    step_rm(
      # contains("am.pm"), contains("hour"), contains("minute"),
      #       contains("second"),
      contains("week"),
      contains("xts")) %>%
    step_fourier(fecha, period = 365, K = 5) %>%
    step_dummy(all_nominal())  |>
    step_zv()
  recipe_spec %>% prep() %>% juice()
  model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE,
                                            seasonality_daily = FALSE,
                                            seasonality_weekly = TRUE) %>%
    set_engine("prophet_xgboost") 
  workflow_fit_prophet_boost <- workflow() %>%
    add_model(model_spec_prophet_boost) %>%
    add_recipe(recipe_spec) %>%
    fit(x)
  model_table <- modeltime_table(
    workflow_fit_prophet_boost
  ) 
  calibration_table <- model_table %>%
    modeltime_calibrate(x)
})

## Predicción
prediccion <- res_tiempo$`Centro-Norte.Angiología y Cirugía Vascular` |>
  modeltime_forecast(h = 2, actual_data = dfs$`Centro-Norte.Angiología y Cirugía Vascular`)

## Visualización
dfs$`Centro-Norte.Angiología y Cirugía Vascular` |> 
  plot_time_series(fecha, total_pacientes)
prediccion |> plot_modeltime_forecast()  

## Guardar modelos

write_rds(res_pacientes, "cu_25_maestros/modelos_pacientes_xgboost.rds")
write_rds(res_tiempo, "cu_25_maestros/modelos_tiempo_xgboost.rds")

