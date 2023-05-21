






## Inventarme inversiones de los países
## Como porcentaje del total de inversiones??
## Modelo ridge con eso
## Coeficientes se aplican a lo que tengo de CM
## Escenario otras inversiones
## Serie temporal: con los spi estimados
## Optimizar la función teniendo en cuenta porcentajes...


## Modelos CU 53
library(dplyr)
library(tidyr)
library(readr)
library(glmnet)
library(tsibble)
library(fable)

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/53_inversion")


## Datos
inversiones <- read_csv("cu_53_step_01_input/CU_53_05_05_inversiones_cm.csv")

spi <- read_csv("cu_53_step_01_input/CU_53_05_02_01_spi.csv")  
spi_meta <- read_csv("cu_53_step_01_input/CU_53_05_02_02_spi_metadata.csv")

x_inv <- inversiones |> 
  group_by(anyo) |> 
  mutate(total_anyo = sum(inversion),
         porc_inv = round(100*(inversion/total_anyo), 2)) |> 
  ungroup() |> 
  pivot_wider(id_cols = anyo, names_from = "grupo",
                           values_from = "porc_inv") |> 
  rename(spiyear = anyo,
         inv_inf = INFRAESTRUCTURAS,
         inv_tur = TURISMO,
         inv_san = SANIDAD) |> 
  select(-RESTO) |> 
  as.matrix()
  

## MODELO

modelo <- read_rds("cu_53_step_04_output/modelo_reg.rds")

## ESTIMACIÓN

## Mostrar en tabla
spi_madrid <- x_inv |> 
  bind_cols(predict(modelo, x_inv)) |> 
  rename(spi = s0)

## SERIE TEMPORAL

spi_ts <- spi_madrid |> as_tsibble(index = spiyear)

modelo_ts <- spi_ts |> 
  model(arima = ARIMA(spi))

prediction <- modelo_ts |> forecast(h = 2)

## GRÁFICO: Serie temporal y su predicción
