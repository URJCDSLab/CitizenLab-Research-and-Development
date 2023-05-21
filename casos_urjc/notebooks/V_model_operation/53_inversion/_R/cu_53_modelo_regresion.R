






## Inventarme inversiones de los países
## Como porcentaje del total de inversiones??
## Modelo ridge con eso
## Coeficientes se aplican a lo que tengo de CM
## Escenario otras inversiones
## Serie temporal: con los spi estimados
## Optimizar la función teniendo en cuenta porcentajes...


## Modelos CU 53
library(glmnet)
library(dplyr)
library(readr)

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/53_inversion")


## Datos
inversiones <- read_csv("cu_53_step_01_input/CU_53_05_05_inversiones_cm.csv")

spi <- read_csv("cu_53_step_01_input/CU_53_05_02_01_spi.csv")  
spi_meta <- read_csv("cu_53_step_01_input/CU_53_05_02_02_spi_metadata.csv")

escenario_spi <- read_csv("cu_53_step_01_input/ESCENARIO_INVERSIONES_PAISES.csv")

dfmodel <- spi |> 
  filter(spicountrycode != "WWW") |> 
  filter(!is.na(score_spi)) |> 
  select(spicountrycode, spiyear, score_spi) |> 
    inner_join(escenario_spi)
x <- dfmodel |> select(-c(spicountrycode, score_spi) ) |> as.matrix()
y <- dfmodel |> select(score_spi)  |> as.matrix()

## Modelo
cv_model <- cv.glmnet(x, y, alpha = 0)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min

## MOSTRAR EN INTERFAZ
best_lambda

# plot(cv_model) 

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
cc <- coef(best_model)


## MOSTRAR EN INTERFAZ
cc

## Predicción escenario región

escenario_region <- read_csv("cu_53_step_01_input/ESCENARIO_INVERSIONES_REGION.csv")

prediccion <- predict(best_model, escenario_region |> makeX())

## Representar series: escenario + predicción

## guardar modelo

write_rds(best_model, "cu_53_step_04_output/modelo_reg.rds")


