setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/55_turismo_gasto/")


library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(xgboost)


gasto_municipio <- read_csv("cu_55_step_01_input/CU_55_05_02_gasto_municipio.csv")

dm <- gasto_municipio |> 
  mutate(nmes = factor(str_sub(mes, 6, 7)),
         pais_orig = factor(pais_orig)) |> 
  select(nmes, pais_orig, turistas, gasto) |> 
  filter(str_detect(pais_orig, "Total", negate = TRUE))

mm <- model.matrix( ~ ., dm)

# mm <- Matrix::sparse.model.matrix(~., data = dm)

train <- sample(1:nrow(mm), round(0.8* nrow(mm)), replace = FALSE)
x.train <- mm[train, 1:(ncol(mm) - 1)]
y.train <- mm[train, ncol(mm)]

x.test <- mm[-train, 1:(ncol(mm) - 1)]
y.test <- mm[-train, ncol(mm)]

modelo <- xgboost(data = x.train, label = y.train, nrounds = 10)

importance <- xgb.importance(feature_names = colnames(x.train), model = modelo)
head(importance)

xgb.plot.importance(importance_matrix = importance)

plot(predict(modelo, x.test), y.test)

write_rds(modelo, "cu_55_maestros/modelo_xgb.rds")


### predicción
## 1. tipo escenario origen

escenario <- read_csv("cu_55_step_01_input/ESCENARIO_ORIGEN.csv") 

escenario.x <- escenario |> 
  mutate(nmes = factor(str_sub(mes, 6, 7), levels = levels(dm$nmes)),
         pais_orig = factor(pais_orig, levels = levels(dm$pais_orig))) |> 
  select(nmes, pais_orig, turistas) |> 
  model.matrix(~., data = _)

predict(modelo, escenario.x)
## 1. tipo escenario destino

escenario <- read_csv("cu_55_step_01_input/ESCENARIO_DESTINO.csv") 

escenario.x <- escenario |> 
  mutate(nmes = factor(str_sub(mes, 6, 7), levels = levels(dm$nmes)),
         pais_orig = factor(pais_orig, levels = levels(dm$pais_orig))) |> 
  select(nmes, pais_orig, turistas) |> 
  model.matrix(~., data = _)

predict(modelo, escenario.x)


