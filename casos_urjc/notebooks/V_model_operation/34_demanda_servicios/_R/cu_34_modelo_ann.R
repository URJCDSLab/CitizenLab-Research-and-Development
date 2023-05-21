## Clasificación clusters

library(readr)
library(dplyr)
library(nnet)


datos <- read_rds("cu_34_step_04_output/output_cluster.rds")

train <- sample(1:nrow(datos), 0.8*nrow(datos))
test <- -train

dfmod <- datos |> 
  select(cluster, Futbol:densidad_hab_km2) |> 
  mutate(cluster = factor(cluster)) |> 
  mutate(prec = if_else(prec < 0, 0, prec))
# Y <- datos |> 
#   transmute(cluster = as.character(cluster)) 

modelo <- nnet(cluster ~ ., size = 10, data = dfmod, subset = train,
               decay = 1, maxit = 200)

table(dfmod$cluster[-train], predict(modelo, dfmod[-train,], type = "class"))
table(dfmod$cluster[train], predict(modelo, dfmod[train,], type = "class"))

write_rds(modelo, "cu_34_maestros/modelo_nnet.rds")

set.seed(1)
escenario <- dfmod |> 
  select(-cluster) |> 
  slice_sample(n = 100)

write_csv(escenario, "cu_34_step_01_input/ESCENARIO_SERVICIOS.csv")


## Tabla con el detalle:
pp <- predict(modelo, escenario) |> round(2)

## Gráfico de barras:
ppclass <- predict(modelo, escenario, type = "class")





  
