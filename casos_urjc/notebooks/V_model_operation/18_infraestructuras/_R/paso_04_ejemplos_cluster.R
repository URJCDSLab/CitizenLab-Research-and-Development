library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(mclust)
library(DT)

## Cluster: DISTRITOS

dfout <- read_rds("cu_18_maestros/datos_cluster_distritos.rds")
df_mc <- read_rds("cu_18_maestros/modelo_cluster_distritos.rds")

## Ejemplo gráfico ----

## pasar a ggplotly

library(ggplot2)
p1 <- dfout |> ggplot(aes(x = Dim.1, y = Dim.2, col = cluster)) +
  geom_point(alpha = 0.5)
p2 <- dfout |> ggplot(aes(x = Dim.2, y = Dim.3, col = cluster)) +
  geom_point(alpha = 0.5)
p3 <- dfout |> ggplot(aes(x = Dim.1, y = Dim.3, col = cluster)) +
  geom_point(alpha = 0.5)

grid.arrange(p1, p2, p3)

## Ejemplo tabla

dfout |> 
  group_by(cluster) |> 
  summarise(n = n(),
            across(9:146, mean)) |> 
  datatable()

## Ejemplo predicción escenario

escenario <- read_csv("cu_18_step_01_input/ESCENARIO_CLUSTER_DIST.csv")

escenario |> 
  mutate(Cluster = predict(df_mc, escenario)$classification,
         .before = 1) |> 
  datatable()

## ---------------------

## Cluster: DIARIO

dfout <- read_rds("cu_18_maestros/datos_cluster_diario.rds")
df_mc <- read_rds("cu_18_maestros/modelo_cluster_diario.rds")

## Ejemplo gráfico ----
## este son muchos puntos, no pasar a ggplotly

library(ggplot2)
p1 <- dfout |> ggplot(aes(x = Dim.1, y = Dim.2, col = cluster)) +
  geom_point(alpha = 0.5)
p2 <- dfout |> ggplot(aes(x = Dim.2, y = Dim.3, col = cluster)) +
  geom_point(alpha = 0.5)
p3 <- dfout |> ggplot(aes(x = Dim.1, y = Dim.3, col = cluster)) +
  geom_point(alpha = 0.5)

grid.arrange(p1, p2, p3)

## Ejemplo tabla

dfout |> 
  group_by(cluster) |> 
  summarise(across(9:15, mean)) |> 
  datatable()

## Ejemplo predicción escenario

escenario <- read_csv("cu_18_step_01_input/ESCENARIO_CLUSTER_DIARIO.csv")

escenario |> 
  mutate(Cluster = predict(df_mc, escenario)$classification,
         .before = 1) |> 
  datatable()
