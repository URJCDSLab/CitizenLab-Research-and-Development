## ESCENARIO PARA REGRESIÓN
library(readr)
library(dplyr)
set.seed(1)

cluster_anyos <- read_rds("cu_45_step_04_output/cluster_anyos.rds")

escenario <- slice_sample(cluster_anyos, n = 1) |> 
  select(-c(anyo, mun_dest, cluster))

write_csv(escenario, "cu_45_step_01_input/ESCENARIO_REG.csv")
