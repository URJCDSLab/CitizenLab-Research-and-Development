library(readr)

dfout <- read_rds("cu_18_maestros/datos_cluster_distritos.rds")

set.seed(1)

write_csv(dfout |> 
            select(-c(1:8)) |> 
            slice_sample(n = 5), 
          "cu_18_step_01_input/ESCENARIO_CLUSTER_DIST.csv")

dfout <- read_rds("cu_18_maestros/datos_cluster_diario.rds")

write_csv(dfout |> 
            select(-c(1:8)) |> 
            slice_sample(n = 5), 
          "cu_18_step_01_input/ESCENARIO_CLUSTER_DIARIO.csv")

write_csv(dfout |> 
            select(-c(1:8)) |> 
            select(-starts_with("evento")) |> 
            slice_sample(n = 10), 
          "cu_18_step_01_input/ESCENARIO_REGRESION.csv")



