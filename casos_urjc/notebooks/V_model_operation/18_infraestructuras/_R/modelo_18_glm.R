library(readr)
library(dplyr)
setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/18_infraestructuras/")

df <- read_csv("cu_18_step_01_input/CU_18_05_20_diario_infra.csv")

mod_infra <- df |> select(-c(id_inf, fecha, evento_zona)) |> 
  glm(evento_infra ~ ., data = _, family = binomial)

mod_zona <- df |> select(-c(id_inf, fecha, evento_infra)) |> 
  glm(evento_zona ~ ., data = _, family = binomial)

write_rds(mod_infra, "cu_18_maestros/mod_glm_infra.rds")
write_rds(mod_zona, "cu_18_maestros/mod_glm_zona.rds")



