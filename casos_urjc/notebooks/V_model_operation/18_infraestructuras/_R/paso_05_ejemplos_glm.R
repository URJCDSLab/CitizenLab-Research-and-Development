library(readr)
library(DT)
library(ggplot2)

mod_infra <- read_rds("cu_18_maestros/mod_glm_infra.rds")
summary(mod_infra)
mod_infra$model |> 
  ggplot(aes(x = demanda, y = evento_infra)) +
  geom_point(alpha = 0.1)

escenario <- read_csv("cu_18_step_01_input/ESCENARIO_REGRESION.csv")

escenario |> 
  mutate(Prob.evento = predict(mod_infra, escenario, type = "response")) |> 
  datatable()


