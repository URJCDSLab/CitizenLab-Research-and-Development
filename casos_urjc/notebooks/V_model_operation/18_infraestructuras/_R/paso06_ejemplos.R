library(readr)
library(mclust)
library(dplyr)
library(purrr)
library(ggplot2)
library(summarytools)

NSIM <- 100

## Simulación clusters

dfout <- read_rds("cu_18_maestros/datos_cluster_distritos.rds")
df_mc <- read_rds("cu_18_maestros/modelo_cluster_distritos.rds")

escenario <- read_csv("cu_18_step_01_input/ESCENARIO_CLUSTER_DIST.csv")
escenariom <- escenario |> 
  summarise(across(everything(), mean)) |> 
  mutate(across(everything(), ~if_else(.x == 0, 0.1, .x)))

escenarios <- escenario |> 
  summarise(across(everything(), sd)) |> 
  mutate(across(everything(), ~if_else(.x == 0, 0.01, .x)))

## Continuas
icont <- c(61:75, 136:139)
ncont <- colnames(escenario)[irec]

simulacion <- escenariom |> 
  # select(-all_of(irec)) |> 
  imap_dfc(~{
    if (.y %in% ncont) {
      rnorm(NSIM, .x, escenarios |> pull(.y))
    }else{
      rpois(NSIM, .x)
    }
  })

simulacion <- simulacion |> 
  bind_cols(cluster = predict(df_mc, simulacion)$classification) |> 
  relocate(cluster, .before = 1)

simulacion |> 
  ggplot(aes(cluster)) + 
  geom_bar()

simulacion |> freq(cluster) 
  

## Simulación regresión


escenario_reg <- read_csv("cu_18_step_01_input/ESCENARIO_REGRESION.csv")

## Uno de estos dos según selección (indicar en gráficos y tablas):
mod_glm <- read_rds("cu_18_maestros/mod_glm_infra.rds")
mod_glm <- read_rds("cu_18_maestros/mod_glm_zona.rds")

escenario_regm <- escenario_reg |> 
  summarise(across(everything(), mean)) |> 
  mutate(across(everything(), ~if_else(.x == 0, 0.1, .x)))

escenario_regs <- escenario_reg |> 
  summarise(across(everything(), sd)) |> 
  mutate(across(everything(), ~if_else(.x == 0, 0.01, .x)))

simulacion_reg <- escenario_regm |> 
  imap_dfc(~{
      rnorm(NSIM, .x, escenario_regs |> pull(.y))
  })

simulacion_reg <- simulacion_reg |> 
  bind_cols(evento = predict(mod_glm, simulacion_reg, type = "response") > 0.5) 

simulacion_reg |> 
  ggplot(aes(evento)) + 
  geom_bar()

simulacion_reg |> freq(evento) 


