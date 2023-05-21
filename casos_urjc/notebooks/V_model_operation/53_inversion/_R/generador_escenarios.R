## Creación de escenarios

setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/53_inversion")

inversiones <- read_csv("cu_53_step_01_input/CU_53_05_05_inversiones_cm.csv")

spi <- read_csv("cu_53_step_01_input/CU_53_05_02_01_spi.csv")  
spi_meta <- read_csv("cu_53_step_01_input/CU_53_05_02_02_spi_metadata.csv")

inversiones <- inversiones |> 
  group_by(anyo) |> 
  mutate(total_anyo = sum(inversion),
         porc_inv = round(100*(inversion/total_anyo), 2)) |> 
  ungroup() 

parametros_sim <- inversiones |> group_by(grupo) |> 
  summarise(m = mean(porc_inv), s = sd(porc_inv))

a <- 20

dfb <- spi |> filter(country == "Spain") |> 
  select(spiyear, score_spi) |> 
  inner_join(inversiones,
             by = c("spiyear" = "anyo")) |> 
  mutate(b = score_spi/porc_inv) |> 
  group_by(grupo) |> 
  summarise(b = mean(b))

b <- setNames(dfb$b, dfb$grupo)

escenario_spi <- spi |> filter(country != "World") |> 
  select(spicountrycode, spiyear, score_spi) |> 
  mutate(inv_inf = (score_spi - rnorm(n(), 
                                      a, 1))/rnorm(n(), 
                                                   b[["INFRAESTRUCTURAS"]],
                                                   0.1*b[["INFRAESTRUCTURAS"]]),
         inv_tur = (score_spi - rnorm(n(), 
                                      a, 1))/rnorm(n(), 
                                                   b[["TURISMO"]],
                                                   0.1*b[["TURISMO"]]),
         inv_san = (score_spi - rnorm(n(), 
                                      a, 1))/rnorm(n(), 
                                                   b[["SANIDAD"]],
                                                   0.1*b[["SANIDAD"]])) |> 
  select(-score_spi)

write_csv(escenario_spi, "cu_53_step_01_input/ESCENARIO_INVERSIONES_PAISES.csv")

## ESCENARIOS INVERSIONES CM

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
  slice_tail(n = 1)

x_escen <- x_inv |> bind_rows(x_inv) |> bind_rows(x_inv) |> bind_rows(x_inv) |>  
  mutate(spiyear = 2022:2025) |> 
  mutate(across(inv_inf:inv_tur, ~.x*1.1^c(0:3)))

write_csv(x_escen, "cu_53_step_01_input/ESCENARIO_INVERSIONES_REGION.csv")
