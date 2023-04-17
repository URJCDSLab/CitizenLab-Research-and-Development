## MODELO

## ARREGLAR LO DEL NÚMERO DE SEMANA: QUE SEA EL NÚMERO DE SEMANA DE LA CAMPAÑA
## VER PRECIPITACIONES NEGATIVAS

setwd("/Users/emilio.lcano/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/04_vacunas")

## Paquetes
library(readr)
library(dplyr)


data_04_completo <- read_csv("CU_04_05_19_vacunacion_gripe_completo.csv") |> 
  mutate(GEOCODIGO = factor(GEOCODIGO),
         scampana = as.numeric(factor(paste0(ano, semana, sep = "-"))))
  

# data_04_agrupado <- data_04_completo |> 
#   select(-DESBDT, -ano, -semana, -nombre_zona) |> 
#   group_by(GEOCODIGO) |> 
#   summarise(across(c(n_vacunas), ~sum(.x, na.rm = TRUE)),
#             across(tmed:densidad_hab_km, ~mean(.x, na.rm = TRUE)),
#             .groups = "drop") 


## MODELO LOCO

library(mgcv)
# library(visibly) # install_github("m-clark/visibly")
# library(ggeffects)

# f <- as.formula(paste0("n_vacunas ~  ", 
#        paste0("s(", colnames(data_04)[-c(1:2)], collapse = ") + "),
#        ")"))

# f <- as.formula(
#   "n_vacunas ~ s(tmed) + s(presMax) + "
# )


f <- as.formula(
  "n_vacunas ~ GEOCODIGO + s(scampana) + s(tmed) + s(prec) + s(velmedia) + s(presMax) + s(benzene) + 
    s(co) + s(no) + s(no2) + s(nox) + s(o3) + s(pm10) + s(pm2.5) + 
    s(so2) + s(capacidad_zona) + s(prop_riesgo) + s(tasa_riesgo) + 
    s(tasa_mayores) + s(poblacion_mayores) + s(nsec) + s(t3_1) + 
    s(t1_1) + s(t2_1) + s(t2_2) + s(t4_1) + s(t4_2) + s(t4_3) + 
    s(t5_1) + s(t6_1) + s(t7_1) + s(t8_1) + s(t9_1) + s(t10_1) + 
    s(t11_1) + s(t12_1) + s(area) + s(densidad_hab_km)"
)

mod_04_gam <- data_04_completo |> 
  gam(f, data = _, family = poisson(link = log))

write_rds(mod_04_gam, "mod_04_gam.rds")

# newdata <- data_04_completo |> 
#   select(-n_vacunas, -n_citas, -nombre_zona) |> 
#   filter(ano == 2021 & semana >= 36 | ano == 2022 & semana <= 5) |> 
#   mutate(scampana = as.numeric(factor(paste0(ano, semana, sep = "-")))) |> 
#   select(-ano, -semana, -DESBDT)
# 
# write_csv(newdata, "NEWDATA.csv")

newdata <- read_csv("NEWDATA.csv")

prediction <- predict.gam(mod_04, newdata, se.fit = TRUE, type = "response")

newdata |> bind_cols(data.frame(prediction)) |> write_csv("PREDICTION.csv")

# data_04_completo |> 
#   filter(GEOCODIGO == "001") |> View()

# summary(mod_04)
# plot_gam(mod_04, main_var = tmed)
# gratia::draw(mod_04)
# plot(ggeffects::ggpredict(mod_04), facets = TRUE)
# plot.gam(mod_04)
# 
# 
# ames <- data_04 |> 
#   select(-DESBDT) |> 
#   mutate(asemana = (paste0(ano, semana, sep = "-"))) |> 
#   group_by(asemana) |> 
#   mutate(nsemana = order(asemana)) |> 
#   ungroup()


## REFERENCIAS MEJORAR

# https://peerj.com/articles/6876/


  
