## PROYECCIÓN CAMPAÑA

setwd("/Users/emilio.lcano/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/04_vacunas")

source("LIBRERIAS.R")

source("VARIABLES.R")



## ZONAS
file_name <- "ZONAS.json"
ZONAS <- st_read(file_name)

## ESCENARIO
file_name <- "NEWDATA.csv"
ESCENARIO <- read_csv(file_name)

## MODELO
modelo <- read_rds("mod_04.rds")

## PREDICCIÓN
file_name <- "PREDICTION.csv"
prediction <- predict.gam(modelo, ESCENARIO, se.fit = TRUE, type = "response")
ESCENARIO.pred <- ESCENARIO |> bind_cols(data.frame(prediction) )
ESCENARIO.pred |> write_csv(file_name)


## VISUALIZACIÓN

## Se muestra solo la variable respuesta

## MAPA

ldata <- ZONAS |> 
  left_join(ESCENARIO.pred, by = c("GEOCODIGO"), 
            multiple = "all") |> 
  # filter((ano == ANO & semana >= SEMANA_INICIO) | (ano == ANO + 1 & semana <= SEMANA_FIN)) |> 
  group_by(GEOCODIGO, DESBDT) |> 
  summarise(n_vacunas = sum(fit, na.rm = TRUE), .groups = "drop")

# COL1 <- rgb(33/255, 150/255, 243/255)
pal <- colorNumeric(palette = "Blues", 
                    domain = ldata$n_vacunas)


ldata |> 
  leaflet() |>
  addTiles() |> 
  addPolygons(color = "#444444", 
              weight = 1, 
              smoothFactor = 0.5,
              fillOpacity = 1,
              fillColor = ~pal(n_vacunas),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0(DESBDT, " (", GEOCODIGO, ")"),
              label = ~paste0(round(n_vacunas), " vacunas")) |> 
  addLegend("bottomright", 
            pal = pal, 
            values = ~n_vacunas,
            title = "Número de vacunas<br/>Predicción escenario",
            labFormat = labelFormat(big.mark = " "),
            opacity = 1
  )

## SERIE

if(is.na(ZONA)){
  sdata <- ESCENARIO.pred |> 
    group_by(scampana) |> 
    summarise(fit = sum(fit, na.rm = TRUE), .groups = "drop")
  NZONA <- NA
} else{
  sdata <- ESCENARIO.pred |> 
    filter(GEOCODIGO == ZONA)
  NZONA <- ZONAS |> 
    filter(GEOCODIGO == ZONA) |> 
    pull(DESBDT)
}

sdata |> 
  ggplot() +
  aes(x = scampana,
      y = fit) +
  geom_line(col = COL1) +
  labs(title = paste0("Predicción campaña ", ANO, "/", ANO + 1),
       subtitle = if_else(is.na(ZONA), "Total zonas", 
                          paste0("Zona ", ZONA,
                                 " (", NZONA, ")")),
       x = "Semana",
       y = "Total vacunas") +
  theme_bw() 


## TABLA

ESCENARIO.pred |> right_join(tibble(ZONAS) |> select(2:3), by = "GEOCODIGO") |> 
  group_by(GEOCODIGO, DESBDT) |> 
  summarise(fit = sum(fit, na.rm = TRUE), .groups = "drop") |> 
  datatable(rownames = FALSE, colnames = c("Código zona", "", "Total predicción vacunas escenario")) |> 
  formatRound(3, dec.mark = ",", mark = ".", digits = 0)




# 
#   ## MAPA
#   ldata <- ZONAS |> 
#     left_join(ESCENARIO.pred, by = c("GEOCODIGO"), 
#               multiple = "all") |> 
#     # filter((ano == ANO & semana >= SEMANA_INICIO) | (ano == ANO + 1 & semana <= SEMANA_FIN)) |> 
#     group_by(GEOCODIGO) |> 
#     summarise(across(fit:se.fit, ~sum(.x, na.rm = TRUE)), 
#               across(tmed:so2, ~mean(.x, na.rm = TRUE)), 
#               .groups = "drop")
#   pal <- colorNumeric(palette = "Blues", 
#                       domain = ldata |> pull(fit))
#   ldata |> 
#     leaflet() |>
#     addTiles() |> 
#     addPolygons(color = "#444444", 
#                 weight = 1, 
#                 smoothFactor = 0.5,
#                 fillOpacity = 1,
#                 fillColor = ~pal(fit),
#                 highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                     bringToFront = TRUE),
#                 popup = ~paste0(GEOCODIGO),
#                 label = ~paste0(fit, " vacunas")) |> 
#     addLegend("bottomright", 
#               pal = pal, 
#               values = ~fit,
#               title = paste0("Predicción vacunas: "),
#               labFormat = labelFormat(big.mark = " "),
#               opacity = 1
#     )
#   
#   ## GRÁFICO
#   if(is.na(ZONA)){
#     sdata <- HISTORICO |> 
#       filter((ano == ANO & semana >= SEMANA_INICIO) | (ano == ANO + 1 & semana <= SEMANA_FIN)) |> 
#       group_by(ano, semana) |> 
#       summarise("{PREDICTOR}" := mean(eval(parse(text = PREDICTOR)), na.rm = TRUE), .groups = "drop")
#     NZONA <- NA
#   } else{
#     sdata <- HISTORICO |> 
#       filter((ano == ANO & semana >= SEMANA_INICIO) | (ano == ANO + 1 & semana <= SEMANA_FIN),
#              GEOCODIGO == ZONA)
#     NZONA <- ZONAS |> 
#       filter(GEOCODIGO == ZONA) |> 
#       pull(DESBDT)
#   }
#   sdata <- sdata |> 
#     mutate(ano_semana = paste0(ano, "-", semana),
#            fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))
#   
#   sdata |> 
#     ggplot() +
#     aes(x = fecha,
#         y = eval(parse(text = PREDICTOR))) +
#     geom_line(col = COL1) +
#     labs(title = paste0("Histórico campaña ", ANO, "/", ANO + 1),
#          subtitle = if_else(is.na(ZONA), "Media zonas", 
#                             paste0("Zona ", ZONA,
#                                    " (", NZONA, ")")),
#          x = "Semana",
#          y = paste("Media de ", PREDICTOR)) +
#     scale_x_date(date_breaks = "1 month",
#                  date_minor_breaks = "1 week",
#                  labels = function(x) month(x, label = TRUE)) +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
#   
#   ## TABLA
#   sdata |> 
#     select(-ano_semana, -fecha) |> 
#     datatable(rownames = FALSE, 
#               colnames = c("Año", "Semana", 
#                            PREDICTOR)) |> 
#     formatRound(3, dec.mark = ",", mark = ".", digits = 2)
#   
#   
#   
#   
#   
# } else if (PREDICTOR %in% colnames(ESCUCHA)){
#   
#   ## MAPA
#   ## NO HAY MAPA PUESTO QUE LOS DATOS DE ESCUCHA NO ESTÁN GEOLOCALIZADOS
#   
#   ## GRÁFICO
#   sdata <- ESCUCHA |> 
#     filter((ano == ANO & semana >= SEMANA_INICIO) | (ano == ANO + 1 & semana <= SEMANA_FIN)) |> 
#     mutate(ano_semana = paste0(ano, "-", semana),
#            fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))
#   
#   NZONA <- NA
#   
#   sdata |> 
#     ggplot() +
#     aes(x = fecha,
#         y = eval(parse(text = PREDICTOR))) +
#     geom_line(col = COL1) +
#     labs(title = paste0("Histórico campaña ", ANO, "/", ANO + 1),
#          subtitle = "Total zonas",
#          x = "Semana",
#          y = paste("Total de ", PREDICTOR)) +
#     scale_x_date(date_breaks = "1 month",
#                  date_minor_breaks = "1 week",
#                  labels = function(x) month(x, label = TRUE)) +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
#   
#   ## TABLA
#   sdata |> 
#     select(-ano_semana, -fecha) |> 
#     datatable(rownames = FALSE, 
#               colnames = c("Año", "Semana", "Tuits gripe", "Interés gripe")) |> 
#     formatRound(4, dec.mark = ",", mark = ".", digits = 0)
#   
#   
#   
# } else if(PREDICTOR %in% colnames(INDICADORES)){
#   
#   ## MAPA
#   ldata <- ZONAS |> 
#     left_join(INDICADORES, by = c("GEOCODIGO", "DESBDT"), 
#               multiple = "all") 
#   pal <- colorNumeric(palette = "Blues", 
#                       domain = ldata |> pull(PREDICTOR))
#   ldata |> 
#     leaflet() |>
#     addTiles() |> 
#     addPolygons(color = "#444444", 
#                 weight = 1, 
#                 smoothFactor = 0.5,
#                 fillOpacity = 1,
#                 fillColor = ~pal(eval(parse(text = PREDICTOR))),
#                 highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                     bringToFront = TRUE),
#                 popup = ~paste0(DESBDT, " (", GEOCODIGO, ")"),
#                 label = ~paste0(round(eval(parse(text = PREDICTOR)), 2)*100)) |> 
#     addLegend("bottomright", 
#               pal = pal, 
#               values = ~eval(parse(text = PREDICTOR)),
#               title = paste0("Predictor: ", PREDICTOR),
#               labFormat = labelFormat(big.mark = " "),
#               opacity = 1
#     )
#   
#   ## SERIE
#   ## No hay serie puesto que los indicadores son estáticos
#   ## Se muestra gráfico de barras con el top 10
#   gdata <- ZONAS |> tibble() |> 
#     left_join(INDICADORES, by = c("GEOCODIGO", "DESBDT")) |> 
#     slice_max(desc(eval(parse(text = PREDICTOR))), n = 10)
#   gdata |> 
#     ggplot() + 
#     aes(y = fct_reorder(DESBDT, eval(parse(text = PREDICTOR))),
#         x =  eval(parse(text = PREDICTOR))) + 
#     geom_col(fill = COL1) +
#     theme_bw() +
#     labs(title = paste0("Top 10 zonas por ", PREDICTOR),
#          # subtitle = "Total zonas",
#          x = PREDICTOR,
#          y = "")
#   
#   ## TABLA
#   
#   tdata <- ZONAS |> tibble() |> 
#     left_join(INDICADORES, by = c("GEOCODIGO", "DESBDT"), 
#               multiple = "all") 
#   
#   tdata |> 
#     datatable(rownames = FALSE) |> 
#     formatRound(5:ncol(tdata), dec.mark = ",", mark = ".", digits = 2)
#   
# }
# 
# 
# 
# 
# 
# 
