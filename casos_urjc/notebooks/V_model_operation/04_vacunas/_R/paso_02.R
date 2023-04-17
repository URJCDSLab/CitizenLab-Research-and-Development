## ZONAS SANITARIAS E HISTÓRICO DE VACUNACIÓN

setwd("/Users/emilio.lcano/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/04_vacunas")

source("LIBRERIAS.R")

source("VARIABLES.R")

file_name <- "CU_04_05_01_zonasgeo.json"
ZONAS <- st_read(file_name)

file_name <- "CU_04_05_16_vacunacion_gripe.csv"
HISTORICO <- read_csv(file_name)


## MAPA

ldata <- ZONAS |> 
  left_join(HISTORICO, by = c("GEOCODIGO", "DESBDT"), 
            multiple = "all") |> 
  filter((ano == ANO & semana >= SEMANA_INICIO) | (ano == ANO + 1 & semana <= SEMANA_FIN)) |> 
  group_by(GEOCODIGO, DESBDT) |> 
  summarise(n_vacunas = sum(n_vacunas), .groups = "drop")

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
              label = ~paste0(n_vacunas, " vacunas")) |> 
  addLegend("bottomright", 
            pal = pal, 
            values = ~n_vacunas,
            title = "Número de vacunas",
            labFormat = labelFormat(big.mark = " "),
            opacity = 1
  )

## SERIE

if(is.na(ZONA)){
  sdata <- HISTORICO |> 
    filter((ano == ANO & semana >= SEMANA_INICIO) | (ano == ANO + 1 & semana <= SEMANA_FIN)) |> 
    group_by(ano, semana) |> 
    summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop")
  NZONA <- NA
} else{
  sdata <- HISTORICO |> 
    filter((ano == ANO & semana >= SEMANA_INICIO) | (ano == ANO + 1 & semana <= SEMANA_FIN),
           GEOCODIGO == ZONA)
  NZONA <- ZONAS |> 
    filter(GEOCODIGO == ZONA) |> 
    pull(DESBDT)
}
sdata <- sdata |> 
  mutate(ano_semana = paste0(ano, "-", semana),
         fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w')))

sdata |> 
  ggplot() +
  aes(x = fecha,
      y = n_vacunas) +
  geom_line(col = COL1) +
  labs(title = paste0("Histórico campaña ", ANO, "/", ANO + 1),
       subtitle = if_else(is.na(ZONA), "Total zonas", 
                          paste0("Zona ", ZONA,
                                 " (", NZONA, ")")),
       x = "Semana",
       y = "Total vacunas") +
  scale_x_date(date_breaks = "1 month",
               date_minor_breaks = "1 week",
               labels = function(x) month(x, label = TRUE)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


## TABLA

HISTORICO |> 
  filter((ano == ANO & semana >= SEMANA_INICIO) | (ano == ANO + 1 & semana <= SEMANA_FIN)) |> 
  group_by(GEOCODIGO, DESBDT) |> 
  summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop") |> 
  datatable(rownames = FALSE, colnames = c("Código zona", "Nombre zona", "Total vacunas campaña")) |> 
            formatRound(3, dec.mark = ",", mark = ".", digits = 0)


