## PROYECCIÓN CAMPAÑA

setwd("/Users/emilio.lcano/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/04_vacunas")

source("LIBRERIAS.R")

source("VARIABLES.R")



## ZONAS
file_name <- "ZONAS.json"
ZONAS <- st_read(file_name)

## ESCENARIO
file_name <- "ACTUAL_TS.csv"
ESCENARIO <- read_csv(file_name)

## ESTIMACIÓN MODELO
# Preparación objeto tsible
actualts <- ESCENARIO |>
  mutate(tsweek = make_yearweek(ano, semana)) |>
  as_tsibble(key = GEOCODIGO, index = tsweek) |>
  fill_gaps()

# Ajuste modelo ARIMA
mod_04_ts <- actualts |>  
  model(arima = ARIMA(n_vacunas))

## PREDICCIÓN
h <- as.numeric(make_yearweek(min(actualts$ano) + 1, SEMANA_FIN) - min(actualts$tsweek))

prediction <- mod_04_ts |> forecast(h = h) |> hilo(level = CONF.LEVEL)

## ARREGLAR DATOS Y GUARDAR

PREDICCION <- prediction |> 
  mutate(across(last_col(), ~paste(.x$lower, .x$upper, sep = ";"))) |> 
  separate(`90%`, into = c("lower", "upper"), sep = ";") |> 
  as_tibble() |> 
  mutate(n_vacunas = .mean,
         ano = year(tsweek),
         semana = isoweek(tsweek),
         dato = "Predicción") |> 
  select(GEOCODIGO, ano, semana, n_vacunas, lower, upper, dato) |> 
  bind_rows(ESCENARIO |> 
              mutate(dato = "Actual"))

write_csv(PREDICCION, "PREDICCION_TS.csv")

## VISUALIZACIÓN

## Se muestra solo la variable respuesta

## MAPA

ldata <- ZONAS |> 
  left_join(PREDICCION, by = c("GEOCODIGO"), 
            multiple = "all") |> 
  group_by(GEOCODIGO, DESBDT) |> 
  summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop")

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
            title = "Número de vacunas<br/>Predicción resto campaña",
            labFormat = labelFormat(big.mark = " "),
            opacity = 1
  )

## SERIE

if(is.na(ZONA)){
  sdata <- PREDICCION |> 
    mutate(ano_semana = paste0(ano, "-", semana),
           fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w'))) |> 
    group_by(fecha, dato) |> 
    summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop")
  NZONA <- NA
  
} else{
  sdata <- PREDICCION |> 
    filter(GEOCODIGO == ZONA) |> 
    # select(GEOCODIGO, tsweek, .mean) |> 
    mutate(ano_semana = paste0(ano, "-", semana),
           fecha = as.Date(parse_date_time(paste(ano, semana, 1, sep="/"),'Y/W/w'))) |> 
    group_by(fecha, dato) |> 
    summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop")
  NZONA <- ZONAS |> 
    filter(GEOCODIGO == ZONA) |> 
    pull(DESBDT)
}


p <- sdata |> 
  ggplot() +
  aes(x = fecha,
      y = n_vacunas) +
  geom_line(aes(col = dato)) +
  labs(x = "Semana",
       y = "Total vacunas",
       col = "Tipo de dato") +
  theme_bw() +
  theme(plot.margin = unit(c(1.2, 1, 1, 1), "cm"))
ggplotly(p) |> 
  layout(title = list(text = paste0("Predicción resto campaña ", 
                        min(year(sdata$fecha)), "/", 
                        min(year(sdata$fecha)) + 1,
                        "<br><sup>", 
                        if_else(is.na(ZONA), "Total zonas", 
                            paste0("Zona ", ZONA,
                                   " (", NZONA, ")")),
                        "</sup>"), 
                      x = 0.05))

## TABLA

PREDICCION |> right_join(tibble(ZONAS) |> select(2:3), by = "GEOCODIGO") |> 
  group_by(GEOCODIGO, DESBDT, dato) |> 
  summarise(n_vacunas = sum(n_vacunas, na.rm = TRUE), .groups = "drop") |> 
  datatable(rownames = FALSE, colnames = c("Código zona", "", "Tipo de dato", "Total predicción vacunas escenario")) |> 
  formatRound(4, dec.mark = ",", mark = ".", digits = 0)



