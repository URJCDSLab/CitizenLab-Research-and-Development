## Ejemplos CU 53

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(plotly)

inversiones <- read_csv("cu_53_step_01_input/CU_53_05_05_inversiones_cm.csv")


## Datos para la tabla de inversiones
inversiones |> pivot_wider(names_from = "grupo", values_from = "inversion") |> 
  datatable()

spi <- read_csv("cu_53_step_01_input/CU_53_05_02_01_spi.csv")  
spi_meta <- read_csv("cu_53_step_01_input/CU_53_05_02_02_spi_metadata.csv")

## Filtro año para mapa
ano <- 2017
frole <- "SPI" 

## Variables a seleccionar
fvars <- spi_meta |> 
  filter(role == frole) |> 
  pull(id_var)

## Un valor por país para un año, para el mapa
spi |> filter(spiyear == 2017) |> 
  select(country, spicountrycode, all_of(fvars))

## Serie temporal

p <- inversiones |> 
  ggplot(aes(x = anyo, y = inversion, col = grupo)) +
  geom_line()
  ggplotly(p)

paises <- c("World", "Spain", "Germany", "Italy")

vartoplot <- "score_spi"
spi |> filter(country %in% paises) |> 
  ggplot(aes_string(x = "spiyear", y = vartoplot, col = "country")) +
  geom_line()


