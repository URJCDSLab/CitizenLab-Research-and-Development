Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

## Interactividad
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)
library(waiter)

## Visualización
library(shiny)
library(ggplot2)
library(leaflet)
library(leafem)
library(plotly)
library(DT)
library(gratia)

## Manipulación
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)

## Modelos
library(sf)
library(mgcv)
library(tsibble)
library(fable)
library(gstat)
library(stars)

# si <- sessionInfo()
# 
# capture.output(print(si), file = "R_session_info.txt")
# pp <- data.frame(t(sapply(si$otherPkgs, function(x) {c(x$Package, x$Version)})))
# colnames(pp) <- c("Paquete", "Version")
# write.table(pp, "R_paquetes.txt", sep = ",", row.names = FALSE)

