########
# APP PASO 2 (PARÁMETROS DE USUARIO) CU 18 (Comportamiento Infra. Eventos extremos)
########

## Paquetes ----


## UI
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(leaflet)
library(leafpop)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)

# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(janitor)
library(cluster)
library(DT)


ui <- function(request) {

  fluidPage(

    theme = bs_theme(bootswatch = "flatly"),

    useShinydashboard(),

    titlePanel(title = "Clustering - CitizenLab CU 34"),

    # ... Otros elementos de la UI
    sidebarLayout(
      sidebarPanel(
        column(width = 6,
            uiOutput("uimunicipio")
        )
      ),
      mainPanel(
        tabsetPanel(
            tabPanel("Tabla resumen cluster",
                column(width = 12, dataTableOutput("cluster_table"))
            ),
            tabPanel("Gráfica cluster",
                column(width = 12, plotOutput("cluster_chart"))
            ),
            tabPanel("Mapa",
                column(width = 12, leafletOutput("map"))
            ),
            tabPanel("Detalle municipio",
                column(width = 12, dataTableOutput("municipio"))
            )
        )
      )
    )
  )
}


server <- function(input, output, session) {
  ## Reactives ----
  carpetas <- reactive({

    carpeta_entrada <- getQueryString()$carpeta_entrada
    carpeta_salida <- getQueryString()$carpeta_salida
    carpeta_maestros <- getQueryString()$carpeta_maestros
    if(any(is.null(carpeta_entrada),
           is.null(carpeta_salida),
           is.null(carpeta_maestros))){
      confirmSweetAlert(
        session = session,
        inputId = "error_carpetas_faltan",
        title = "Error",
        text = "Revise la url, alguna carpeta requerida en el caso no se ha especificado (carpeta_entrada, carpeta_salida, carpeta_maestros). La aplicación se cerrará.",
        type = "error",
        btn_labels = c("", "Cerrar"),
        btn_colors = c("white", "red")
      )
      warning("Revise la url, alguna carpeta requerida en el caso no se ha especificado (carpeta_entrada, carpeta_salida, carpeta_maestros).")
      invisible(NULL)
    } else if(!all(file.exists(carpeta_entrada),
                   file.exists(carpeta_salida),
                   file.exists(carpeta_maestros))){
      confirmSweetAlert(
        session = session,
        inputId = "error_carpetas_existen",
        title = "Error",
        text = "Revise la url, alguna carpeta requerida en el caso no existe. La aplicación se cerrará.",
        type = "error",
        btn_labels = c("", "Cerrar"),
        btn_colors = c("white", "red")
      )
      warning("Revise la url, alguna carpeta requerida en el caso no existe.")
      invisible(NULL)
    } else{
      return(invisible(list(carpeta_entrada = carpeta_entrada,
                            carpeta_salida = carpeta_salida,
                            carpeta_maestros = carpeta_maestros)))
    }
  })

  ## observers ----

  observeEvent(input$error_carpetas_faltan,{
    stopApp()
  })

  observeEvent(input$error_carpetas_existen,{
    stopApp()
  })

  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"),
             show_col_types = FALSE)
  })

  # Dataframes

  dfvariables <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "VARIABLES.csv"), show_col_types = FALSE)
  })

  dfservicios <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "SERVICIOS.csv"), show_col_types = FALSE)
  })

  dfescenarios <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "ESCENARIOS.csv"), show_col_types = FALSE)
  })

  sfsecciones <- reactive({
    read_sf(file.path(carpetas()$carpeta_entrada, "SECCIONES.json"))
  })

  dfcluster <- reactive({
    dfservicios() |> drop_na() |> remove_constant()
  })

  dfzcluster <- reactive({
    dfcluster() |> select(-c(Fecha:CSEC, NSEC)) |> scale()
  })

  dfwithcluster <- reactive({
    km <- model_cluster()
    dfcluster() |>
        mutate(cluster = km$clustering) |>
        relocate(cluster, NSEC, .before = 1)
  })

  model_cluster <- reactive({
    km <- clara(dfzcluster(), 10)
    print(paste0(carpetas()$carpeta_salida, "/MODELO_CLUSTER.rds"))
    write_rds(km, paste0(carpetas()$carpeta_salida, "/MODELO_CLUSTER.rds"))
    km
  })

  ## render outputs ----

  ## dynamic UI ----

  # Render the inputs

  output$cluster_table <- renderDataTable({
    datatable(dfwithcluster() |>
        group_by(cluster) |>
        summarise(across(Futbol:densidad_hab_km2, median)),
              options = list(scrollX = TRUE))
  })

  output$cluster_chart <- renderPlot({
    fviz_cluster(model_cluster(), geom = "point", alpha = 0.5) + theme_bw()
  })

  output$municipio <- renderDataTable({
    req(input$pimunicipio)
    municipio <- dfwithcluster() |> filter(CMUN == input$pimunicipio)
    datatable(municipio,
              options = list(scrollX = TRUE))
  })

  output$uimunicipio <- renderUI({
    selectizeInput(
      inputId = "pimunicipio",
      label = "Municipio a visualizar (Solo detalle en detalle de municipio)",
      selected = dfcluster()$CMUN[1],
      choices = unique(dfcluster()$CMUN)
    )
  })

  # Render map
  output$map <- renderLeaflet({
    plot_df <- dfwithcluster() |>
        group_by(NSEC, CMUN, CDIS, CSEC, cluster) |>
        summarise(n = n()) |>
        rowwise() |>
        slice_max(n) |>
        ungroup() |> 
      mutate(cluster = factor(cluster))

    plot_df <- sfsecciones() |> inner_join(plot_df, by=c("CSEC", "CDIS", "CMUN"))
    pal <- colorFactor(palette = "viridis", domain = plot_df$cluster)
    plot_df |>
    leaflet() |>
      addTiles() |>
      addPolygons(fillColor = ~pal(plot_df$cluster),
                  color = "black",
                  weight = 1,
                  smoothFactor = 0.5,
                  fillOpacity = 1,
                  label = ~NSEC) |> 
                  # popup = popupTable(plot_df, c("futbol", "nservicios", "capacidad"))) |>
                  # label = ~paste0("Distrito ", CDIS, ";Municipio ", CMUN)) |>
        addLegend("bottomright",
            pal = pal,
            values = ~plot_df$cluster,
            title = "cluster",
            labFormat = labelFormat(big.mark = " ")
        )
  })


}

shinyApp(ui, server)