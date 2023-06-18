# APP PASO 5 (SIMULACIÓN)
## Paquetes
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(bslib, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(purrr)
library(leaflet)
library(ggplot2)
library(plotly, warn.conflicts = FALSE)
library(mclust)


# SERVER
library(sf)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
COL1 <- rgb(33/255, 150/255, 243/255)

# UI
ui <- function(request) {

  fluidPage(

    theme = bs_theme(bootswatch = "flatly"),

    useShinydashboard(),

    titlePanel(title = "Simulación - CitizenLab CU 18"),




    # ... Otros elementos de la UI

   

    sidebarLayout(

      sidebarPanel(

        column(width = 6,
               uiOutput("uimodelo"),
               uiOutput("select_variable")

        )

      ),

      mainPanel(

        tabsetPanel(

                    tabPanel("Simulaciones",

                  column(width = 12,
                   DT::dataTableOutput("table_data"),
                   plotOutput("bar_plot")
            ))

        )

      )

    )

  )
  

}


# SERVER
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

  ## Read variable data
  dfvariables <- reactive({
    read_csv(paste0(carpetas()$carpeta_entrada, "/VARIABLES.csv"), 
             show_col_types = FALSE)
  })

  nsim <- reactive({dfvariables()$valor[dfvariables()$variable == "NSIM"]})

  escenario_reg <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "/ESCENARIO_REGRESION.csv"))
  })

  escenario_clus <- reactive({
    read_csv(file.path(carpetas()$carpeta_entrada, "/ESCENARIO_CLUSTER_DIST.csv"))
  })
  
  ## MODELO
  modelo_zona <- reactive({
    read_rds(file.path(carpetas()$carpeta_maestros,"/mod_glm_zona.rds"))})
  
  modelo_infra <- reactive({
    read_rds(file.path(carpetas()$carpeta_maestros,"/mod_glm_infra.rds"))})

  modelo_clus <- reactive({
    read_rds(file.path(carpetas()$carpeta_maestros,"/modelo_cluster_distritos.rds"))})

      # Render the inputs
  output$uimodelo <- renderUI({
    selectInput(
      inputId = "modelo",
      label = "Seleccionar modelo",
      choices = c("Regresión", "Cluster"), 
      selected = "Regresión"    
      )
  })


  output$select_variable <- renderUI({
    req(input$modelo)
    if(input$modelo == 'Regresión'){
    selectInput(
    inputId = "variable",
    label = "Escenario de simulación",
    choices = c('infraestructuras', 'zonas'), 
    selected = 'infraestructuras'    
  )
  }
    
  
})


  simulaciones <- reactive({
    req(input$modelo, input$variable, nsim(), modelo_clus(), modelo_infra(), modelo_zona(), escenario_reg(), escenario_clus())

    if (input$modelo == "Cluster") {
    escenariom <- escenario_clus() |> 
        summarise(across(everything(), mean)) |> 
        mutate(across(everything(), ~if_else(.x == 0, 0.1, .x)))
        
        ## Continuas
    icont <- c(61:75, 136:139)
    ncont <- colnames(escenario_clus())[!colnames(escenario_clus()) %in% colnames(escenario_clus())[icont]]

    simulacion_df <- escenariom |> 
        imap_dfc(~{
            if (.y %in% ncont) {
                rnorm(nsim(), .x, escenario_clus() |> pull(.y))
            }else{
                rpois(nsim(), .x)
            }})

    simulacion <- simulacion_df |> 
        bind_cols(cluster = predict(modelo_clus(), simulacion_df)$classification) |> 
        relocate(cluster, .before = 1)
                
        }else{
            escenario_regm <- escenario_reg() |> 
              summarise(across(everything(), mean)) |> 
              mutate(across(everything(), ~if_else(.x == 0, 0.1, .x)))

            escenario_regs <- escenario_reg() |> 
              summarise(across(everything(), sd)) |> 
              mutate(across(everything(), ~if_else(.x == 0, 0.01, .x)))

            simulacion <- escenario_regm |> 
            imap_dfc(~{
                rnorm(nsim(), .x, escenario_regs |> pull(.y))

            
            })
            if (input$variable == 'infraestructuras'){
              simulacion <- simulacion |> 
                bind_cols(evento = predict(modelo_infra(), simulacion, type = "response") > 0.5)
            } else{
              simulacion <- simulacion |> 
                bind_cols(evento = predict(modelo_zona(), simulacion, type = "response") > 0.5)
            }
        } 
        
    return(simulacion)
  })

    output$bar_plot <- renderPlot({
        req(input$modelo)
        req(simulaciones())
        if (input$modelo == "Cluster") {
        simulaciones() |> 
            ggplot(aes(cluster)) + 
            geom_bar()} else {
              simulaciones() |> 
            ggplot(aes(evento)) + 
            geom_bar()
            }
  })

  output$table_data <- DT::renderDataTable({
    req(simulaciones())
    
    datatable(simulaciones(), options=list(scrollX = TRUE))
  })
}

shinyApp(ui, server)