# setwd("~/academico/gh_repos/__transferencia/citizenlab/CitizenLab-Research-and-Development/casos_urjc/notebooks/V_model_operation/04_vacunas")

# shiny::runApp("app_04_01.R", port = 4001)
shiny::runApp("cu_04_step_02_app.R", port = 4000, launch.browser = FALSE)
shiny::runApp("cu_04_step_06_app.R", port = 4001, launch.browser = FALSE)
# (6) http://127.0.0.1:4001/?carpeta_entrada=cu_04_step_05_output&carpeta_salida=cu_04_step_06_output&carpeta_maestros=cu_04_maestros
profvis::profvis(shiny::runApp("cu_04_step_02_app.R", port = 4000, launch.browser = FALSE))

# (1) http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_01_input&carpeta_salida=cu_04_step_01_output&carpeta_maestros=cu_04_maestros
# (2) http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_01_output&carpeta_salida=cu_04_step_02_output&carpeta_maestros=cu_04_maestros
# (3) http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_03_output&carpeta_maestros=cu_04_maestros
# (4) http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_04_output&carpeta_maestros=cu_04_maestros
# (6) http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_05_output&carpeta_salida=cu_04_step_06_output&carpeta_maestros=cu_04_maestros
# (8) http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_07_output&carpeta_salida=cu_04_step_08_output&carpeta_maestros=cu_04_maestros

# run_with_themer(shinyApp(ui, server))

# bootswatch_themes()