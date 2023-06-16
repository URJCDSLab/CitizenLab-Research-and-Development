Ejecución del caso CU25_Modelo de gestión de Lista de Espera Quirúrgica
========================================================================




Paso 1
------

Tipo: Parametrización/configuración

Qué hace: Carga de ficheros





Terminal:

````
$ Rscript -e 'shiny::runApp("cu_25_step_01_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_25_step_01_input&carpeta_salida=cu_25_step_01_output&carpeta_maestros=cu_25_maestros


Paso 2
------

Tipo: Parametrización/configuración

Qué hace: Parámetros de configuración

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_25_step_02_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_25_step_01_output&carpeta_salida=cu_25_step_02_output&carpeta_maestros=cu_25_maestros



Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza histórico listas de espera

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_25_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_25_step_02_output&carpeta_salida=cu_25_step_03_output&carpeta_maestros=cu_25_maestros



Paso 4
------

Tipo: Visualización de resultados

Qué hace: Predice el escenario para las próximas semanas


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_25_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_25_step_02_output&carpeta_salida=cu_25_step_04_output&carpeta_maestros=cu_25_maestros




Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Simulación listas de espera y visualización


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_25_step_05_app.R", port = 4000)'
````


Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_25_step_04_output&carpeta_salida=cu_25_step_05_output&carpeta_maestros=cu_25_maestros

Paso 6
------

Tipo: Proceso

Qué hace: Predicción y simulación de la cola para todas las zonas y especialidades


Terminal:

````
$ Rscript cu_25_step_06_script.R cu_25_step_05_output cu_25_step_06_output cu_25_maestros
````



Paso 7
------

Tipo: Visualización

Qué hace: Visualización del mapa de riesgo


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_25_step_07_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_25_step_06_output&carpeta_salida=cu_25_step_07_output&carpeta_maestros=cu_25_maestros


