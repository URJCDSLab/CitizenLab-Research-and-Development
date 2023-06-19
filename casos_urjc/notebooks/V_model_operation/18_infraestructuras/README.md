Ejecución del caso CU18_Comportamienta Infra. Eventos extremos			
========================================================================



Paso 1
------

Tipo: Parametrización/configuración

Qué hace: Carga de ficheros



Terminal:

````
$ Rscript -e 'shiny::runApp("cu_18_step_01_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_18_step_01_input&carpeta_salida=cu_18_step_01_output&carpeta_maestros=cu_18_maestros


Paso 2
------

Tipo: Parametrización/configuración

Qué hace: Parámetros de configuración

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_18_step_02_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_18_step_01_output&carpeta_salida=cu_18_step_02_output&carpeta_maestros=cu_18_maestros



Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza infraestructuras en el mapa y en forma de tabla

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_18_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_18_step_02_output&carpeta_salida=cu_18_step_03_output&carpeta_maestros=cu_18_maestros






Paso 4
------

Tipo: Visualización de resultados

Qué hace: Visualizar la clasificación de infraestructuras guardada


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_18_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_18_step_02_output&carpeta_salida=cu_18_step_04_output&carpeta_maestros=cu_18_maestros





Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Visualizar modelo GLM y predecir escenarios


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_18_step_05_app.R", port = 4000)'
````

Navegador:
http://127.0.0.1:4000/?carpeta_entrada=cu_18_step_02_output&carpeta_salida=cu_18_step_05_output&carpeta_maestros=cu_18_maestros




Paso 6
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Simular miles de valores según escenario y visualizar resumen del resultado

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_18_step_06_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_18_step_02_output&carpeta_salida=cu_18_step_06_output&carpeta_maestros=cu_18_maestros




