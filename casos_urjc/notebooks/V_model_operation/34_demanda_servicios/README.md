CU34_Predicción de demanda de servicios
========================================================================


Paso 1
------

Tipo: Parametrización/configuración

Qué hace: Carga de ficheros


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_34_step_01_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_34_step_01_input&carpeta_salida=cu_34_step_01_output&carpeta_maestros=cu_34_maestros


Paso 2
------

Tipo: Parametrización/configuración

Qué hace: Parámetros de configuración

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_34_step_02_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_34_step_01_output&carpeta_salida=cu_34_step_02_output&carpeta_maestros=cu_34_maestros



Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza datos de servicios

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_34_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_34_step_02_output&carpeta_salida=cu_34_step_03_output&carpeta_maestros=cu_34_maestros



Paso 4
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Encuentra clusters y los visualiza


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_34_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_34_step_02_output&carpeta_salida=cu_34_step_04_output&carpeta_maestros=cu_34_maestros



Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Cargar escenarios y predecir su cluster.


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_34_step_05_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_34_step_02_output&carpeta_salida=cu_34_step_05_output&carpeta_maestros=cu_34_maestros





Paso 6
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Simulación de servicios

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_34_step_06_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_34_step_02_output&carpeta_salida=cu_34_step_06_output&carpeta_maestros=cu_34_maestros




