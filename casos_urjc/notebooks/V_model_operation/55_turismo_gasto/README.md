Ejecución del caso CU55_Modelo agregado de estimación del gasto medio por turista
========================================================================



=======

Paso 1
------

Tipo: Parametrización/configuración

Qué hace: Carga de ficheros


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_55_step_01_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_55_step_01_input&carpeta_salida=cu_55_step_01_output&carpeta_maestros=cu_55_maestros


Paso 2
------

Tipo: Parametrización/configuración

Qué hace: Parámetros de configuración

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_55_step_02_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_55_step_01_output&carpeta_salida=cu_55_step_02_output&carpeta_maestros=cu_55_maestros



Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza gasto medio de turistas por origen

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_55_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_55_step_02_output&carpeta_salida=cu_55_step_03_output&carpeta_maestros=cu_55_maestros


Paso 4
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Muestra detalles de modelo y predice el escenario


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_55_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_55_step_02_output&carpeta_salida=cu_55_step_04_output&carpeta_maestros=cu_55_maestros




Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Simulación de gasto de turistas


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_55_step_05_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_55_step_02_output&carpeta_salida=cu_55_step_05_output&carpeta_maestros=cu_55_maestros





