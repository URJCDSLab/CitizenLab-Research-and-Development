Ejecución del caso CU45_Planificación y promoción del destino en base a los patrones en origen de los turistas	
========================================================================


Paso 1
------

Tipo: Parametrización/configuración

Qué hace: Carga de ficheros


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_45_step_01_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_45_step_01_input&carpeta_salida=cu_45_step_01_output&carpeta_maestros=cu_45_maestros


Paso 2
------

Tipo: Parametrización/configuración

Qué hace: Parámetros de configuración

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_45_step_02_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_45_step_01_output&carpeta_salida=cu_45_step_02_output&carpeta_maestros=cu_45_maestros



Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza número de turistas por origen en mapa

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_45_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_45_step_02_output&carpeta_salida=cu_45_step_03_output&carpeta_maestros=cu_45_maestros



Paso 4
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Clasifica municipios en clusters y visualiza los resultados


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_45_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_45_step_02_output&carpeta_salida=cu_45_step_04_output&carpeta_maestros=cu_45_maestros




Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Estimar modelo de regresión para predecir cluster


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_45_step_05_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_45_step_04_output&carpeta_salida=cu_45_step_05_output&carpeta_maestros=cu_45_maestros




>Se cargan los datos con los cluster, el modelo de cluster y el escenario a predecir
>
>Se estima el modelo de regresión multinomial y se muestran coeficientes.
>
>Se predice el cluster del escenario
>
>Se muestra la caracterización del cluster predicho: valores medios de las variables
>y mapa con los municipios que lo componen


Paso 6
------

Tipo:Procesamiento de datos

Qué hace: Estimar modelo espacial para interpolación de krigging

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_06_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_05_output&carpeta_salida=cu_04_step_06_output&carpeta_maestros=cu_04_maestros

>Se cargan los parámetros de configuración: objetivo
>
>Se arreglan los datos para el modelo
>
>Se estima el modelo y se guarda en output

Paso 7
------

Tipo: Visualización de resultados

Qué hace: Representar el modelo espacial de oportunidades

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_06_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_05_output&carpeta_salida=cu_04_step_06_output&carpeta_maestros=cu_04_maestros

>Se cargan los datos del modelo
>
>Se representa el modelo en el mapa
>


