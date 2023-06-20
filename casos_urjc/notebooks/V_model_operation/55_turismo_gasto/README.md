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


>Seleccionar un criterio: origen o destino
>
>Seleccionar un municipio y un origen (país)
>
>Seleccionar un mes (para el mapa)
>
>Si criterio origen, mapa de municipios coloreado según gasto medio de ese origen
>
>Si criterio destino, mapa de países coloreado según gasto en ese municipio
>
>
>Gráfico de Serie temporal de origen y destino
>
>Gráfico de barras ranking de mayor gasto por origen/destino (según criterio elegido)
>
>Tabla con los datos completos del origen o destino (según criterio elegido)


Paso 4
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Muestra detalles de modelo y predice el escenario


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_04_output&carpeta_maestros=cu_04_maestros


>Cargar el modelo xgboost entrenado
>
>Mostrar gráfico de importancia de variables
>
>Cargar escenario
>
>Realizar predicción del escenario
>
>Si el tipo de escenario es origen, mostrar en mapa los municipios coloreados según
la predicción de gasto medio
>
>Si el tipo de escenario es destino, mostrar en mapa los países coloreados según la
predicción de gasto medio
>
>Si el escenario tiene más de un mes, mostrar gráfico serie temporal
>
>Mostrar tabla con el escenario añadiendo la predicción


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

>Se carga el escenario de turistas
>
>Se calculan los parámetros de las distribuciones
>
>Se simula el escenario NSIM veces
>
>Se predice el gasto para cada simulación
>
>Se muestra resumen de las variables: histogramas de gasto y de visitantes
>y resumen numérico con summarytools::descr() para cada origen/destino
>según parámetro configuración



