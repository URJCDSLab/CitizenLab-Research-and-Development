Ejecución del caso
==================



Paso 1
------

Tipo: Parametrización/configuración`

Qué hace: Carga de ficheros

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_01_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_01_input&carpeta_salida=cu_04_step_01_output&carpeta_maestros=cu_04_maestros


Paso 2
------

Tipo: Parametrización/configuración

Qué hace: Parámetros de configuración

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_02_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_01_output&carpeta_salida=cu_04_step_02_output&carpeta_maestros=cu_04_maestros


Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza histórico de vacunación y predictores

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_03_output&carpeta_maestros=cu_04_maestros


Paso 4
------

Tipo: Visualización de resultados + Procesamiento de datos

Qué hace: Predecir próxima campaña y base a histórico y modelo, y visualizarlo. 

NOTA: el modelo ya está guardado y el procesamiento tarda muy poco, por eso no se hace otro notebook, se hace la predicción desde el propio interfaz web.

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_04_output&carpeta_maestros=cu_04_maestros


Paso 5
------

Tipo: Procesamiento de datos

Qué hace: Ajustar modelos y hacer predicciones resto campaña actual


NOTA: el orden de los argumentos es carpeta_entrada, carpeta_salida, carpeta_maestros

Terminal:

````
$ Rscript cu_04_step_05_script.R cu_04_step_04_output cu_04_step_05_output cu_04_maestros
````

Paso 6
------

Tipo: Visualización de resultados

Qué hace: Visualizar predicción resto de campaña actual

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_06_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_05_output&carpeta_salida=cu_04_step_06_output&carpeta_maestros=cu_04_maestros


Paso 7
------

Tipo: Procesamiento de datos

NOTA: el orden de los argumentos es carpeta_entrada, carpeta_salida, carpeta_maestros

Terminal:

````
$ Rscript cu_04_step_07_script.R cu_04_step_05_output cu_04_step_07_output cu_04_maestros
````

Paso 8
------

Tipo: Visualización de resultados

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_08_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_07_output&carpeta_salida=cu_04_step_08_output&carpeta_maestros=cu_04_maestros



