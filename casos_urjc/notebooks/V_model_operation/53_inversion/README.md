Ejecución del caso CU53_impacto de las políticas de inversión en sanidad, infraestructuras y promoción turística en el SPI
========================================================================


Paso 1
------

Tipo: Parametrización/configuración

Qué hace: Carga de ficheros


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_53_step_01_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_53_step_01_input&carpeta_salida=cu_53_step_01_output&carpeta_maestros=cu_53_maestros


Paso 2
------

Tipo: Parametrización/configuración

Qué hace: Parámetros de configuración

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_53_step_02_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_53_step_01_output&carpeta_salida=cu_53_step_02_output&carpeta_maestros=cu_53_maestros



Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza SPI en el mapa y en forma de tabla

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_53_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_53_step_02_output&carpeta_salida=cu_53_step_03_output&carpeta_maestros=cu_53_maestros



Paso 4
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Estima el modelo de regresión y visualiza los resultados


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_53_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_53_step_02_output&carpeta_salida=cu_53_step_04_output&carpeta_maestros=cu_53_maestros




Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Estimar SPI con inversiones y proyectar serie temporal


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_53_step_05_app.R", port = 4000)'
````


Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_53_step_04_output&carpeta_salida=cu_53_step_05_output&carpeta_maestros=cu_53_maestros

Paso 6
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Resolver problema de optimización SPI y mostrar resultados

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_53_step_06_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_53_step_04_output&carpeta_salida=cu_53_step_06_output&carpeta_maestros=cu_53_maestros



