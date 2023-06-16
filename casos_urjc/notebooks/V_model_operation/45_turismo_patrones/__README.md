Ejecución del caso CU45_Planificación y promoción del destino en base a los patrones en origen de los turistas	
========================================================================


>NOTA EMILIO -> EQUIPO DESARROLLO URJC: Pongo explicado en formato cita los pasos que hay, lo
que hacen y los datos y scripts que necesitan. Con esto y el caso completo 04 
deberíais poder crear los `**_app.R` y `**_script.R` que se necesiten. Cambiad
también los fragmentos de llamada a los scripts y enlace al navegador como corresponda
(dejo los cu_04 sin tocar). De las variables (paso 2) hay instrucciones en el propio
archivo VARIABLES.csv en `**step_01_input`.


Paso 1
------

Tipo: Parametrización/configuración

Qué hace: Carga de ficheros


> ARCHIVOS QUE SE TIENEN QUE PODER CARGAR:
>> ├── CU_45_05_01_municipios_geo.json
├── CU_45_05_03_receptor.csv
├── CU_45_05_04_interno_prov.csv
├── CU_45_05_05_interno_mun.csv
>
>
>> * ESCENARIO_REG.csv
>
>Vienen de la carpeta Data/Output del caso en notbooks dominios II y III
>Copiados a mano en step_01_input
>Guardar en step_01_output




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


> VARIABLES QUE SE TIENEN QUE PODER MODIFICAR Y TIPO DE CONTROL:
>
> NCLUS

Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza número de turistas por origen en mapa

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_03_output&carpeta_maestros=cu_04_maestros

>Seleccionar el alcance del análisis: municipios receptores o emisores, o puntos de interés y valoraciones
>
>Municipios receptores: mapa de municipios comunidad de madrid, colores según el país/provincia mayoritarios.
>
> En una tabla, mostrar los turistas por origen, según lo seleccionado antes.
>
> Municipios emisores: mapa de municipios de España, colores según flujos a C.Madrid
>
>Puntos de interés: puntos coloreados según puntuación, o bien municipios coloreados según
mediana de puntuación
>
>Seleccionado un municipio, mostrar mapa de provincias de España o de paises del
mundo, coloreado por número de turistas recibidos en el municipio seleccionado


Paso 4
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Clasifica municipios en clusters y visualiza los resultados


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_04_output&carpeta_maestros=cu_04_maestros


>OBTENER CLUSTERS
>Cargar los datos de los municipios (ver código ejemplos)
>
>Obener clusters para cada año (ver cu_45_modelo_cluster.R)
>
>Guardar resultado en output
>
>Selección de año
>
>Mostrar resumen de clusters y gráfico
>
>Mostrar tabla de detalle seleccionado un municipio
>
>Mostrar mapa con el color según cluster de cada municipio
>
>Gráfico de dispersión y=receptor, x=ccaa seleccionada, color del cluster, para el año seleccionado
>
>Serie temporal (x=mes, y=turistas) de internacionales y ccaa seleccionada para un municipio seleccionado



Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Estimar modelo de regresión para predecir cluster


Terminal:

````
$ Rscript xxxxxx
````

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


