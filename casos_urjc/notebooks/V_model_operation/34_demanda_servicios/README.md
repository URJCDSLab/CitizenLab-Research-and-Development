CU34_Predicción de demanda de servicios
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
>> * CU_34_05_05_servicios_completo.csv
>> * CU_34_05_01_secciones_geo.json


>> * ESCENARIO_SERVICIOS.csv
>
>Vienen de la carpeta Data/Output del caso en notbooks dominios II y III
>Copiados a mano en step_01_input
>Guardar en step_01_output




Terminal:

````
$ Rscript -e 'shiny::runApp("cu_34_step_01_app.R", port = 4000)'
```

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


> VARIABLES QUE SE TIENEN QUE PODER MODIFICAR Y TIPO DE CONTROL:
>
> NSIM


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

>Seleccionar tipo de servicio de entre los valores únicos columna Servicio
>
>Seleccionar intervalo de fechas
>
>Seleccionar variable a representar
>
>Agregar los datos para representar en el mapa
>
>En el mapa, representar secciones censales con color de relleno según variable
>seleccionada
>
>Tabla con los datos del mapa
>
>Seleccionar sección censal para ver serie
>
>Gráfico de serie temporal de la sección seleccionada con los datos de la serie según fechas seleccionadas


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


>OBTENER CLUSTERS
>Cargar los datos de los servicios
>
>Obener clusters (ver cu_34_modelo_cluster.R)
>
>Guardar resultado en output
>
>Mostrar resumen de clusters y gráfico
>
>Mostrar tabla de detalle seleccionado un municipio
>
>Mostrar mapa con el cluster mayoritario por sección censal
>



Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Cargar escenarios y predecir su cluster.


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_34_step_05_app.R", port = 4000)'
````

>Se carga el modelo de redes neuronales
>
>Se carga el escenario
>
>Se predicen las probabilidades de los clusters
>
>Se muestra tabla detalle de las predicciones (probabilidades)
>
>Se muestra gráfico de barras con las frecuencias de los clusters predichos

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

>Se carga el escenario de servicios
>
>Se calculan los parámetros de las distribuciones
>
>Se simula el escenario NSIM veces
>
>Se muestra resumen de las variables: gráfico de barras Futbol, histogramas del resto
>y resumen numérico con summarytools::freq() y summarytools::descr()
>


