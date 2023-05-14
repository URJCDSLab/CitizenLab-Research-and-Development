Ejecución del caso CU18_Comportamienta Infra. Eventos extremos			
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
>> * CU_18_05_03_distritos_geo.json
>> * CU_18_05_20_diario_infra.csv
>> * CU_18_05_19_01_infraestructuras.csv
>> * CU_18_05_16_distritos_variables.csv
>> * ESCENARIO_CLUSTER_DIARIO.csv
>> * ESCENARIO_CLUSTER_DIST.csv
>> * ESCENARIO_REGRESION.csv
>
>Vienen de la carpeta Data/Output del caso en notbooks dominios II y III
>Copiad a mano en step_01_input
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
>> MODELO 
>> NIVEL
>> NSIM


Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza infraestructuras en el mapa y en forma de tabla

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_03_output&carpeta_maestros=cu_04_maestros

>Si se ha seleccionado NIVEL=Distrito, entonces mapa de áreas y poder seleccionar
lo que representa el color de entre las columnas de CU_18_05_16_distritos_variables.csv
>
>Si se ha seleccionado NIVEL=Diario, entonces mapa de puntos. Seleccionar rango de
fechas para filtrar. La forma de agregar: suma en todo menos en tmed, velmedia y presMax
que se calcula la media. Se representa con color la columna que se seleccione de 
CU_18_05_20_diario_infra.csv




Paso 4
------

Tipo: Visualización de resultados

Qué hace: Visualizar la clasificación de infraestructuras guardada


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_04_output&carpeta_maestros=cu_04_maestros


>Cargar datos de clusters de cu_19_maestros
>Si se ha seleccionado NIVEL=Distrito, entonces mapa de áreas coloreado según el cluster
>
>Si se ha seleccionado NIVEL=Diario, entonces mapa de puntos, coloreado según el cluster
>
>Texto en popup: Distrito o nombre infraestructura
>
>Texto en label: tabla con valores de todas las variables que no sean cero 
>
>Gráfico clusters: Ver paso_04_ejemplos.R
>Tabla clusters: Ver paso_04_ejemplos.R
>
> Predicción escenario: tabla con los valores y el cluster asignado a cada línea
del escenario. Ver paso_04_ejemplos.R


Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Visualizar modelo GLM y predecir escenarios


Terminal:

````
$ Rscript xxxxxx
````

>Se carga el modelo de regresión. Se muestra summary y gráfico de dispersión
respuesta (evento_infra o evento_zona) vs una de las variables del modelo
seleccionadas.
>
>Se hace predicción del escenario guardado. Se muestra tabla con valores y 
probabilidad predicha.


Paso 6
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Simular miles de valores según escenario y visualizar resumen del resultado

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_06_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_05_output&carpeta_salida=cu_04_step_06_output&carpeta_maestros=cu_04_maestros

>Se selecciona modelo a simular: cluster o regresión. El de cluster, solo se considera el de distritos.
>El de regresión, se selecciona eventos_zona o eventos_infra
>
>Se carga el escenario y se calculan las medias y desviación típicas de las variables.
>Se simulan NSIM realizaciones multivariantes, tomando distribución Poisson si es
>recuentos y normal si es continua, con sus parámetros.
>
>Se presentan gráficos de barras con las predicciones (clusters, clasificación), y
tabla.
>
>Ver ejemplos en paso_06_ejemplos.


