Ejecución del caso CU25_Modelo de gestión de Lista de Espera Quirúrgica
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
>>├── CU_25_05_03_areasgeo.json
├── CU_25_05_05_01_hospitales.csv
├── CU_25_05_06_indicadores_area.csv
├── CU_25_05_07_01_capacidad.csv
└── CU_25_05_07_02_lista_espera.csv
>
>> * ESCENARIOxxx
>
>Vienen de la carpeta Data/Output del caso en notbooks dominios II y III
>Copiados a mano en step_01_input
>Guardar en step_01_output




Terminal:

````
$ Rscript -e 'shiny::runApp("cu_25_step_01_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_25_step_01_input&carpeta_salida=cu_25_step_01_output&carpeta_maestros=cu_25_maestros


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
> HORIZONTE (numericInput)
> NPER (numericInput)

Paso 3
------

Tipo: Visualización de resultados

Qué hace: Visualiza histórico listas de espera

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_03_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_03_output&carpeta_maestros=cu_04_maestros

>Cargar zonas de salud y hospitales
>
>Cargar datos de lista de espera, indicadores y capacidad
>
>Para el mapa: Seleccionar semana, o todas (entonces agrupar y calcular medias)
>
>Seleccionar especialidad, o todas (entonces agrupar y calcular suma personas, media tiempo)
>
>Seleccionar variable a mostrar en el mapa: puede ser de indicadores, de capacidad o de
>listas de espera
>
>Representar en el mapa: polígonos coloreados según variable seleccionada, y hospitales como puntos
>
>Si se seleccionan todas las semanas, mostrar gráfico de serie temporal
>
>
>Tabla con los datos completos según la selección


Paso 4
------

Tipo: Visualización de resultados

Qué hace: Predice el escenario para las próximas semanas


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_04_step_04_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_04_step_02_output&carpeta_salida=cu_04_step_04_output&carpeta_maestros=cu_04_maestros


>Cargar los modelos prophet/xgboost entrenados
>
>Seleccionar horizonte de predicción (por defecto el de variables)
>
>Seleccionar especialidad y zona (o todas y agrupar)
>
>Seleccionar parámetro a predecir (media, pacientes)
>
>Representar evolución y predicciones en serie temporal
>
>Mostrar tabla con las predicciones
>
>Guardar tabla de predicciones
>
>Modelos en maestros, ver ejemplos predicción y viz en cu_25_modelo_xgboost.R

Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Simulación listas de espera y visualización


Terminal:

````
$ Rscript xxxxxx
````

>Se cargan los modelos de xgboost
>
>se cargan las capacidades
>
>Se selecciona: Especialidad, zona sanitaria
>
>Se selecciona origen de los parámetros: últimos valores o valores predichos a h semanas
>
>Si se elige últimos valores, coger el último valor de la lista (ver ejemplo en modelo_des).
>Si se elige predicción, predecir con modelo xgboost y coger esos valores. Los valores son personas en lista y tiempo medio
>
>Se muestra la capacidad de la especialidad y zona (archivo **capacidad.csv).
>
>Permitir cambiar cualquiera de los tres valores
>
>Se calculan los parámetros de la cola M/M/m y se muestran (ver modelo_des.R)
>
>Se ejecuta la simulación, con los parámetros anteriores y el tiempo cogido de las
VARIABLES (que se pueda cambiar igualmente)
>
>Visualizar simulación: llegadas, estado de la cola (ver ejemplo)
>Mostrar tablas de la simulación
>
>Botón para guardar escenario

Paso 6
------

Tipo: Proceso

Qué hace: Predicción y simulación de la cola para todas las zonas y especialidades


Terminal:

````
$ Rscript xxxxxx
````

>Se cargan los modelos de xgboost
>Se hacen todas las predicciones
>Se hacen todas las simulaciones
>Se guardan los archivos 


Paso 7
------

Tipo: Visualización

Qué hace: Visualización del mapa de riesgo


Terminal:

````
$ Rscript xxxxxx
````

>Se cargan las predicciones y simulaciones
>Se selecciona un parámetro de la cola (personas, tiempo)
>Se selecciona una de: predicciones o simulaciones
>Si simulaciones, elegir último periodo o predicción horizonte h
>Se selecciona un periodo (semana para predicción xgboost, día para simulación)
>Se representa mapa de calor

