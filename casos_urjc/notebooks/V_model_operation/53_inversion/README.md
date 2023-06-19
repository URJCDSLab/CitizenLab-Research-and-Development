Ejecución del caso CU53_impacto de las políticas de inversión en sanidad, infraestructuras y promoción turística en el SPI
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
>> * CU_53_05_02_01_spi.csv
>> * CU_53_05_02_02_spi_metadata.csv
>> * CU_53_05_03_paisesgeo.json
>> * CU_53_05_05_inversiones_cm.csv


>> * ESCENARIO_INVERSIONES_PAISES.csv
>> * ESCENARIO_INVERSIONES_REGION.csv
>
>Vienen de la carpeta Data/Output del caso en notbooks dominios II y III
>Copiados a mano en step_01_input
>Guardar en step_01_output


OUTPUTS:
>> * SPI.csv
>> * SPI_META.csv
>> * PAISES.json
>> * INVERSIONES_REGION_DETAIL.csv
>> * INVERSIONES_REGION.csv
>> * INVERSIONES_PAISES.csv
>> * VARIABLES.csv

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


> VARIABLES QUE SE TIENEN QUE PODER MODIFICAR Y TIPO DE CONTROL:
>
> MODELO
>
> ANYOOPT
>
> RESTINVTOT
>
> RESTINVINF
>
> RESTINVTUR
>
> RESTINVSAN


INPUTS (FROM STEP 1):
>> * SPI.csv
>> * SPI_META.csv
>> * PAISES.json
>> * INVERSIONES_REGION_DETAIL.csv
>> * INVERSIONES_REGION.csv
>> * INVERSIONES_PAISES.csv
>> * VARIABLES.csv

OUTPUTS:
>> * SPI.csv
>> * SPI_META.csv
>> * PAISES.json
>> * INVERSIONES_REGION_DETAIL.csv
>> * INVERSIONES_REGION.csv
>> * INVERSIONES_PAISES.csv
>> * VARIABLES.csv

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

>Seleccionar un año de los que hay SPI y/o inversiones
>
>En el mapa, mostrar el valor del SPI de cada país en popup, colorear por este valor,
>y en label el desglose por Dimensiones (ver relaciones en `*spi.csv` y `*spi_metadata.csv`)
>
> En una tabla, mostrar las inversiones por años en la cm (wide: tipo de inversión en columnas)
>
> Par de gráficos de serie temporal: uno con el SPI de los países que se seleccionen y el indicadores que se seleccione (selectize con grupos, Todas las variables), y otro
> con las inversiones de la CM, los tres tipos


INPUTS (FROM STEP 2):
>> * SPI.csv
>> * SPI_META.csv
>> * PAISES.json
>> * INVERSIONES_REGION_DETAIL.csv
>> * INVERSIONES_REGION.csv
>> * INVERSIONES_PAISES.csv
>> * VARIABLES.csv


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

>ESTIMAR MODELO
>Cargar escenario de regresión de input paso 1
>
>Estimar el modelo de regresión (ver cu_53_modelos_regresion.R)
>
>Guardar modelo en carpeta output
>
>Mostrar tabla de coeficientes y valor de lambda optimo
>
>PREDECIR VALOR
>
>Cargar escenario de inversión de input paso 1
>
>Hacer predicción de los escenarios con el modelo

|> podemos incluir algún gráfico mas a parte de las tablas (serie temporal; forecast plot)

INPUTS (FROM STEP 2):
>> * SPI.csv
>> * SPI_META.csv
>> * PAISES.json
>> * INVERSIONES_REGION_DETAIL.csv
>> * INVERSIONES_REGION.csv
>> * INVERSIONES_PAISES.csv
>> * VARIABLES.csv

OUTPUTS
>> * MODELO_REG.rds


Paso 5
------

Tipo: Visualización y procesamiento de datos

Qué hace: Estimar SPI con inversiones y proyectar serie temporal


Terminal:

````
$ Rscript -e 'shiny::runApp("cu_53_step_05_app.R", port = 4000)'
````

>Se carga el modelo de regresión y los datos de inversiones de la entrada
>
>Se transforman los datos de inversiones para hacer la estimación con modelo
>
>Se ajusta el modelo ARIMA
>
>Se muestran parámetros modelo ARIMA
>
>Se muestran las series de la estimación y su proyección.

INPUTS
>> * SPI.csv (Step 2)
>> * SPI_META.csv (Step 2)
>> * PAISES.json (Step 2)
>> * INVERSIONES_REGION_DETAIL.csv (Step 2)
>> * INVERSIONES_REGION.csv (Step 2)
>> * INVERSIONES_PAISES.csv (Step 2)
>> * VARIABLES.csv (Step 2)
>> * MODELO_REG.rds (maestros)



Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_53_step_02_output&carpeta_salida=cu_53_step_04_output&carpeta_maestros=cu_53_maestros

Paso 6
------

Tipo: Visualización de resultados y procesamiento de datos

Qué hace: Resolver problema de optimización SPI y mostrar resultados

Terminal:

````
$ Rscript -e 'shiny::runApp("cu_53_step_06_app.R", port = 4000)'
````

Navegador:

http://127.0.0.1:4000/?carpeta_entrada=cu_53_step_02_output&carpeta_salida=cu_53_step_06_output&carpeta_maestros=cu_53_maestros

>Se cargan los parámetros de configuración: restricciones y año a predecir
>
>Se resuelve la optimización
>
>Se muestran los valores óptimos

INPUTS
>> * SPI.csv (Step 2)
>> * SPI_META.csv (Step 2)
>> * PAISES.json (Step 2)
>> * INVERSIONES_REGION_DETAIL.csv (Step 2)
>> * INVERSIONES_REGION.csv (Step 2)
>> * INVERSIONES_PAISES.csv (Step 2)
>> * VARIABLES.csv (Step 2)
>> * MODELO_REG.rds (maestros)

