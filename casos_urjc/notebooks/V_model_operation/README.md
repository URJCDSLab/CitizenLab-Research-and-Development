NOTAS DEPLOYMENT EMILIO
=======================

Proceso general
---------------

* Una carpeta para cada caso
* Scripts R de pruebas o preparación, en carpeta `_R`
* Scripts R que solo generan artefectos (e.g., modelos en .rds) también en `_R`
* Cada caso tendrá varios PASOS
* Cada PASO tiene un script de R que es el que se ejecuta en el contenedor, y una carpeta de salida
* El paso 1 también tiene carpeta de entrada
* Hay además una carpeta de maestros
* Los scripts pueden ser: 
    - De proceso: lee datos, procesa, escribe datos
    - De interfaz: App shiny. Puede leer y escribir o solo leer.
    - Si se puede, en un único archivo

Ver los notebooks del CU_O4 para ver cómo funciona, y screen castaquí:

http://emilio.lcano.com/tmp/screencast_cu_04.mp4


Series temporales
-----------------

Paquetes {tidyverts}

* {tsibble} para preparar las estructuras de datos
* {fable} para ajustar los modelos


CASO 04 vacunación
------------------

Modelos con covariables guay. Pero para predecir no habrá información, por ejemplo de tendencias, y solo se podría predecir el siguiente con el último valor.
Hago el ARIMA sin más, y ya veremos si lo mejoro.



