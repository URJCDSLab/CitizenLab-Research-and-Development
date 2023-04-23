Notas para resolver issues de notebooks R


* Nos piden notebooks de todos los procesos para cada caso. Idealmente
habría que hacer uno o varios notebooks para cada fichero que se usa en el 
deploy basándonos en las plantillas que nos pasan de Python. A esto no llegamos,
así que hacemos lo siguiente.

* En cada caso de uso tengo un archivo de datos en la carpeta Data/Output que
termina en "completo" donde están todos los datos unidos. Ejemplo: en el caso de 
de uso de las vacunas de gripe, hay un fichero con la popularidad en Google que
solo hay una fila por semana, y otro fichero que tiene el histórico de vacunación
que tiene una fila por semana y zona sanitaria. Están unidos de forma que
la popularidad en google se repite en todas las filas de esa semana. En el
deploy los uso por separado, pero aquí nos sirve para hacer los notebooks como
churros y poder pasar al deploy con shiny.

* Cogemos la plantilla del proceso, la copiamos en la carpeta del caso dentro de
`dominios_II_y_III` y renombramos, por ejemplo: 

06.- Data Adequacy_CU_xx_xx_xxxx_v_01.ipynb -> 06.- Data Adequacy_CU_04_19_datos_completos_v_01.ipynb

En este ejemplo el 19 es el notebook de 05 que generó el archivo de datos completos.

* Todas las plantillas ya tienen el código que importa los datos y muestra la estructura.
Solo hay que poner el nombre del archivo donde pone:

`iFile <- "xxxx"`

* Metemos código que funcione o texto que diga que ya se ha hecho en el proceso 05 (en la carpeta del caso
hay un rmd con algunas notas). 

* Añadir algo de código, lo que se os ocurra referente al proceso. En algunos notebooks 
he dejado los huecos que había en las plantillas de Python. No hay que llenarlo todo,
pero tampoco dejarlos todos vacíos. En los que no pongamos nada, decir por ejemplo que
no aplica, que se ha hecho en el proceso 05, o que se hará en el dominio IV por conveniencia.

* En algunas plantillas he dejado casi todo el código. Por ejemplo en 12 EDA debería funcionar
lo que he puesto en casi todos los casos. Y en 13 que es visualización he dejado
solo un mapa y digo que en el 12 hay más visualizaciones. 

* En otros no hay prácticamente nada de código, si al ir haciéndolos veis que
el código sirve para el resto, copiadlo a la plantilla y así los siguientes son
más rápidos.

* En la parte final de los notebooks, lo normal es que no guardemos nada. Si luego
en el deploy vemos que hace falta guardar alguna transformación, ya lo haremos.

* Donde sí tenemos que añadir algo es en la parte de "Report", aunque sea genérico.
Ya hay alguna frase general, intentad añadir alguna frase particular de ese fichero y
ese caso, con lo que hayáis visto.

* Los notebooks del proceso 5 los tengo hechas de casi todo, termino las que me faltan
mientras se van haciendo los de los procesos 6 a 13 de los otros.



Notas sobre la estructura de archivos:
--------------------------------------


* Documentación de apoyo:

````
../../_doc/
├── 2_Casos_de_Uso ESQUEMA_TODOS.xlsx -> si os hace falta entender el caso aquí podeís mirar
├── 3_Casos_de_Uso TAREAS_TODOS.xlsx
├── 5_Casos_de_Uso DATOS PEDIR A EMPRESAS_TODOS.xlsx
├── Data Processing Domain HELP WITH NOTEBOOKS.pdf
└── metodologia_citizenlab.pdf -> Aquí están descritos los dominios, procesos, etc. que es lo que tenemos que cumplir
````


* Plantillas:

````
../../0_plantillas/
├── _notebooks_R_II_data_processing -> Aquí están las plantillas que hay que usar 
├── _notebooks_py_feature_eng -> Las que nos mandaron en Python Domain III
└── _notebooks_py_preprocessing -> Las que nos mandaron en Python Domain II
´´´´

* Notebooks y datos.

En cada caso, los datos están en Data/Input. Los notebooks, en el raíz de cada caso.

````
../../dominios_II_y_III/
├── 04_vacunas
│   ├── Data
│   │   ├── Input
│   │   │   └── Zonas Básicas de Salud del Área Única
│   │   └── Output
│   ├── images
│   └── src
├── 18_infraestructuras
│   ├── Data
│   │   ├── Input
│   │   │   ├── Espana_Seccionado2022_ETRS89H30
│   │   │   ├── Salud_ Centros sanitarios. Centros de Atención Integral a Drogodependientes
│   │   │   ├── Salud_ Centros sanitarios. Centros de especialidades
│   │   │   ├── Salud_ Centros sanitarios. Centros de salud
│   │   │   ├── Salud_ Centros sanitarios. Consultorios de salud
│   │   │   ├── Salud_ Centros sanitarios. Otros centros
│   │   │   ├── Salud_ Centros sanitarios. Servicio de salud mental de distrito
│   │   │   ├── Transportes y Comunicaciones_ Cercanías (estaciones)
│   │   │   ├── Transportes y Comunicaciones_ Intercambiadores
│   │   │   ├── Transportes y comunicaciones_ Aeropuertos
│   │   │   ├── Transportes y comunicaciones_ Helisuperficies
│   │   │   ├── Transportes y comunicaciones_ Metro (bocas)
│   │   │   ├── admon_comercio_actividad_educacion
│   │   │   │   ├── Administración Pública. Administración de Justicia
│   │   │   │   ├── Administración pública_ Agencia Tributaria
│   │   │   │   ├── Administración pública_ Ayuntamientos, Consejerías, Ministerios, etc.
│   │   │   │   ├── Administración pública_ Embajadas y consulados
│   │   │   │   ├── Administración pública_ Oficinas de empleo
│   │   │   │   ├── Administración pública_ Seguridad Social
│   │   │   │   ├── Colectivo empresarial por tamaño y actividad
│   │   │   │   ├── Comercio_ Centros comerciales
│   │   │   │   ├── Comercio_ Galerías de alimentación
│   │   │   │   ├── Comercio_ Grandes superficies especializadas
│   │   │   │   ├── Comercio_ Hipermercados
│   │   │   │   ├── Comercio_ Mercadillos
│   │   │   │   ├── Comercio_ Mercados de abastos
│   │   │   │   ├── Comercio_ Otros servicios. Bancos
│   │   │   │   ├── Educación_ Campus universitarios
│   │   │   │   ├── Educación_ Centros educativos no universitarios. Centros privados
│   │   │   │   ├── Educación_ Centros educativos no universitarios. Centros públicos
│   │   │   │   ├── Educación_ Centros educativos no universitarios. Servicios educativos
│   │   │   │   ├── Educación_ Centros educativos universitarios
│   │   │   │   └── Educación_ Colegios mayores
│   │   │   └── madrid
│   │   └── Output
│   ├── doc
│   ├── images
│   └── src
├── 25_listas_espera
│   ├── Data
│   │   ├── Input
│   │   │   └── Areas de Salud
│   │   └── Output
│   ├── images
│   └── src
├── 34_servicios
│   ├── Data
│   │   ├── Input
│   │   │   └── Espa§a_Seccionado2022_ETRS89H30
│   │   └── Output
│   └── images
├── 45_turismo_origen
│   ├── Data
│   │   ├── Input
│   │   └── Output
│   └── images
├── 53_inversion
│   ├── Data
│   │   ├── Input
│   │   │   └── xls
│   │   └── Output
│   └── images
└── 55_turismo_gasto
````


