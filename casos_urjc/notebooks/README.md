Notebooks casos de uso - Generalidades
======================================

* Para cada proceso de la metodología hay que hacer notebooks con todo el flujo de trabajo.

* Tenemos plantillas de Jupyter Notebooks en python, que es el lenguaje que usan en los casos de uso de UAH.

* Los casos de uso de URJC los estamos haciendo en R, entonces voy "traduciendo" las plantillas a R. Deberían funcionar en otro ordenador con R instalado.

* Las plantillas son orientativas, se puede adaptar lo que se quiera y no tiene que haber de todas. Algunas son herramientas muy específicas de python, si hay algo parecido en R se hace, o se añaden otras.

* Hay que meter todas las explicaciones que se pueda para que haya mucha documentación. Estas explicaciones, intercaladas con el código y al final en apartados de conclusiones, acciones, etc.

* Al final de cada notebook hay un apartado de deploy para poner el código que se va a necesitar en el despliegue final, que será una aplicación shiny en el caso de R.

* Si alguno de los procesos no aplica, poner algo igualmente en el notebook explicando por qué

* Ir haciéndolos en horizontal para tener algo de todos los casos de uso


## Nombres de notebooks

`nombrenotebook_CU_xx_NombreProperData_v_01`

### ejemplo

07.- Data Sampling_CU_03_CLIMA_MADRID_2019_V_01

El proper data es el fichero ya pseudo-final de trabajo, con el que se hace el notebook


## Ayuda que necesito

- Descargar/generar los datos que me falten para tener conjuntos de datos copletos y poder empezar con los modelos (ver README.md en carpeta dominio/cu)

- Rellenar los notebooks de todos los procesos

- Ayuda con los notebooks: 
    * Salida UTF-8 acentos
    * lintr errores que no son

Repo github

GranadaDavid

## Documentación 

En la carpeta doc:

- Esquemas, tareas y datos de cada caso de uso
- Metodología CitizenLab
- Indicaciones tareas processing
