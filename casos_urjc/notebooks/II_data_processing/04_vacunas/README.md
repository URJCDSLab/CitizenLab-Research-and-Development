# Notebooks CU 04 vacunas

- Operaciones de limpieza hechas en 05 a reportar en otros procesos:
    - 05 imputación missing porque hacía falta antes
    - 12 y 13 nombres de columnas importadas


## 05. - Data Collection

### Datos del caso de uso

• Vacunas:
[X] Número de vacunas suministradas por zona (o centro) y semana (por edad y sexo). Campaña en curso e histórico campaña anterior.
[X] Número de citas solicitadas (con la granularidad que se pueda, idealmente la misma que las suministradas)

• Población (por edad, sexo y zona, al menos dos últimos periodos disponibles):
[X] Población total _-> se añaden todos los disponibles_
[X] Densidad de población
[X] Porcentaje de personas mayores de 65 años
[X] Porcentaje de personas inmigrantes
[X] Tasa de paro

• Sanitarios (por edad, sexo y zona, al menos dos últimos periodos disponibles):
[X] Porcentaje de población de riesgo
[X] Tasa de variación población mayor de 65 años (se puede calcular con los datos de población)
[X] Tasa de variación población de riesgo
[X] Capacidad de los censos sanitarios (ver con experto en el dominio cómo se mide)

• Medioambientales, semanales como mínimo e histórico dos años. Por estación o agregados por zona. Estos como mínimo, pero si hay más de otros casos de uso (e.g., viento) que se puedan coger:
[X] Temperatura
[X] Humedad _-> cambiar por precipitaciones y añadir otras_
[X] Niveles de contaminación (NO_2 estación de medición más próxima u otra más apropiada según expertos del dominio) _-> cambiado por nox aunque se incluye todo_

• Internet, agregados por semana o que se puedan agregar, con la delimitación geográfica que se pueda (país, provincia, ...):
[X] Número de búsquedas (o su estimación) en Internet palabra "gripe"
[X] Número de tuits (o su estimación) palabra "gripe"

### Notebooks específicos

#### 05. - Data Collection_CU_04_01_zonas_v_01

* Completada

#### 05. - Data Collection_CU_04_02_poblacion_v_01

* Indicadores de sección censal extrapolados a zona sanitaria
* Descripciones de los indicadores socioeconómicos (metadatos)
* Completada

#### 05. - Data Collection_CU_04_02_poblacion_v_01

...