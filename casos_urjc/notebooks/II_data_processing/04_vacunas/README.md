# Notebooks CU 04 vacunas

- Operaciones de limpieza hechas en 05 a reportar en otros procesos:
    - 05 imputación missing porque hacía falta antes


## 05. - Data Collection

### Datos del caso de uso

• Vacunas:
o    Número de vacunas suministradas por zona (o centro) y semana (por edad y sexo). Campaña en curso e histórico campaña anterior.
o    Número de citas solicitadas (con la granularidad que se pueda, idealmente la misma que las suministradas)
• Población (por edad, sexo y zona, al menos dos últimos periodos disponibles):
o    Población total
o    Densidad de población
o    Porcentaje de personas mayores de 65 años
o    Porcentaje de personas inmigrantes
o    Tasa de paro
• Sanitarios (por edad, sexo y zona, al menos dos últimos periodos disponibles):
o    Porcentaje de población de riesgo
o    Tasa de variación población mayor de 65 años (se puede calcular con los datos de población)
o    Tasa de variación población de riesgo
o    Capacidad de los censos sanitarios (ver con experto en el dominio cómo se mide)
• Medioambientales, semanales como mínimo e histórico dos años. Por estación o agregados por zona. Estos como mínimo, pero si hay más de otros casos de uso (e.g., viento) que se puedan coger:
o    Temperatura
o    Humedad
o    Niveles de contaminación (NO_2 estación de medición más próxima u otra más apropiada según expertos del dominio)
• Internet, agregados por semana o que se puedan agregar, con la delimitación geográfica que se pueda (país, provincia, ...):
o    Número de búsquedas (o su estimación) en Internet palabra "gripe"
o    Número de tuits (o su estimación) palabra "gripe"

### Notebooks específicos

#### 05. - Data Collection_CU_04_01_zonas_v_01

* Completada

#### 05. - Data Collection_CU_04_02_poblacion_v_01

* Indicadores de sección censal extrapolados a zona sanitaria
* Descripciones de los indicadores socioeconómicos (metadatos)
* Completada

#### 05. - Data Collection_CU_04_02_poblacion_v_01