# Notebooks CU 18 infraestructuras


## 05. - Data Collection

### Notas

- No se sincroniza en el repositorio la carpeta Data/Input/Espana_Seccionado2022_ETRS89H30/ porque excede el tamaño.

- No se sincroniza en el repositorio la carpeta Data/Input/admon_comercio_actividad_educacion/ porque excede el tamaño.

- Operaciones de limpieza hechas en 05 a reportar en otros procesos:
    - 07 Limpieza de nombres y grupos de administraciones
    - 15 precipitación Ip inapreciable (moja la calle pero no llega a 0,1)
    
### Lista de categorías de POIS de OSM

http://download.geofabrik.de/osm-data-in-gis-formats-free.pdf

### Datos del caso de uso

**Infraestructuras:**
[X] Ubicación exacta (coordenadas estáticas de los edificios)
[X] Capacidad
[X] Demanda
[X] Tipo de infraestructura
[X] Otras disponibles
**Demográficas:**
[X] Población por edad y zona
[X] Superficie de la unidad territorial utilizada (se puede calcular con la geometría del mapa)
AÑADIDAS: todos los indicadores del INE
**Socioeconómicas**
[X] Actividad predominante de la zona -> cambiado por info de administración, etc.
[X] Número de viviendas e instalaciones educativas (distinguiendo guarderías, colegios, institutos, universidades) -> Número de viviendas no encontrado. Resto con info de administración, etc.
[X] Número de industrias, comercios, oficinas y ocio (bares, restaurantes, teatros/cines, instalaciones deportivas) -> POIS de turismo
[X] Distancia más corta a infraestructuras relevantes (polideportivo, instituto, etc.). Se puede calcular con las coordenadas de todas.
• Meteorológicas
[X] Temperatura y humedad. Si el alcance temporal es mayor que el diario, incluir media, desviación típica, máximo y mínimo. -> en vez de humedad usamos precipitaciones, que es un dato directo de las estaciones
• Transporte. Si están categorizadas como infraestructuras se puede obtener con las coordenadas.
[X] Número de estaciones de metro y paradas de autobús en la zona de referencia. -> metro, cercanias, aeropuertos, etc.
[X] Distancia a la estación de metro más cercana -> intercambiadores
[X] Distancia a la estación de tren más cercana
[X] Distancia al aeropuerto

* Eventos

[ ] Fecha
[ ] Demanda infraestructura
[ ] Capacidad infraestructura
[ ] Evento (avería, accidente, ...)


