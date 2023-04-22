from pytrends.request import TrendReq
import pandas as pd

# Iniciar sesión en Google Trends
pytrend = TrendReq()

# Especificar la palabra clave a buscar
keyword = 'gripe'

# Establecer la región de búsqueda
pytrend.build_payload(kw_list=[keyword], geo='ES-MD')

# Obtener los datos de tendencias de búsqueda por día
trend_daily = pytrend.get_historical_interest([keyword], year_start=2021, month_start=9, day_start=1, year_end=2023, month_end=1, day_end=31, cat=0, geo='ES-MD', gprop='', sleep=0)

# Seleccionar solo los datos correspondientes al período septiembre 2021 - enero 2023
start_date = '2021-09-01'
end_date = '2023-01-31'
trend_daily = trend_daily.loc[start_date:end_date]

# Convertir los datos a un formato de marco de datos
trend_daily = pd.DataFrame(trend_daily.stack(), columns=['busquedas_gripe']).reset_index()

# Agrupar los datos por fecha y calcular el número de búsquedas por zona de la Comunidad de Madrid
trend_daily = trend_daily.groupby(['date', 'geoName']).sum()

# Guardar los datos en un archivo CSV
trend_daily.to_csv('busquedas_diarias_gripe_zonas_CAM.csv')



