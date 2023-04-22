import datetime
import json
import pandas as pd
import requests
import numpy as np
import time

API_KEY = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJydWJlbi5yb2RyaWd1ZXpAdXJqYy5lcyIsImp0aSI6IjBlM2RhMDc2LTE0ZjYtNGQxZC05ZjQwLThhOWFkYmNiYjkwYSIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNjY5MzAzODA2LCJ1c2VySWQiOiIwZTNkYTA3Ni0xNGY2LTRkMWQtOWY0MC04YTlhZGJjYmI5MGEiLCJyb2xlIjoiIn0.RpPC2tJMcb5FrGQaMagGkeumAsUnGTwxXf-OZaVQFLg"


def get_stations():
    url = "https://opendata.aemet.es/opendata/api/valores/climatologicos/inventarioestaciones/todasestaciones"
    querystring = {"api_key": API_KEY}
    headers = {'cache-control': "no-cache"}
    response = requests.request("GET", url, headers=headers, params=querystring)
    metadata = json.loads(response.text)
    data_raw = requests.get(metadata['datos'])
    data = json.loads(data_raw.text)
    return data


def get_weather_one_month(date, ind):
    next_day = date + datetime.timedelta(days=30)

    from_date = f"{date.year}-{date.month}-{date.day}T00:00:00UTC"
    to_date = f"{next_day.year}-{next_day.month}-{next_day.day}T23:59:59UTC"

    url = f"https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/{from_date}/fechafin/{to_date}/estacion/{ind}"
    querystring = {"api_key": API_KEY}
    headers = {'cache-control': "no-cache", "accept": "application/json"}

    response = requests.request("GET", url, headers=headers, params=querystring)
    metadata = json.loads(response.text)
    data_raw = requests.get(metadata['datos'])
    data = json.loads(data_raw.text)
    return data


def get_weather_between(from_date, to_date, station):
    data = []
    curr_date = from_date
    fails = 0
    while curr_date < to_date:
        print(curr_date)
        try:
            batch = get_weather_one_month(curr_date, station['indicativo'])
            data.extend(batch)
            new_date = [int(x) for x in batch[-1]['fecha'].split("-")]
            curr_date = datetime.datetime(new_date[0], new_date[1], new_date[2])
            fails = 0
        except Exception as e:
            if fails == 2:
                raise
            print("Failed on ", curr_date, "with exception ", e)
            fails += 1
            time.sleep(60)

    df = pd.DataFrame(data)
    df['latitud'] = station['latitud']
    df['longitud'] = station['longitud']
    return df


start_date = datetime.datetime(2022, 1, 1)
end_date = datetime.datetime(2022, 12, 31)
province = 'MADRID'
stations = [station for station in get_stations() if station['provincia'] == province]

hardcoded_stations = ['3129', '3194U', '3196', '3195']

data = []
# for station in [s for s in stations if s['indicativo'] in hardcoded_stations]:
#     station_data = get_weather_between(datetime.datetime(2022, 1, 1), datetime.datetime(2022, 12, 31),
#                                        station)
#     for k, v in station.items():
#         station_data[k] = v
#     data.append(station_data)

df = pd.concat(data)

numeric_cols = ['altitud', 'tmed', 'prec', 'tmin', 'tmax', 'racha', 'sol', 'presMax', 'presMin', 'dir', 'velmedia']

for col in numeric_cols:
    if not pd.api.types.is_numeric_dtype(df[col]):
        df[col] = pd.to_numeric(df[col].str.replace(',', '.'), errors='coerce')

df['tmed'] = df['tmed'].astype(float)
df['velmedia'] = df['velmedia'].astype(float)
df['Sensacion_Termica'] = 13.12 + 0.6215 * df['tmed'] - 11.37 * (df['velmedia'] ** 0.16) + 0.3965 * df[
    'tmed'] * (df['velmedia'] ** 0.16)

thermal_sens_cuts = [float('-inf'), 10, 15, 20, 25, 30, float('inf')]
thermal_sens_score = [0, 1, 2, 3, 4, 3]

precipitation_cuts = [float('-inf'), 0.1, 3, 10, 20, float('inf')]
precipitation_score = [0, -1, -2, -3, -4]

df['Clima_term'] = pd.to_numeric(pd.cut(df['Sensacion_Termica'], bins=thermal_sens_cuts, labels=thermal_sens_score, ordered=False), errors='coerce')
df['Clima_prec'] = pd.to_numeric(pd.cut(df['prec'], bins=precipitation_cuts, labels=precipitation_score), errors='coerce')
df['Clima'] = df['Clima_prec'] + df['Clima_term']


def to_clima_categ(x):
    if np.isnan(x):
        return x
    x = int(max(x, 0))
    return ['Muy malo', 'Malo', 'Regular', 'Bueno', 'Muy bueno'][x]

df['Clima'] = df['Clima'].apply(to_clima_categ)
df['Viento'] = df['racha'] > 20.0   # Brisa moderada o mas

remap = {
    'latitud': 'Lat',
    'longitud': 'Long',
    'altitud': 'Altitud',
    'tmed': 'Temperatura',
    'prec': 'Precipitaciones',
    'tmin': 'Temperatura_Minima',
    'tmax': 'Temperatura_Maxima',
    'racha': 'Racha',
    'sol': 'Sol',
    'presMax': 'Presion_Maxima',
    'presMin': 'Presion_Minina',
    'fecha': 'Fecha',
    'nombre': 'Nombre',
    'provincia': 'Provincia',
    'horatmax': 'Hora_Temperatura_Maxima',
    'horaracha': 'Hora_Racha',
    'horatmin': 'Hora_Temperatura_Minima',
    'dir': 'Dir',
    'velmedia': 'Velocidad_Media',
    'horaPresMax': 'Hora_Presion_Maxima',
    'horaPresMin': 'Hora_Presion_Minima'
}

df.rename(columns=remap, inplace=True)
df_sub = df[list(remap.values()) + ['Clima', 'Sensacion_Termica', 'Viento']]
df_sub.sort_index(axis=1).to_csv('weather.csv', index=None)