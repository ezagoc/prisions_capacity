########## Import necessary packages

from geopy.geocoders import Nominatim
import pandas as pd
import numpy as np
import time
from tqdm import tqdm
from time import sleep
from random import randint
import logging
from geopy.geocoders import Nominatim
from geopy.exc import GeocoderTimedOut, GeocoderServiceError
import requests
import unidecode

########## Defining the functions:
user_agent = 'user_me_{}'.format(randint(10000, 99999))
geolocator = Nominatim(user_agent=user_agent)
def reverse_geocode(geolocator, loc, sleep_sec):
    try:
        location = geolocator.geocode(loc)
        if location == None:
            lat_osm = None
            long_osm = None
        else:
            lat_osm = location.latitude
            long_osm = location.longitude
        return lat_osm, long_osm
    except GeocoderTimedOut:
        logging.info('TIMED OUT: GeocoderTimedOut: Retrying...')
        print('Error 1')
        sleep(randint(1*100,sleep_sec*100)/100)
        return reverse_geocode(geolocator, loc, sleep_sec)
    except GeocoderServiceError as e:
        logging.info('CONNECTION REFUSED: GeocoderServiceError encountered.')
        logging.error(e)
        return None, None
    except Exception as e:
        logging.info('ERROR: Terminating due to exception {}'.format(e))
        return None, None
    
def geocode_place(place_name, api_key):
    base_url = "https://maps.googleapis.com/maps/api/geocode/json"
    params = {
        "address": place_name,
        "key": api_key
    }
    response = requests.get(base_url, params=params)
    data = response.json()
    if data['status'] == 'OK':
        result = data['results'][0]
        lat = result['geometry']['location']['lat']
        lng = result['geometry']['location']['lng']
        formatted_address = result['formatted_address']
        return lat, lng, formatted_address, data['status']
    else:
        return None, None, None, data['status']  # or handle differently
    
def normalize_name2(name):
    if pd.isnull(name):
        return ""
    name = name.lower().strip()                      # Lowercase and trim
    name = unidecode.unidecode(name)                 # Remove accents
    name = name.replace('*', '')
    name = ' '.join(name.split())
    name = name.replace('c. p. r. s.', 'cereso')                    # Remove extra spaces
    return name

def choose_name(row):
    if pd.notna(row['center_name3']):
        return row['center_name3']
    elif pd.notna(row['center_name2']):
        return row['center_name2']
    else:
        return row['center_name1']

    
# Here your api key
api_key = "API_KEY"

# Import the file after checking it manual for errors in the automatic deduplication
df = pd.read_excel('../../../data/00-map/capacity/geolocate/deduplicated_prisons_2012_manual.xlsx')
df['name_geolocate'] = df.apply(choose_name, axis=1)
df['name_geolocate'] = df['name_geolocate'].apply(normalize_name2) + ', ' + df['state']

list_locations = list(df['name_geolocate'])

# Run the loop to geolocate each prison
df_final = pd.DataFrame()
for loc in tqdm(list_locations): 
    dfloc = pd.DataFrame()
    # Geocode using Google Maps
    lat, lng, address, status = geocode_place(loc, api_key)

    print(status)
    # Geocode using OSM
    lat_osm, long_osm = reverse_geocode(geolocator, loc, sleep_sec = 2)

    dfloc = pd.DataFrame([{
    'name_geolocate': loc,
    'status_gmaps': status,
    'address': address,
    'lat_gmaps': lat,
    'long_gmaps': lng,
    'lat_osm': lat_osm,
    'long_osm': long_osm
        }])

    df_final = pd.concat([df_final, dfloc]).reset_index(drop = True)
    time.sleep(2)


df_final.to_excel('../../../data/00-map/capacity/geolocate/geolocated.xlsx', 
                  index=False)

# After this I manually checked each one to guarantee it was correct and the final output is geolocated_manual.xlsx