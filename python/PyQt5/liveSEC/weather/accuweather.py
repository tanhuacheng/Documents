#!/usr/bin/python3
# -*- coding:utf-8

import requests


API_KEY = 'MGLTGzNykaObgo8WZ350j9qPku3i5z7D'

API_LOCATIONS_SEARCH    = 'http://dataservice.accuweather.com/locations/v1/cities/autocomplete'
API_CURRENT_CONDITIONS  = 'http://dataservice.accuweather.com/currentconditions/v1/'
API_DAILY_FORCAST       = 'http://dataservice.accuweather.com/forecasts/v1/daily/1day/'


class AccuWeather:

    def _get(self, url, params=None):
        try:
            req = requests.get(url, params=params, timeout=32)
            if req.status_code == requests.codes.ok:
                return req.json()
        except:
            pass

        return []

    def search_locations(self, key, language='zh-cn'):
        ''' result example
            [{
                "Version": 1,
                "Key": "2332730",
                "Type": "City",
                "Rank": 35,
                "LocalizedName": "普安县",
                "Country": {
                "ID": "CN",
                "LocalizedName": "中国"
                },
                "AdministrativeArea": {
                "ID": "GZ",
                "LocalizedName": "贵州省"
                }
            }]
        '''
        payload = {
            'apikey': API_KEY,
            'language': language,
            'q': key
        }

        return self._get(API_LOCATIONS_SEARCH, params=payload)

    def current_conditions(self, location_key, language='zh-cn', details=False):
        ''' result example truncated version
            [{
                'WeatherIcon': 12,
                'LocalObservationDateTime': '2018-10-23T00:17:00+08:00',
                'WeatherText': '阵雨',
                'MobileLink': 'http://m.accuweather.com/zh/cn/puan-county/2332730/current-weather/2332730?lang=zh-cn',
                'LocalSource': {
                    'Id': 7,
                    'Name': '华风',
                    'WeatherCode': '03'
                },
                'Link': 'http://www.accuweather.com/zh/cn/puan-county/2332730/current-weather/2332730?lang=zh-cn',
                'Temperature': {
                    'Metric': {
                        'Unit': 'C',
                        'UnitType': 17,
                        'Value': 10.3
                    },
                    'Imperial': {
                        'Unit': 'F',
                        'UnitType': 18,
                        'Value': 51.0
                    }
                },
                'EpochTime': 1540225020,
                'IsDayTime': False
            }]
        '''
        payload = {
            'apikey': API_KEY,
            'language': language,
            'details': details,
        }

        return self._get(API_CURRENT_CONDITIONS + location_key, params=payload)

    def daily_forcast(self, location_key, language='zh-cn', details=False, metric=False):
        payload = {
            'apikey': API_KEY,
            'language': language,
            'details': details,
            'metric': metric,
        }

        return self._get(API_DAILY_FORCAST + location_key, params=payload)
