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

        return None

    def locations_search(self, key, language='zh-cn'):
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
