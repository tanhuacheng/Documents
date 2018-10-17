#!/usr/bin/python3
# -*- coding:utf-8

import requests


API_KEY = 'MGLTGzNykaObgo8WZ350j9qPku3i5z7D'

API_LOCATIONS_SEARCH    = 'http://dataservice.accuweather.com/locations/v1/cities/autocomplete'
API_CURRENT_CONDITIONS  = 'http://dataservice.accuweather.com/currentconditions/v1/'
API_DAILY_FORCAST       = 'http://dataservice.accuweather.com/forecasts/v1/daily/1day/'


class AccuWeather:

    def locations_search(self, key, language='zh-cn'):
        payload = {
            'apikey': API_KEY,
            'language': language,
            'q': key
        }

        try:
            return requests.get(API_LOCATIONS_SEARCH, params=payload, timeout=32).json()
        except:
            return None

    def current_conditions(self, location_key, language='zh-cn', details=False):
        payload = {
            'apikey': API_KEY,
            'language': language,
            'details': details,
        }

        try:
            url = API_CURRENT_CONDITIONS + location_key
            return requests.get(url, params=payload, timeout=32).json()
        except:
            return None

    def daily_forcast(self, location_key, language='zh-cn', details=False, metric=False):
        payload = {
            'apikey': API_KEY,
            'language': language,
            'details': details,
            'metric': metric,
        }

        try:
            url = API_DAILY_FORCAST + location_key
            return requests.get(url, params=payload, timeout=32).json()
        except:
            return None
