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

        detail example
            [{'ApparentTemperature': {'Imperial': {'Unit': 'F',
                                                'UnitType': 18,
                                                'Value': 81.0},
                                    'Metric': {'Unit': 'C',
                                                'UnitType': 17,
                                                'Value': 27.2}},
            'Ceiling': {'Imperial': {'Unit': 'ft', 'UnitType': 0, 'Value': 30000.0},
                        'Metric': {'Unit': 'm', 'UnitType': 5, 'Value': 9144.0}},
            'CloudCover': 10,
            'DewPoint': {'Imperial': {'Unit': 'F', 'UnitType': 18, 'Value': 66.0},
                        'Metric': {'Unit': 'C', 'UnitType': 17, 'Value': 18.9}},
            'EpochTime': 1540265640,
            'IsDayTime': True,
            'Link': 'http://www.accuweather.com/zh/cn/shenzhen/58194/current-weather/58194?lang=zh-cn',
            'LocalObservationDateTime': '2018-10-23T11:34:00+08:00',
            'LocalSource': {'Id': 7, 'Name': '华风', 'WeatherCode': '00'},
            'MobileLink': 'http://m.accuweather.com/zh/cn/shenzhen/58194/current-weather/58194?lang=zh-cn',
            'ObstructionsToVisibility': '',
            'Past24HourTemperatureDeparture': {'Imperial': {'Unit': 'F',
                                                            'UnitType': 18,
                                                            'Value': -2.0},
                                                'Metric': {'Unit': 'C',
                                                            'UnitType': 17,
                                                            'Value': -1.1}},
            'Precip1hr': {'Imperial': {'Unit': 'in', 'UnitType': 1, 'Value': 0.0},
                            'Metric': {'Unit': 'mm', 'UnitType': 3, 'Value': 0.0}},
            'PrecipitationSummary': {'Past12Hours': {'Imperial': {'Unit': 'in',
                                                                    'UnitType': 1,
                                                                    'Value': 0.0},
                                                    'Metric': {'Unit': 'mm',
                                                                'UnitType': 3,
                                                                'Value': 0.0}},
                                    'Past18Hours': {'Imperial': {'Unit': 'in',
                                                                    'UnitType': 1,
                                                                    'Value': 0.0},
                                                    'Metric': {'Unit': 'mm',
                                                                'UnitType': 3,
                                                                'Value': 0.0}},
                                    'Past24Hours': {'Imperial': {'Unit': 'in',
                                                                    'UnitType': 1,
                                                                    'Value': 0.0},
                                                    'Metric': {'Unit': 'mm',
                                                                'UnitType': 3,
                                                                'Value': 0.0}},
                                    'Past3Hours': {'Imperial': {'Unit': 'in',
                                                                'UnitType': 1,
                                                                'Value': 0.0},
                                                    'Metric': {'Unit': 'mm',
                                                                'UnitType': 3,
                                                                'Value': 0.0}},
                                    'Past6Hours': {'Imperial': {'Unit': 'in',
                                                                'UnitType': 1,
                                                                'Value': 0.0},
                                                    'Metric': {'Unit': 'mm',
                                                                'UnitType': 3,
                                                                'Value': 0.0}},
                                    'Past9Hours': {'Imperial': {'Unit': 'in',
                                                                'UnitType': 1,
                                                                'Value': 0.0},
                                                    'Metric': {'Unit': 'mm',
                                                                'UnitType': 3,
                                                                'Value': 0.0}},
                                    'PastHour': {'Imperial': {'Unit': 'in',
                                                                'UnitType': 1,
                                                                'Value': 0.0},
                                                    'Metric': {'Unit': 'mm',
                                                            'UnitType': 3,
                                                            'Value': 0.0}},
                                    'Precipitation': {'Imperial': {'Unit': 'in',
                                                                    'UnitType': 1,
                                                                    'Value': 0.0},
                                                        'Metric': {'Unit': 'mm',
                                                                    'UnitType': 3,
                                                                    'Value': 0.0}}},
            'Pressure': {'Imperial': {'Unit': 'inHg', 'UnitType': 12, 'Value': 30.06},
                        'Metric': {'Unit': 'mb', 'UnitType': 14, 'Value': 1018.0}},
            'PressureTendency': {'Code': 'F', 'LocalizedText': '下降'},
            'RealFeelTemperature': {'Imperial': {'Unit': 'F',
                                                'UnitType': 18,
                                                'Value': 83.0},
                                    'Metric': {'Unit': 'C',
                                                'UnitType': 17,
                                                'Value': 28.2}},
            'RealFeelTemperatureShade': {'Imperial': {'Unit': 'F',
                                                        'UnitType': 18,
                                                        'Value': 78.0},
                                        'Metric': {'Unit': 'C',
                                                    'UnitType': 17,
                                                    'Value': 25.8}},
            'RelativeHumidity': 64,
            'Temperature': {'Imperial': {'Unit': 'F', 'UnitType': 18, 'Value': 79.0},
                            'Metric': {'Unit': 'C', 'UnitType': 17, 'Value': 26.1}},
            'TemperatureSummary': {'Past12HourRange': {'Maximum': {'Imperial': {'Unit': 'F',
                                                                                'UnitType': 18,
                                                                                'Value': 79.0},
                                                                    'Metric': {'Unit': 'C',
                                                                                'UnitType': 17,
                                                                                'Value': 26.1}},
                                                        'Minimum': {'Imperial': {'Unit': 'F',
                                                                                'UnitType': 18,
                                                                                'Value': 72.0},
                                                                    'Metric': {'Unit': 'C',
                                                                                'UnitType': 17,
                                                                                'Value': 22.0}}},
                                    'Past24HourRange': {'Maximum': {'Imperial': {'Unit': 'F',
                                                                                'UnitType': 18,
                                                                                'Value': 82.0},
                                                                    'Metric': {'Unit': 'C',
                                                                                'UnitType': 17,
                                                                                'Value': 28.0}},
                                                        'Minimum': {'Imperial': {'Unit': 'F',
                                                                                'UnitType': 18,
                                                                                'Value': 72.0},
                                                                    'Metric': {'Unit': 'C',
                                                                                'UnitType': 17,
                                                                                'Value': 22.0}}},
                                    'Past6HourRange': {'Maximum': {'Imperial': {'Unit': 'F',
                                                                                'UnitType': 18,
                                                                                'Value': 79.0},
                                                                    'Metric': {'Unit': 'C',
                                                                            'UnitType': 17,
                                                                            'Value': 26.1}},
                                                        'Minimum': {'Imperial': {'Unit': 'F',
                                                                                'UnitType': 18,
                                                                                'Value': 72.0},
                                                                    'Metric': {'Unit': 'C',
                                                                            'UnitType': 17,
                                                                            'Value': 22.0}}}},
            'UVIndex': 3,
            'UVIndexText': '中度',
            'Visibility': {'Imperial': {'Unit': 'mi', 'UnitType': 2, 'Value': 10.0},
                            'Metric': {'Unit': 'km', 'UnitType': 6, 'Value': 16.1}},
            'WeatherIcon': 1,
            'WeatherText': '晴',
            'WetBulbTemperature': {'Imperial': {'Unit': 'F',
                                                'UnitType': 18,
                                                'Value': 70.0},
                                    'Metric': {'Unit': 'C',
                                                'UnitType': 17,
                                                'Value': 21.4}},
            'Wind': {'Direction': {'Degrees': 23, 'English': 'NNE', 'Localized': '东北偏北'},
                    'Speed': {'Imperial': {'Unit': 'mi/h', 'UnitType': 9, 'Value': 8.1},
                                'Metric': {'Unit': 'km/h', 'UnitType': 7, 'Value': 13.0}}},
            'WindChillTemperature': {'Imperial': {'Unit': 'F',
                                                    'UnitType': 18,
                                                    'Value': 79.0},
                                    'Metric': {'Unit': 'C',
                                                'UnitType': 17,
                                                'Value': 26.1}},
            'WindGust': {'Speed': {'Imperial': {'Unit': 'mi/h',
                                                'UnitType': 9,
                                                'Value': 8.1},
                                    'Metric': {'Unit': 'km/h',
                                                'UnitType': 7,
                                                'Value': 13.0}}}}]
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
