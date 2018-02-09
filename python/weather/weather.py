#!/usr/bin/python3
# -*- coding:utf-8

"""
查询城市天气

Usage:
    weather.py <city>

Options:
    -h,--help   显示帮助菜单

Example:
    weather.py 兴义
"""

__author__ = 'tanhuacheng'

__result_example1__ = {
    'city': '宝安区',
    'updatetime': '15:21',
    'wendu': '17',
    'fengli': '4级',
    'shidu': '42%',
    'fengxiang': '东风',
    'sunrise_1': '07:00',
    'sunset_1': '18:17',
    'sunrise_2': {},
    'sunset_2': {},

    'yesterday': {
        'date_1': '7日星期三',
        'high_1': '高温 14℃',
        'low_1': '低温 10℃',
        'day_1': {
            'type_1': '多云',
            'fx_1': '无持续风向',
            'fl_1': '<3级',
        },
        'night_1': {
            'type_1': '多云',
            'fx_1': '无持续风向',
            'fl_1': '<3级',
        },
    },

    'forecast': [
        {
            'date': '8日星期四',
            'high': '高温 16℃',
            'low': '低温 10℃',
            'day': {
                'type': '多云',
                'fengxiang': '无持续风向',
                'fengli': '<3级',
            },
            'night': {
                'type': '多云',
                'fengxiang': '无持续风向',
                'fengli': '<3级',
            },
        },
        {
            'date': '9日星期五',
            'high': '高温 19℃',
            'low': '低温 13℃',
            'day': {
                'type': '小雨',
                'fengxiang': '无持续风向',
                'fengli': '<3级',
            },
            'night': {
                'type': '小雨',
                'fengxiang': '无持续风向',
                'fengli': '<3级',
            },
        },
        {
            'date': '10日星期六',
            'high': '高温 21℃',
            'low': '低温 14℃',
            'day': {
                'type': '小雨',
                'fengxiang': '无持续风向',
                'fengli': '<3级',
            },
            'night': {
                'type': '小雨',
                'fengxiang': '无持续风向',
                'fengli': '<3级',
            },
        },
        {
            'date': '11日星期天',
            'high': '高温 16℃',
            'low': '低温 9℃',
            'day': {
                'type': '多云',
                'fengxiang': '东北风',
                'fengli': '3-4级',
            },
            'night': {
                'type': '晴',
                'fengxiang': '东北风',
                'fengli': '3-4级',
            },
        },
        {
            'date': '12日星期一',
            'high': '高温 17℃',
            'low': '低温 11℃',
            'day': {
                'type': '晴',
                'fengxiang': '东北风',
                'fengli': '3-4级',
            },
            'night': {
                'type': '多云',
                'fengxiang': '无持续风向',
                'fengli': '<3级',
            },
        }
    ],

    'zhishus': [
        {
            'name': '晨练指数',
            'value': '较适宜',
            'detail': '早晨气象条件较适宜晨练，但晨练时会感觉有点凉，建议晨练着装不要过于单薄，以防感冒。',
        },
        {
            'name': '舒适度',
            'value': '舒适',
            'detail': '白天不太热也不太冷，风力不大，相信您在这样的天气条件下，应会感到比较清爽和舒适。',
        },
        {
            'name': '穿衣指数',
            'value': '较冷',
            'detail': '建议着厚外套加毛衣等服装。年老体弱者宜着大衣、呢外套加羊毛衫。',
        },
        {
            'name': '感冒指数',
            'value': '较易发',
            'detail': '天气较凉，较易发生感冒，请适当增加衣服。体质较弱的朋友尤其应该注意防护。',
        },
        {
            'name': '晾晒指数',
            'value': '适宜',
            'detail': '天气不错，适宜晾晒。赶紧把久未见阳光的衣物搬出来吸收一下太阳的味道吧！',
        },
        {
            'name': '旅游指数',
            'value': '适宜',
            'detail': '天气较好，但丝毫不会影响您出行的心情。温度适宜又有微风相伴，适宜旅游。',
        },
        {
            'name': '紫外线强度',
            'value': '弱',
            'detail': '紫外线强度较弱，建议出门前涂擦SPF在12-15之间、PA+的防晒护肤品。',
        },
        {
            'name': '洗车指数',
            'value': '较适宜',
            'detail': '较适宜洗车，未来一天无雨，风力较小，擦洗一新的汽车至少能保持一天。',
        },
        {
            'name': '运动指数',
            'value': '较适宜',
            'detail': '天气较好，无雨水困扰，较适宜进行各种运动，但因气温较低，在户外运动请注意增减衣物。',
        },
        {
            'name': '约会指数',
            'value': '较适宜',
            'detail': '虽然有点风，但情侣们可以放心外出，不用担心天气来调皮捣乱而影响了兴致。',
        },
        {
            'detail': '天气较好，不会降水，因此您可放心出门，无须带雨伞。',
            'name': '雨伞指数',
            'value': '不带伞',
        },
    ],
}

__result_example2__ = {
    'city': '北京',
    'updatetime': '17:11',
    'wendu': '2',
    'fengli': '3级',
    'shidu': '17%',
    'fengxiang': '西风',
    'sunrise_1': '07:16',
    'sunset_1': '17:42',
    'sunrise_2': {},
    'sunset_2': {},

    'environment': {
        'aqi': '82',
        'pm25': '60',
        'suggest': '极少数敏感人群应减少户外活动',
        'quality': '良',
        'MajorPollutants': '颗粒物(PM2.5)',
        'o3': '60',
        'co': '1',
        'pm10': '106',
        'so2': '16',
        'no2': '40',
        'time': '17:00:00',
    },

    'yesterday': {
        'date_1': '7日星期三',
        'high_1': '高温 2℃',
        'low_1': '低温 -9℃',
        'day_1': {
            'type_1': '多云',
            'fx_1': '北风',
            'fl_1': '4-5级',
        },
        'night_1': {
            'type_1': '晴',
            'fx_1': '西南风',
            'fl_1': '<3级',
        },
    },

    'forecast': [
        {
            'date': '8日星期四',
            'high': '高温 3℃',
            'low': '低温 -7℃',
            'day': {
                'type': '多云',
                'fengxiang': '西南风',
                'fengli': '3-4级',
            },
            'night': {
                'type': '多云',
                'fengxiang': '北风',
                'fengli': '<3级',
            },
        },
        {
            'date': '9日星期五',
            'high': '高温 5℃',
            'low': '低温 -9℃',
            'day': {
                'type': '多云',
                'fengxiang': '西北风',
                'fengli': '4-5级',
            },
            'night': {
                'type': '晴',
                'fengxiang': '西北风',
                'fengli': '3-4级',
            },
        },
        {
            'date': '10日星期六',
            'high': '高温 0℃',
            'low': '低温 -10℃',
            'day': {
                'type': '晴',
                'fengxiang': '西北风',
                'fengli': '3-4级',
            },
            'night': {
                'type': '晴',
                'fengxiang': '西北风',
                'fengli': '<3级',
            },
        },
        {
            'date': '11日星期天',
            'high': '高温 0℃',
            'low': '低温 -8℃',
            'day': {
                'type': '晴',
                'fengxiang': '西北风',
                'fengli': '3-4级',
            },
            'night': {
                'type': '晴',
                'fengxiang': '西北风',
                'fengli': '<3级',
            },
        },
        {
            'date': '12日星期一',
            'high': '高温 4℃',
            'low': '低温 -6℃',
            'day': {
                'type': '晴',
                'fengxiang': '西北风',
                'fengli': '<3级',
            },
            'night': {
                'type': '晴',
                'fengxiang': '北风',
                'fengli': '<3级',
            },
        },
    ],

    'zhishus': [
        {
            'name': '晨练指数',
            'value': '较不宜',
            'detail': '风力稍大，较不宜晨练，室外锻炼请注意选择避风的地点，避免迎风锻炼。',
        },
        {
            'name': '舒适度',
            'value': '较不舒适',
            'detail': '白天天气晴好，但仍会使您感觉偏冷，不很舒适，请注意适时添加衣物，以防感冒。',
        },
        {
            'name': '穿衣指数',
            'value': '冷',
            'detail':
            '天气冷，建议着棉服、羽绒服、皮夹克加羊毛衫等冬季服装。年老体弱者宜着厚棉衣、冬大衣或厚羽绒服。',
        },
        {
            'name': '感冒指数',
            'value': '较易发',
            'detail':
            '天凉，昼夜温差较大，较易发生感冒，请适当增减衣服，体质较弱的朋友请注意适当防护。',
        },
        {
            'name': '晾晒指数',
            'value': '基本适宜',
            'detail': '天气不错，午后温暖的阳光仍能满足你驱潮消霉杀菌的晾晒需求。',
        },
        {
            'name': '旅游指数',
            'value': '一般',
            'detail':
            '天空状况还是比较好的，但温度比较低，且风稍大，会让人感觉有点冷。外出请备上防风保暖衣物。',
        },
        {
            'name': '紫外线强度',
            'value': '最弱',
            'detail':
            '属弱紫外线辐射天气，无需特别防护。若长期在户外，建议涂擦SPF在8-12之间的防晒护肤品。',
        },
        {
            'name': '洗车指数',
            'value': '较适宜',
            'detail': '较适宜洗车，未来一天无雨，风力较小，擦洗一新的汽车至少能保持一天。',
        },
        {
            'name': '运动指数',
            'value': '较不宜',
            'detail':
            '天气较好，但考虑天气寒冷，风力较强，推荐您进行室内运动，若户外运动请注意保暖并做好准备活动。',
        },
        {
            'name': '约会指数',
            'value': '较不适宜',
            'detail':
            '天气较冷，且室外有风，外出约会可能会让恋人受些苦，最好在温暖的室内促膝谈心。',
        },
        {
            'name': '雨伞指数',
            'value': '不带伞',
            'detail': '天气较好，不会降水，因此您可放心出门，无须带雨伞。',
        },
    ],
}

import requests
import xml.etree.ElementTree as ElementTree

def __xmltodict(xml):
    res = {}
    for elem in xml:
        res[elem.tag] = elem.text if elem.text else __xmltodict(elem)
    return res

def weather(city):
    res = requests.post('http://wthrcdn.etouch.cn/WeatherApi?city=%s' % city)
    xml = ElementTree.fromstring(res.text)

    res = {}
    for elem in xml:
        if elem.tag == 'forecast' or elem.tag == 'zhishus':
            res[elem.tag] = []
            for e in elem:
                res[elem.tag].append(__xmltodict(e))
        else:
            res[elem.tag] = elem.text if elem.text else __xmltodict(elem)
    return res

if __name__ == '__main__':
    import sys
    from docopt import docopt

    arguments = docopt(__doc__)
    res = weather(arguments['<city>'])
    print(arguments['<city>'])

    if 'error' in res:
        print(res['error'])
        sys.exit(1)

    print('城市:', res['city'])
    print('温度:', res['wendu'] + '℃')
    print('湿度:', res['shidu'])
    print('风力:', res['fengli'])
    print('日出:', res['sunrise_1'])
    print('日落:', res['sunset_1'])

    if 'environment' in res:
        print(res['environment'])

    print('')
    print('更新时间:', res['updatetime'])
