import os

icones_dir = os.path.join(os.path.curdir, 'icones')

APP_NAME = 'liveSEC'
APP_ICON = os.path.join(icones_dir, 'dog.png')

WIN_SIZE = (1024, 633)

TOOLBAR_MAX_HEIGHT = 48
NAVIGATION_MAX_WIDTH = 176

NAV_ITEMS = [
    {
        'id': 'scene',
        'text': '情景',
        'icones': {
            'normal': os.path.join(icones_dir, 'nav_scene_normal.png'),
            'active': os.path.join(icones_dir, 'nav_scene_active.png'),
        },
    },

    {
        'id': 'music',
        'text': '音乐',
        'icones': {
            'normal': os.path.join(icones_dir, 'nav_music_normal.png'),
            'active': os.path.join(icones_dir, 'nav_music_active.png'),
        },
    },

    {
        'id': 'weather',
        'text': '天气',
        'icones': {
            'normal': os.path.join(icones_dir, 'nav_weather_normal.png'),
            'active': os.path.join(icones_dir, 'nav_weather_active.png'),
        },
    },
]
