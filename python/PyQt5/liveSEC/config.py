import os

icones_dir = os.path.join(os.path.curdir, 'icones')

APP_NAME = 'liveSEC'
APP_ICON = os.path.join(icones_dir, 'app.jpg')

WIN_SIZE = (1280, 791)

TOOLBAR_MAX_HEIGHT = 60
NAVIGATION_MAX_WIDTH = 200

NAV_ITEMS = [
    {
        'id': 'scene',
        'text': '情景',
        'icones': {
            'normal': os.path.join(icones_dir, 'nav_scene_normal'),
            'active': os.path.join(icones_dir, 'nav_scene_active.png'),
        },
    },

    {
        'id': 'music',
        'text': '音乐',
        'icones': {
            'normal': os.path.join(icones_dir, 'nav_music_normal'),
            'active': os.path.join(icones_dir, 'nav_music_active.png'),
        },
    },
]
