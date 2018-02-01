import os

icones_dir = os.path.join(os.path.curdir, 'icones')

APP_NAME = 'liveSEC'
APP_ICON = os.path.join(icones_dir, 'dog.png')

WIN_SIZE = (768, 475)

TOOLBAR_MAX_HEIGHT = 48
NAVIGATION_MAX_WIDTH = 176

NAV_ITEMS = (
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
)

icones_music_dir = os.path.join(icones_dir, 'music')
CONFIG_MUSIC = {
    'control-bar': {
        'minimum-height': 48,
        'maximum-height': 60,
        'background-color': '#002b36',

        'button-prev': {
            'size': (32, 32),
            'border-images': (
                os.path.join(icones_music_dir, 'prev1.png'),
                os.path.join(icones_music_dir, 'prev2.png'),
                os.path.join(icones_music_dir, 'prev3.png'),
            ),
        },

        'stack-play-and-pause': {
            'size': (32, 32),

            'button-play': {
                'size': (32, 32),
                'border-images': (
                    os.path.join(icones_music_dir, 'play1.png'),
                    os.path.join(icones_music_dir, 'play2.png'),
                    os.path.join(icones_music_dir, 'play3.png'),
                ),
            },

            'button-pause': {
                'size': (32, 32),
                'border-images': (
                    os.path.join(icones_music_dir, 'pause1.png'),
                    os.path.join(icones_music_dir, 'pause2.png'),
                    os.path.join(icones_music_dir, 'pause3.png'),
                ),
            },
        },

        'button-next': {
            'size': (32, 32),
            'border-images': (
                os.path.join(icones_music_dir, 'next1.png'),
                os.path.join(icones_music_dir, 'next2.png'),
                os.path.join(icones_music_dir, 'next3.png'),
            ),
        },

        'stack-order': {
            'size': (24, 24),

            'button-loop': {
                'size': (24, 24),
                'border-images': (
                    os.path.join(icones_music_dir, 'loop1.png'),
                    os.path.join(icones_music_dir, 'loop2.png'),
                    os.path.join(icones_music_dir, 'loop3.png'),
                ),
            },

            'button-repeat': {
                'size': (24, 24),
                'border-images': (
                    os.path.join(icones_music_dir, 'repeat1.png'),
                    os.path.join(icones_music_dir, 'repeat2.png'),
                    os.path.join(icones_music_dir, 'repeat3.png'),
                ),
            },

            'button-random': {
                'size': (24, 24),
                'border-images': (
                    os.path.join(icones_music_dir, 'random1.png'),
                    os.path.join(icones_music_dir, 'random2.png'),
                    os.path.join(icones_music_dir, 'random3.png'),
                ),
            },
        },

        'label-played-time': {
            'style-sheet': '''
                color: #93A1A1; margin: 0px 0px 3px 0px;
            ''',
        },

        'progress-bar': {
        },

        'label-total-time': {
            'style-sheet':'''
                color: #93A1A1; margin: 0px 0px 3px 0px;
            ''',
        },

        'stack-volume': {
            'size': (32, 32),

            'button-volume-mute': {
                'size': (32, 32),
                'border-images': (
                    os.path.join(icones_music_dir, 'volume-mute1.png'),
                    os.path.join(icones_music_dir, 'volume-mute2.png'),
                    os.path.join(icones_music_dir, 'volume-mute3.png'),
                ),
            },

            'button-volume-low': {
                'size': (32, 32),
                'border-images': (
                    os.path.join(icones_music_dir, 'volume-low1.png'),
                    os.path.join(icones_music_dir, 'volume-low2.png'),
                    os.path.join(icones_music_dir, 'volume-low3.png'),
                ),
            },

            'button-volume-medium': {
                'size': (32, 32),
                'border-images': (
                    os.path.join(icones_music_dir, 'volume-medium1.png'),
                    os.path.join(icones_music_dir, 'volume-medium2.png'),
                    os.path.join(icones_music_dir, 'volume-medium3.png'),
                ),
            },

            'button-volume-high': {
                'size': (32, 32),
                'border-images': (
                    os.path.join(icones_music_dir, 'volume-high1.png'),
                    os.path.join(icones_music_dir, 'volume-high2.png'),
                    os.path.join(icones_music_dir, 'volume-high3.png'),
                ),
            },
        },

        'button-lyric': {
            'size': (32, 32),
            'border-images': (
                os.path.join(icones_music_dir, 'lyric1.png'),
                os.path.join(icones_music_dir, 'lyric2.png'),
                os.path.join(icones_music_dir, 'lyric3.png'),
            ),
        },

        'button-playlist': {
            'size': (32, 32),
            'border-images': (
                os.path.join(icones_music_dir, 'playlist1.png'),
                os.path.join(icones_music_dir, 'playlist2.png'),
                os.path.join(icones_music_dir, 'playlist3.png'),
            ),
        },
    },
}
