#!/usr/bin/python3
# -*- coding:utf-8

import os

icones_dir = os.path.join(os.path.curdir, 'icones')
icones_dir_music = os.path.join(icones_dir, 'music')

main_config = {
    'app-name': 'liveSEC',
    'app-icon': os.path.join(icones_dir, 'app.png'),
    'minimum-size': (768, 475),

    'toolbar': {
        'maximum-height': 48,
    },

    'navigation': {
        'maximum-width': 176,
        'items': (
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
        ),
    },

    'container': {
        'music': {
            'control-bar': {
                'minimum-height': 48,
                'maximum-height': 60,
                'background-color': '#002b36',

                'button-prev': {
                    'size': (32, 32),
                    'border-images': (
                        os.path.join(icones_dir_music, 'prev1.png'),
                        os.path.join(icones_dir_music, 'prev2.png'),
                        os.path.join(icones_dir_music, 'prev3.png'),
                    ),
                },

                'stack-play-and-pause': {
                    'size': (32, 32),

                    'button-play': {
                        'size': (32, 32),
                        'border-images': (
                            os.path.join(icones_dir_music, 'play1.png'),
                            os.path.join(icones_dir_music, 'play2.png'),
                            os.path.join(icones_dir_music, 'play3.png'),
                        ),
                    },

                    'button-pause': {
                        'size': (32, 32),
                        'border-images': (
                            os.path.join(icones_dir_music, 'pause1.png'),
                            os.path.join(icones_dir_music, 'pause2.png'),
                            os.path.join(icones_dir_music, 'pause3.png'),
                        ),
                    },
                },

                'button-next': {
                    'size': (32, 32),
                    'border-images': (
                        os.path.join(icones_dir_music, 'next1.png'),
                        os.path.join(icones_dir_music, 'next2.png'),
                        os.path.join(icones_dir_music, 'next3.png'),
                    ),
                },

                'stack-order': {
                    'size': (24, 24),

                    'button-loop': {
                        'size': (24, 24),
                        'border-images': (
                            os.path.join(icones_dir_music, 'loop1.png'),
                            os.path.join(icones_dir_music, 'loop2.png'),
                            os.path.join(icones_dir_music, 'loop3.png'),
                        ),
                    },

                    'button-repeat': {
                        'size': (24, 24),
                        'border-images': (
                            os.path.join(icones_dir_music, 'repeat1.png'),
                            os.path.join(icones_dir_music, 'repeat2.png'),
                            os.path.join(icones_dir_music, 'repeat3.png'),
                        ),
                    },

                    'button-random': {
                        'size': (24, 24),
                        'border-images': (
                            os.path.join(icones_dir_music, 'random1.png'),
                            os.path.join(icones_dir_music, 'random2.png'),
                            os.path.join(icones_dir_music, 'random3.png'),
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
                            os.path.join(icones_dir_music, 'volume-mute1.png'),
                            os.path.join(icones_dir_music, 'volume-mute2.png'),
                            os.path.join(icones_dir_music, 'volume-mute3.png'),
                        ),
                    },

                    'button-volume-low': {
                        'size': (32, 32),
                        'border-images': (
                            os.path.join(icones_dir_music, 'volume-low1.png'),
                            os.path.join(icones_dir_music, 'volume-low2.png'),
                            os.path.join(icones_dir_music, 'volume-low3.png'),
                        ),
                    },

                    'button-volume-medium': {
                        'size': (32, 32),
                        'border-images': (
                            os.path.join(icones_dir_music, 'volume-medium1.png'),
                            os.path.join(icones_dir_music, 'volume-medium2.png'),
                            os.path.join(icones_dir_music, 'volume-medium3.png'),
                        ),
                    },

                    'button-volume-high': {
                        'size': (32, 32),
                        'border-images': (
                            os.path.join(icones_dir_music, 'volume-high1.png'),
                            os.path.join(icones_dir_music, 'volume-high2.png'),
                            os.path.join(icones_dir_music, 'volume-high3.png'),
                        ),
                    },
                },

                'button-lyric': {
                    'size': (32, 32),
                    'border-images': (
                        os.path.join(icones_dir_music, 'lyric1.png'),
                        os.path.join(icones_dir_music, 'lyric2.png'),
                        os.path.join(icones_dir_music, 'lyric3.png'),
                    ),
                },

                'button-playlist': {
                    'size': (32, 32),
                    'border-images': (
                        os.path.join(icones_dir_music, 'playlist1.png'),
                        os.path.join(icones_dir_music, 'playlist2.png'),
                        os.path.join(icones_dir_music, 'playlist3.png'),
                    ),
                },
            },
        },
    },
}

if __name__ == '__main__':
    import sys
    import json

    if len(sys.argv) <= 1:
        print(main_config)
    else:
        with open(sys.argv[1], 'w') as f:
            json.dump(main_config, f)
