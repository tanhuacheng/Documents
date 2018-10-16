#!/usr/bin/python3

import mediaplayer


class MediaPlayer(mediaplayer.MediaPlayer):

    def on_length_changed(self, value):
        print('length', value)

    def on_position_changed(self, value):
        print('postion', value)

    def on_end_reached(self):
        print('end')

    def on_audio_volume(self, value):
        print('volume', value)


mplayer = MediaPlayer()
mplayer.play('../musics/data/music/douban_p2842584_128k.mp3')
mplayer.play('../musics/data/music/douban_p2842584_128k.mp3')
