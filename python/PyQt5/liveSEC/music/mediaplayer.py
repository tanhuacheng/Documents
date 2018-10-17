import vlc


class Event:

    def on_length_changed(self, value):
        pass

    def on_position_changed(self, value):
        pass

    def on_end_reached(self):
        pass

    def on_audio_volume(self, value):
        pass


class MediaPlayer(Event):

    def __init__(self):
        self._instance = vlc.Instance()
        self._player = vlc.MediaPlayer(self._instance)
        self._media = None
        self._length = 0

        self._event = self._player.event_manager()
        self._event.event_attach(vlc.EventType.MediaPlayerLengthChanged, self._length_changed)
        self._event.event_attach(vlc.EventType.MediaPlayerPositionChanged, self._position_changed)
        self._event.event_attach(vlc.EventType.MediaPlayerEndReached, self._end_reached)
        self._event.event_attach(vlc.EventType.MediaPlayerAudioVolume, self._audio_volume)

    def play(self, mrl=None):
        if mrl:
            if self._media:
                self._player.stop()
                self._media.release()
            self._media = self._instance.media_new(mrl)
            self._player.set_media(self._media)
            self._player.play()
            self._length = 0
        elif self._media:
            self._player.pause()

    def seek(self, value):
        self._player.set_position(value/100)
        self.on_position_changed(value)

    def volume(self):
        return self._player.audio_get_volume()

    def set_volume(self, value):
        self._player.audio_set_volume(value)

    def _length_changed(self, *args, **kwargs):
        length = self._media.get_duration()
        if length < 0:
            return

        if abs(self._length - length) > 500:
            self._length = length
            self.on_length_changed(length)

    def _position_changed(self, *args, **kwargs):
        if not self._length:
            self._length_changed()
        self.on_position_changed(round(self._player.get_position()*100))

    def _end_reached(self, *args, **kwargs):
        self.on_end_reached()

    def _audio_volume(self, *args, **kwargs):
        self.on_audio_volume(self._player.audio_get_volume())
