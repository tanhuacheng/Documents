#include <stdio.h>
#include <stdlib.h>
#include <alsa/asoundlib.h>

static snd_pcm_t* handle;

static int pcm_open (void)
{
    if (snd_pcm_open(&handle, "default", SND_PCM_STREAM_PLAYBACK, 0) < 0) {
        perror("snd_pcm_open");
        return -1;
    }

    if (snd_pcm_set_params(
            handle, SND_PCM_FORMAT_U8, SND_PCM_ACCESS_RW_INTERLEAVED, 1, 8000, 1, 200000) < 0) {
        perror("snd_pcm_set_params");
        return -1;
    }

    return 0;
}

static void pcm_play (unsigned char* buff, int size)
{
    snd_pcm_sframes_t frames = snd_pcm_writei(handle, buff, size);

    if (frames < 0) {
        frames = snd_pcm_recover(handle, frames, 0);
    }
}

static void pcm_close ()
{
    snd_pcm_close(handle);
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    if (pcm_open() < 0) {
        printf("pcm_open error\n");
        return 1;
    }

    int16_t data16[] = {
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
        0, 11585, 16384, 11585, 0, -11585, -16384, -11585,
    };

    char data8[sizeof(data16) / sizeof(data16[0])];
    for (int i = 0; i < (int)sizeof(data8); i++) {
        data8[i] = data16[i] >> 8;
    }

    int count = 5 * 10; // 10s

    while (count--) {
        pcm_play((void*)data16, sizeof(data16));
    }

    pcm_close();

    return 0;
}
