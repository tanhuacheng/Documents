/*****************************************************************************
 * equalizer.c:
 *****************************************************************************
 * Copyright (C) 2004-2012 VLC authors and VideoLAN
 * $Id: 98a4a7ee46bfb692639d28d2b25e0805a6b4fd13 $
 *
 * Authors: Laurent Aimar <fenrir@via.ecp.fr>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston MA 02110-1301, USA.
 *****************************************************************************/

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>
#include "equalizer.h"

/*****************************************************************************
 * Local prototypes
 *****************************************************************************/
struct filter_sys_t
{
    /* Filter static config */
    int i_band;
    float *f_alpha;
    float *f_beta;
    float *f_gamma;

    /* Filter dyn config */
    float *f_amp;   /* Per band amp */
    float f_gamp;   /* Global preamp */
    bool b_2eqz;

    /* Filter state */
    float x[32][2];
    float y[32][128][2];

    /* Second filter state */
    float x2[32][2];
    float y2[32][128][2];
};

#define EQZ_IN_FACTOR (0.25f)

/*****************************************************************************
 * Equalizer stuff
 *****************************************************************************/
typedef struct
{
    int i_band;

    struct
    {
        float f_frequency;
        float f_alpha;
        float f_beta;
        float f_gamma;
    } band[EQZ_BANDS_MAX];
} eqz_config_t;

/* Equalizer coefficient calculation function based on equ-xmms */
static void EqzCoeffs( int i_rate, float f_octave_percent,
                       bool b_use_vlc_freqs,
                       eqz_config_t *p_eqz_config )
{
    const float *f_freq_table_10b = b_use_vlc_freqs
                                  ? f_vlc_frequency_table_10b
                                  : f_iso_frequency_table_10b;
    float f_rate = (float) i_rate;
    float f_nyquist_freq = 0.5f * f_rate;
    float f_octave_factor = powf( 2.0f, 0.5f * f_octave_percent );
    float f_octave_factor_1 = 0.5f * ( f_octave_factor + 1.0f );
    float f_octave_factor_2 = 0.5f * ( f_octave_factor - 1.0f );

    p_eqz_config->i_band = EQZ_BANDS_MAX;

    for( int i = 0; i < EQZ_BANDS_MAX; i++ )
    {
        float f_freq = f_freq_table_10b[i];

        p_eqz_config->band[i].f_frequency = f_freq;

        if( f_freq <= f_nyquist_freq )
        {
            float f_theta_1 = ( 2.0f * (float) M_PI * f_freq ) / f_rate;
            float f_theta_2 = f_theta_1 / f_octave_factor;
            float f_sin     = sinf( f_theta_2 );
            float f_sin_prd = sinf( f_theta_2 * f_octave_factor_1 )
                            * sinf( f_theta_2 * f_octave_factor_2 );
            float f_sin_hlf = f_sin * 0.5f;
            float f_den     = f_sin_hlf + f_sin_prd;

            p_eqz_config->band[i].f_alpha = f_sin_prd / f_den;
            p_eqz_config->band[i].f_beta  = ( f_sin_hlf - f_sin_prd ) / f_den;
            p_eqz_config->band[i].f_gamma = f_sin * cosf( f_theta_1 ) / f_den;
        }
        else
        {
            /* Any frequency beyond the Nyquist frequency is no good... */
            p_eqz_config->band[i].f_alpha =
            p_eqz_config->band[i].f_beta  =
            p_eqz_config->band[i].f_gamma = 0.0f;
        }
    }
}

static inline float EqzConvertdB( float db )
{
    /* Map it to gain,
     * (we do as if the input of iir is /EQZ_IN_FACTOR, but in fact it's the non iir data that is *EQZ_IN_FACTOR)
     * db = 20*log( out / in ) with out = in + amp*iir(i/EQZ_IN_FACTOR)
     * or iir(i) == i for the center freq so
     * db = 20*log( 1 + amp/EQZ_IN_FACTOR )
     * -> amp = EQZ_IN_FACTOR*(10^(db/20) - 1)
     **/

    if( db < -20.0f )
        db = -20.0f;
    else if(  db > 20.0f )
        db = 20.0f;
    return EQZ_IN_FACTOR * ( powf( 10.0f, db / 20.0f ) - 1.0f );
}

void* EqzInit (int rate, bool use_vlc_freqs, bool use_twopass, const eqz_preset_t* preset)
{
    struct filter_sys_t *p_sys = calloc(1, sizeof(*p_sys));

    eqz_config_t cfg;
    EqzCoeffs(rate, 1.0f, use_vlc_freqs, &cfg);

    /* Create the static filter config */
    p_sys->i_band = cfg.i_band;
    p_sys->f_alpha = malloc( p_sys->i_band * sizeof(float) );
    p_sys->f_beta  = malloc( p_sys->i_band * sizeof(float) );
    p_sys->f_gamma = malloc( p_sys->i_band * sizeof(float) );

    for (int i = 0; i < p_sys->i_band; i++)
    {
        p_sys->f_alpha[i] = cfg.band[i].f_alpha;
        p_sys->f_beta[i]  = cfg.band[i].f_beta;
        p_sys->f_gamma[i] = cfg.band[i].f_gamma;
    }

    /* Filter dyn config */
    p_sys->b_2eqz = use_twopass;

    if(preset->f_preamp < -20.f )
        p_sys->f_gamp = .1f;
    else if(preset->f_preamp < 20.f )
        p_sys->f_gamp = powf( 10.f, preset->f_preamp / 20.f );
    else
        p_sys->f_gamp = 10.f;

    p_sys->f_amp = malloc(p_sys->i_band * sizeof(float));
    for (int i = 0; i < p_sys->i_band; i++)
    {
        p_sys->f_amp[i] = EqzConvertdB(preset->f_amp[i]);
    }

    /* Filter state */
    for (int ch = 0; ch < 32; ch++)
    {
        p_sys->x[ch][0]  =
        p_sys->x[ch][1]  =
        p_sys->x2[ch][0] =
        p_sys->x2[ch][1] = 0.0f;

        for (int i = 0; i < p_sys->i_band; i++ )
        {
            p_sys->y[ch][i][0]  =
            p_sys->y[ch][i][1]  =
            p_sys->y2[ch][i][0] =
            p_sys->y2[ch][i][1] = 0.0f;
        }
    }

    return p_sys;
}

void EqzFilter(void* inst, float *out, float *in, int samples, int channels)
{
    struct filter_sys_t *p_sys = inst;

    for(int i = 0; i < samples; i++)
    {
        for (int ch = 0; ch < channels; ch++)
        {
            const float x = in[ch];
            float o = 0.0f;

            for (int j = 0; j < p_sys->i_band; j++ )
            {
                float y = p_sys->f_alpha[j] * ( x - p_sys->x[ch][1] ) +
                          p_sys->f_gamma[j] * p_sys->y[ch][j][0] -
                          p_sys->f_beta[j]  * p_sys->y[ch][j][1];

                p_sys->y[ch][j][1] = p_sys->y[ch][j][0];
                p_sys->y[ch][j][0] = y;

                o += y * p_sys->f_amp[j];
            }
            p_sys->x[ch][1] = p_sys->x[ch][0];
            p_sys->x[ch][0] = x;

            /* Second filter */
            if( p_sys->b_2eqz )
            {
                const float x2 = EQZ_IN_FACTOR * x + o;
                o = 0.0f;
                for (int j = 0; j < p_sys->i_band; j++ )
                {
                    float y = p_sys->f_alpha[j] * ( x2 - p_sys->x2[ch][1] ) +
                              p_sys->f_gamma[j] * p_sys->y2[ch][j][0] -
                              p_sys->f_beta[j]  * p_sys->y2[ch][j][1];

                    p_sys->y2[ch][j][1] = p_sys->y2[ch][j][0];
                    p_sys->y2[ch][j][0] = y;

                    o += y * p_sys->f_amp[j];
                }
                p_sys->x2[ch][1] = p_sys->x2[ch][0];
                p_sys->x2[ch][0] = x2;

                /* We add source PCM + filtered PCM */
                out[ch] = p_sys->f_gamp * p_sys->f_gamp *( EQZ_IN_FACTOR * x2 + o );
            }
            else
            {
                /* We add source PCM + filtered PCM */
                out[ch] = p_sys->f_gamp *( EQZ_IN_FACTOR * x + o );
            }
        }

        in  += channels;
        out += channels;
    }
}

void EqzClean (void* inst)
{
    struct filter_sys_t *p_sys = inst;

    free(p_sys->f_alpha);
    free(p_sys->f_beta);
    free(p_sys->f_gamma);
    free(p_sys->f_amp);
    free(p_sys);
}

// end of file
