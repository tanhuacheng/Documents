// guess_localip.h

#ifndef GUESS_LOCALIP_H_KFUNDQHD
#define GUESS_LOCALIP_H_KFUNDQHD

#include <stddef.h>
#include <net/if.h>

#ifdef __cplusplus
extern "C" {
#endif

const char* guess_localip (char* straddr, size_t addrstrlen, unsigned int flag);

#ifdef __cplusplus
}
#endif

#endif /* end of include guard: GUESS_LOCALIP_H_KFUNDQHD */
