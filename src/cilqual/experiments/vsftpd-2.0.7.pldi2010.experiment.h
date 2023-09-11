#include "null_prelude.h"


#if EXPERIMENT_CONDITION == 0
    // typed only
#elif EXPERIMENT_CONDITION == 1
    // vsf_sysutil_sockaddr_clear
    #define ENABLE_MIX1
    #define MIX1(x) MIX(x)
#elif EXPERIMENT_CONDITION == 2
    // str_next_dirent
    #define ENABLE_MIX2
    #define MIX2(x) MIX(x)
#elif EXPERIMENT_CONDITION == 3
    // vsf_sysutil_malloc
    #define ENABLE_MIX3
    #define MIX3(x) MIX(x)
#elif EXPERIMENT_CONDITION == 4
    // main -- vsf_sysutil_dns_resolve
    #define ENABLE_MIX4
    #define MIX4(x) MIX(x)
#elif EXPERIMENT_CONDITION == 5
    // vsf_sysutil_sockaddr_clear and str_next_dirent
    #define ENABLE_MIX1
    #define MIX1(x) MIX(x)
    #define ENABLE_MIX2
    #define MIX2(x) MIX(x)
#else
    #error No more experiment conditions.
#endif


#ifndef MIX1
    #define MIX1(x)
#endif
#ifndef MIX2
    #define MIX2(x)
#endif
#ifndef MIX3
    #define MIX3(x)
#endif
#ifndef MIX4
    #define MIX4(x)
#endif

