/*
 * Map _malloc_r to malloc
 */
#include <malloc.h>

#ifndef _malloc_r
_PTR _malloc_r _PARAMS ((struct _reent * r, size_t s)) {
    return malloc(s);
}
#endif
