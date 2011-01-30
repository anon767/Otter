/*
 * Map _malloc_r to malloc
 */
#include <stdlib.h>

#ifndef _malloc_r
void * _malloc_r(struct _reent * r, size_t s) {
    return malloc(s);
}
#endif
