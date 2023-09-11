/*
 * Map _realloc_r to reealloc
 */
#include <stdlib.h>

#ifndef _realloc_r
void * _realloc_r(struct _reent * r, void * p, size_t s) {
    return realloc(p, s);
}
#endif
