/*
 * Map _free_r to free
 */
#ifndef _free_r
#include <stdlib.h>

void _free_r(struct _reent * r, void *p) {
    free(p);
}
#endif
