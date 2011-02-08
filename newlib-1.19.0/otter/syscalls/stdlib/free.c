#include <stdlib.h>

/* Without a definition of Otter-builtin functions, programs can't call them
	 through function pointers. However, if we provide a definition that *can* be
	 called through a function pointer, it really gets invoked. The calls here,
	 though, are *not* through function pointers, so they *will* get intercepted
	 as usual. Thus, despite looking like it, these are not infinite recursions
	 for Otter. */

void free(void *p) {
    free(p);
}

void __otter_multi_gfree(void *p) {
    __otter_multi_gfree(p);
}
