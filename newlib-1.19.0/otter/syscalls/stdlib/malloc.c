#include <stdlib.h>
#include <otter/multiotter_builtins.h> // For __otter_multi_gmalloc

/* Without a definition of Otter-builtin functions, programs can't call them
	 through function pointers. However, if we provide a definition that *can* be
	 called through a function pointer, it really gets invoked. The call here,
	 though, is *not* through a function pointer, so it *will* get intercepted as
	 usual. Thus, despite looking like it, this is not an infinite recursion for
	 Otter. */

void *malloc(size_t s) {
    return malloc(s);
}

void *__otter_multi_gmalloc(size_t size) {
    return __otter_multi_gmalloc(size);
}
