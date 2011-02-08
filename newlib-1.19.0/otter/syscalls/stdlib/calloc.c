#include <stddef.h> // For size_t
#include <stdlib.h> // For malloc
#include <string.h> // For memset
#include <otter/multiotter_builtins.h> // For __otter_multi_gmalloc

static void *__otter_calloc(size_t count, size_t size, void *(alloc)(size_t))
{
	size_t len = count * size;
	void *ptr = alloc(len);
	return memset(ptr, 0, len);
}

void *calloc(size_t count, size_t size) {
	return __otter_calloc(count, size, malloc);
}

void *__otter_multi_calloc(size_t count, size_t size) {
	return __otter_calloc(count, size, __otter_multi_gmalloc);
}
