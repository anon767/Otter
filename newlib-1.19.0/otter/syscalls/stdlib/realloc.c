#include <stddef.h> // For size_t
#include <stdlib.h> // For malloc
#include <string.h> // For memcpy
#include <otter/otter_builtins.h> // For __otter_get_allocated_size
#include <otter/multiotter_builtins.h> // For __otter_multi_gmalloc

/* This realloc *always* frees the original pointer and returns a new one. Some
   specs aren't entirely clear about whether this is valid for a call which
   *shrinks* the allocated size (this is, someone might assume that a call which
   shrinks the size would always return the original pointer), but the ANSI spec
   *does* make it clear that this is acceptable behavior. */

static void* __otter_realloc(void* ptr, size_t size, void *(alloc)(size_t), void (my_free)(void *))
{
	if (ptr == NULL)
		return alloc(size);
	if (size == 0)
	{
		my_free(ptr);
		return 0;
	}

	size_t old_size = __otter_get_allocated_size(ptr);
	char* ptr2 = alloc(size);
	size = (size > old_size) ? old_size : size;
	memcpy(ptr2, ptr, size);
	my_free(ptr);

	return ptr2;
}

void* realloc(void* ptr, size_t size) {
	return __otter_realloc(ptr, size, malloc, free);
}

void* __otter_multi_grealloc(void* ptr, size_t size) {
	return __otter_realloc(ptr, size, __otter_multi_gmalloc, __otter_multi_gfree);
}
