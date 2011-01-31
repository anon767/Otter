#include <stddef.h> // For size_t
#include <stdlib.h> // For malloc
#include <string.h> // For memcpy
#include "otter/otter_builtins.h" // For __otter_get_allocated_size

/* This realloc *always* frees the original pointer and returns a new one. Some
   specs aren't entirely clear about whether this is valid for a call which
   *shrinks* the allocated size (this is, someone might assume that a call which
   shrinks the size would always return the original pointer), but the ANSI spec
   *does* make it clear that this is acceptable behavior. */

void* realloc(void* ptr, size_t size)
{
	if (ptr == NULL)
		return malloc(size);
	if (size == 0)
	{
		free(ptr);
		return 0;
	}

	int old_size = __otter_get_allocated_size(ptr);
	char* ptr2 = malloc(size);
	size = (size > old_size) ? old_size : size;
	memcpy(ptr2, ptr, size);
	free(ptr);

	return ptr2;
}
