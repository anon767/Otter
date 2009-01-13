#include <stdlib.h>
#include <errno.h>

void* realloc(void* ptr, size_t size) {
	if (ptr == NULL) {
		return malloc(size);
	}

	if (size == 0) {
		free(ptr);
		return NULL;
	}

	// currently memory block has unbounded length
	return ptr;
//	void* result = malloc(size);
//	if (result == NULL) {
//		errno = ENOMEM;
//		return NULL;
//	}
//
//	memcpy(result, ptr, size);
//	free(ptr);
//
//	return result;
}
