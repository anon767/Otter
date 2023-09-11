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

	/* currently memory block has unbounded length, so we just write
		 into the last byte so the symbolic executor knows that the array
		 is that long now. */
	((char*)ptr)[size-1] = 0;
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
