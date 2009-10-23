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
		 is that long now. If we prohibit out-of-bounds writes, then we
		 will have to do realloc the obvious (but slightly less efficient)
		 way, using malloc and memcpy, below. */
	((char*)ptr)[size-1] = ((char*)ptr)[size-1];
	return ptr;
//	void* result = malloc(size);
//	memcpy(result, ptr, size);
//	free(ptr);
//
//	return result;
}
