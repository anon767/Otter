#include <stdlib.h>

/* It's weird to have mmap call malloc when in reality it works the
	 other way, but "malloc" is the name of the symbolic executor's
	 built-in memory allocation function, so that's what we do. We don't
	 currently deal with anything with the other arguments of mmap. */
void *mmap(void *start, size_t length, int prot, int flags, int fd, off_t offset) {
	void *x = malloc(length);
	return x;
}

/* munmap is not compatible with free in the same way mmap is
	 compatible with malloc, because of the length argument. Unless the
	 length is the same as the allocated length, just calling free
	 doesn't make sense in our symbolic executor's memory model. One way
	 we might handle this is to set all bytes in the block that is being
	 unmapped to the undefined byte, but for now we just ignore munmap
	 (and therefore run the risk of allowing accesses to memory that has
	 been unmapped). */
int munmap(void *addr, size_t len) {
	return 0;
}
