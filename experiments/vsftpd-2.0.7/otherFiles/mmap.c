#include <stdlib.h>

void *mmap(void *__addr, size_t __len, int __prot, int __flags, int __fd, __off_t __offset) {
	void* x = malloc(__len);
//	for (int i = 0; i < __len; ++i) {
//		x[i] = __SYMBOLIC();
//	}
	return x;
}

int munmap(void *addr, size_t len) {
	return 0;
}
