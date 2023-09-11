#ifndef _SYS_MMAN_H
#define _SYS_MMAN_H

#define PROT_READ 4
#define PROT_WRITE 2
#define PROT_EXEC 1
#define PROT_NONE 0

#define MAP_SHARED 1
#define MAP_PRIVATE 2
#define MAP_FIXED 4

//#define MAP_FAILED (-1)
#define MAP_FAILED 0

#include <sys/types.h>

void *mmap(void *addr, size_t len, int prot, int flags, int fd, off_t off);
int munmap(void *addr, size_t len);
int mprotect(void *addr, size_t len, int prot);

#endif
