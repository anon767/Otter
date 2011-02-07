#ifndef _OTTER_SYS_STAT_H
#define _OTTER_SYS_STAT_H

#include_next <sys/stat.h>

// newlib's sys/stat.h doesn't declare lstat and mknod unless one of __SPU__, __rtems__, or __CYGWIN__ is defined.

int lstat(const char* name, struct stat* s);
int mknod(const char* name, mode_t mode, dev_t dev);

#endif
