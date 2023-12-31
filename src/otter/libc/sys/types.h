#ifndef _SYS_TYPES_H
#define _SYS_TYPES_H

/* TODO: revisit these definitions as the types get used */
#include <stddef.h>

// See http://www.opengroup.org/onlinepubs/009695399/basedefs/sys/types.h.html
typedef int blkcnt_t;
typedef int blksize_t;
typedef int clock_t;
typedef unsigned int clockid_t;
typedef unsigned int dev_t;
typedef unsigned int fsblkcnt_t;
typedef unsigned int fsfilcnt_t;
typedef unsigned int gid_t;
typedef unsigned int id_t;
typedef unsigned int ino_t;
typedef unsigned int mode_t;
typedef unsigned int nlink_t;
typedef int off_t;
typedef int pid_t;
typedef int ssize_t;
typedef int suseconds_t;
typedef int time_t;
typedef unsigned int uid_t;
typedef unsigned int useconds_t;
// See http://refspecs.freestandards.org/LSB_4.0.0/LSB-Core-generic/LSB-Core-generic/libc-ddefs.html
typedef char *caddr_t;

/* most of these should be structs */
typedef int key_t;
typedef int pthread_attr_t;
typedef int pthread_cond_t;
typedef int pthread_condattr_t;
typedef int pthread_key_t;
typedef int pthread_mutex_t;
typedef int pthread_mutexattr_t;
typedef int pthread_once_t;
typedef int pthread_rwlock_t;
typedef int pthread_rwlockattr_t;
typedef int pthread_t;

#define IOV_MAX 256
#define UIO_MAXIOV IOV_MAX

#endif
