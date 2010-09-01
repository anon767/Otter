#ifndef _SYS_TYPES_H
#define _SYS_TYPES_H

/* TODO: revisit these definitions as the types get used */
#include <stddef.h>

#define blkcnt_t int
#define blksize_t unsigned int
#define clock_t int
#define clockid_t unsigned int
#define dev_t unsigned int
#define fsblkcnt_t unsigned int
#define fsfilcnt_t unsigned int
#define gid_t unsigned int
#define id_t unsigned int
#define ino_t unsigned int
#define mode_t unsigned int
#define nlink_t unsigned int
#define off_t int
#define pid_t unsigned int
#define ssize_t unsigned int
#define suseconds_t unsigned int
#define time_t int
#define uid_t unsigned int
#define useconds_t unsigned int

/* most of these should be structs */
#define key_t int
#define pthread_attr_t int
#define pthread_cond_t int
#define pthread_condattr_t int
#define pthread_key_t int
#define pthread_mutex_t int
#define pthread_mutexattr_t int
#define pthread_once_t int
#define pthread_rwlock_t int
#define pthread_rwlockattr_t int
#define pthread_t int

#endif
