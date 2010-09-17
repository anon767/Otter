#ifndef _FCNTL_H
#define _FCNTL_H

#include <sys/types.h>

#define F_DUPFD 1
#define F_GETFD 2
#define F_SETFD 3
#define F_GETFL 4
#define F_SETFL 5
#define F_GETLK 6
#define F_SETLK 7
#define F_SETLKW 8
#define F_GETOWN 9
#define F_SETOWN 10

#define FD_CLOEXEC 8

#define F_RDLCK 1
#define F_UNLCK 0
#define F_WRLCK 2

/* like GNU */
#define O_ACCMODE 3
#define O_RDONLY 1
#define O_WRONLY 2
#define O_RDWR 3

#define O_CREAT 4
#define O_EXCL 6
#define O_NOCTTY 8
#define O_TRUNC 16

#define O_APPEND 16
#define O_NONBLOCK 32
#define O_SYNC 64

struct flock
{
	short l_type;
	short l_whence;
	off_t l_start;
	off_t l_len;
	pid_t l_pid;
};

int creat(const char* name, mode_t mode);
int fcntl(int fd, int cmd, ...);
int open(const char* name, int oflag, ...);

#endif
