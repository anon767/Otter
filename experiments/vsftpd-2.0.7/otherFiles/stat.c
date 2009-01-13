#include "iosim.h"
#include <stdlib.h>
#include <errno.h>

#define __USE_FILE_OFFSET64
#include <sys/stat.h>

int __xstat(int __ver, const char *__filename, struct stat *__stat_buf) {
	if (IOSIM_findfile(__filename) != NULL) {
		// What information do I put in __stat_buf?
		return 0;
	}
	return -1;
/* 	const char *symbolic_file; */
/* 	if (__filename == symbolic_file) { */
/* 		return 1; */
/* 	} */
/* 	return 0; */
}

int __fxstat(int __ver, int __fildes, struct stat *__stat_buf) {
	// Handle stats of stdio specially
	if (__fildes == 0) {
		__stat_buf->st_mode = S_IFSOCK;
		return 0;
	}
		
	IO_BUF* buf = IOSIM_fd[__fildes];
	if (buf == NULL) {
		__EVALSTR("Bad file descriptor in __fxstat",31);
		errno = EBADF;
		return -1;
	}
	__stat_buf->st_mode = S_IFREG;
	__stat_buf->st_size = buf->len;
	return 0;
}

int __lxstat(int __ver , char const   *__filename ,
						 struct stat *__stat_buf ) {
	return 0;
}
