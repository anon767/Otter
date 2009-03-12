#include "iosim.h"
#include <stdlib.h>
#include <errno.h>

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
//	// Handle stat of fd 0 specially
//	if (__fildes == 0) {
//		__stat_buf->st_mode = S_IFSOCK;
//		return 0;
//	}

	sym_file_stream_t* sym_stream = IOSIM_fd[__fildes];
	if (sym_stream == NULL) {
		__COMMENT("Bad file descriptor in __fxstat");
		errno = EBADF;
		return -1;
	}
//	__stat_buf->st_mode = S_IFREG | S_IROTH; // Regular file, readable by other
//	__stat_buf->st_size = file->len;
	*__stat_buf = sym_stream->sym_file->stat;
	return 0;
}

int __lxstat(int __ver , char const   *__filename ,
						 struct stat *__stat_buf ) {
	return 0;
}
