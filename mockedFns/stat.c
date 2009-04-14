#include "iosim.h"
#include <stdlib.h>
#include <errno.h>

#include <sys/stat.h>

int __xstat(int __ver, const char *__filename, struct stat *__stat_buf) {
	char *absoluteName = IOSIM_toAbsolute(__filename);
	sym_file_t *file = IOSIM_findfile(absoluteName);
	free(absoluteName);
	if (file) {
		*__stat_buf = file->stat;
		return 0;
	}
	errno = ENOENT;
	return -1;
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
	// We don't have links yet, so this is just normal stat.
	return __xstat(__ver, __filename, __stat_buf);
}
