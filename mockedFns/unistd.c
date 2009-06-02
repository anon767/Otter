#include "iosim.h"
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>

#if __HAVE_getpid__
pid_t        getpid(void){
	//__LIMIT_FUNC(getpid,1);
	return (pid_t) 1;
}
#endif

#if __HAVE_getgid__
gid_t        getgid(void){ 
	return (gid_t) SERVER_GID;
}
#endif

//#if __HAVE_fork__
pid_t fork(void){
//	static pid_t new_pid = 0;
//	if ((char)__SYMBOLIC()) {
//		__COMMENT("Forking to parent");
//		new_pid++;
//		return new_pid; // child process id > 0
//	}
	__COMMENT("Forking to child");
	return 0;
}
//#endif

//#if __HAVE_getuid__
uid_t        getuid(void){
	return (uid_t) 0; // vsftpd must run as root
}
//#endif

#if __HAVE_gethostname__
int gethostname(char *name, size_t namelen){
	strcpy(name,SERVER_IP);
	return 0;
}
#endif

#if __HAVE_pipe__
int pipe(int fildes[2]){
	fildes[0] = IOSIM_newfd(); // 6
	fildes[1] = IOSIM_newfd(); // 7, never use coz we don't really fork a child
	return 0;
}
#endif

//#if __HAVE_read__
ssize_t read(int fildes, void *buf, size_t nbyte){
	return IOSIM_read(fildes, buf, nbyte);
}	
//#endif

//ssize_t pread(int fildes, void *buf, size_t nbyte, off_t offset) {
//	if (fildes >= IOSIM_num_fd || fildes < 0) {
//		errno = EBADF;
//		return -1;
//	}
//	IO_BUF *inbuf = IOSIM_fd[fildes];
//	if (!inbuf) {
//		errno = EBADF;
//		return -1;
//	}
//	off_t origOffset = inbuf->cur; // Remember the old offset
//	inbuf->cur = offset; // Read from the given offset
//	ssize_t numRead = IOSIM_read(fildes, buf, nbyte);
//	inbuf->cur = origOffset; // Reset the offset
//	return numRead;
//}

//#if __HAVE_close__
int close(int fildes){
	IOSIM_close(fildes);
	return 0;
}
//#endif

//char write_output[10][1000];
ssize_t write(int fildes, const void *buf, size_t nbyte){
//	static int call_count = 0;
//
//	call_count ++;
//	switch(call_count){
//	case 1:
//	case 2:
//	case 3:
//	case 4:
//	default: 
		//assert(fildes==5);
		//strncat(write_output[fildes],buf,nbyte);
		//__EVALSTR(write_output[fildes]);

		IOSIM_write(fildes, buf, nbyte);
		return nbyte;
//	}
//	return -1;
}

pid_t setsid() {
	return 0;
}
pid_t getpid() {
	return 0;
}
pid_t getpgrp() {
	return 0;
}
int setgid(gid_t gid) {
	return 0;
}
int setuid(uid_t uid) {
	return 0;
}
char *getcwd(char *buf, size_t size) {
	return IOSIM_getcwd(buf,size);
}
int dup2(int fildes, int fildes2) {
	IOSIM_fd[fildes2] = IOSIM_fd[fildes];
	return fildes2;
}

off_t lseek(int fildes, off_t offset, int whence) {
	if (fildes >= IOSIM_num_fd || fildes < 0) {
		errno = EBADF;
		return -1;
	}
	sym_file_stream_t *sym_stream = IOSIM_fd[fildes];
	off_t newOffset;
	if (!sym_stream) {
		errno = EBADF;
		return -1;
	}
	switch (whence) {
	case SEEK_SET:
		newOffset = offset;
		break;
	case SEEK_CUR:
		newOffset = sym_stream->offset + offset;
		break;
	case SEEK_END:
		newOffset = sym_stream->sym_file->stat.st_size + offset;
		break;
	default:
		errno = EINVAL;
		return -1;
	}
	if (newOffset < 0) {
		errno = EINVAL;
		return -1;
	}
	sym_stream->offset = newOffset;

	free(sym_stream->buffer);
	sym_stream->buffer = NULL;

	return newOffset;
}

int isatty(int fildes)
{
  return 1;
}

