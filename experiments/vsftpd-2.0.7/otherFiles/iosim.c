#include "iosim.h"
#include <string.h>
#include <stdlib.h>
#include <errno.h>
// filename to buf mapping
#define	IOSIM_MAX_FILE	1024
int		IOSIM_num_file = 0;
char* IOSIM_file_name[IOSIM_MAX_FILE];
IO_BUF* IOSIM_file_buf[IOSIM_MAX_FILE];


// IOSIM_fd[i] is the io buf of fd i
IO_BUF*	IOSIM_fd[IOSIM_MAX_FD];
// fd: 0-stdin, 1-stdout, 2-stderr
// The next available fd. Starting from 3.
int		IOSIM_num_fd = 3;

IO_BUF* IOSIM_newbuf(int len, char* buf){
	IO_BUF* ret = malloc(sizeof(IO_BUF));
	ret->cur = 0;
	if(len<0)
		ret->len = strlen(buf);
	else 
		ret->len = len;
	ret->buf = buf;
	return ret;
}


IO_BUF* IOSIM_findfile(const char *file){
	for(int i=0;i<IOSIM_num_file;i++){
		if(strcmp(file,IOSIM_file_name[i])==0)
			return IOSIM_file_buf[i];
	}
	return NULL;
}

void IOSIM_addfile(const char *file,IO_BUF* buf){
	if (IOSIM_num_file >= IOSIM_MAX_FILE) {
		__EVALSTR("Too many files",14);
		exit(); // This should cause the symbolic executor to halt with 'Failure("hd")'
	}
	IOSIM_file_name[IOSIM_num_file] = file;
	IOSIM_file_buf[IOSIM_num_file] = buf;
	IOSIM_num_file++;
}

int IOSIM_newfd(){
	if (IOSIM_num_fd >= IOSIM_MAX_FD) {
		__EVALSTR("Too many file descriptors",24);
		exit(); // This should cause the symbolic executor to halt with 'Failure("hd")'
	}
	int ret = IOSIM_num_fd;
	IOSIM_num_fd ++;
	return ret;
}

int IOSIM_attach(int fildes, IO_BUF* s){
	IOSIM_fd[fildes] = s;
	return 1;
}

int IOSIM_close(int fildes) {
//	if (fildes < 0 || fildes >= IOSIM_num_fd || IOSIM_fd[fildes] == NULL) {
//		errno = EBADF;
//		return -1;
//	}
//	IOSIM_fd[fildes] = NULL;
	return 0;
}

int IOSIM_read(int fildes, void *buf, int nbyte){
	int n,cur,len;
	char* cbuf = buf;

	if(nbyte == 0) return 0;
	//	if(fildes == 0) return -1;

	IO_BUF* in = IOSIM_fd[fildes];
	cur = in->cur;
	len = in->len;
	for(n=0;n<nbyte;n++){
		if(cur>=len) break;
		cbuf[n] = in->buf[cur];
		cur++;
	}
	in->cur = cur;
	if (n < 0) {
		__EVALSTR("n is negative in IOSIM_read; this shouldn't be",46);
		exit(); // This should cause the symbolic executor to halt with 'Failure("hd")'
	}
	return n;
}
int IOSIM_write(int fildes, const void *buf, int nbyte){
	int n,cur,len;
	char* cbuf = buf;

	if(nbyte == 0) return 0;
	if(fildes == 0) return -1;

	IO_BUF* out = IOSIM_fd[fildes];
	cur = out->cur;
	len = out->len;
	for(n=0;n<nbyte;n++){
		if(cur>=len) break;
		out->buf[cur] = cbuf[n];
		cur++;
	}
	out->cur = cur;
	if(n<=0) return -1;
	else return n;
}
		
