#include "iosim.h"
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <dirent.h>

#define	IOSIM_MAX_FILE	1024
int		IOSIM_num_file = 0; // Number of files currently
char* IOSIM_file_name[IOSIM_MAX_FILE]; // Array of file names
sym_file_t* IOSIM_file[IOSIM_MAX_FILE]; // Array of the files themselves


// IOSIM_fd[i] is the stream associate with descriptor i
sym_file_stream_t*	IOSIM_fd[IOSIM_MAX_FD];
// fd: 0-stdin, 1-stdout, 2-stderr
// The next available fd. Starting from 3.
int		IOSIM_num_fd = 3;

//sym_file_t* IOSIM_newbuf(int len, char* buf){
//	sym_file_stream_t* ret = malloc(sizeof(sym_file_stream_t));
//	ret->cur = 0;
//	if(len<0)
//		ret->len = strlen(buf);
//	else 
//		ret->len = len;
//	ret->buf = buf;
//	return ret;
//}

// If the file exists, return it; otherwise return NULL.
sym_file_t *IOSIM_findfile(const char *file){
	for(int i=0;i<IOSIM_num_file;i++){
		if(strcmp(file,IOSIM_file_name[i])==0)
			return IOSIM_file[i];
	}
	return NULL;
}

// This creates the file and returns it
sym_file_t *IOSIM_addfile(const char *filename, mode_t mode){
	if (IOSIM_num_file >= IOSIM_MAX_FILE) {
		errno = EMFILE;
		return -1;
	}
	if (IOSIM_num_fd >= IOSIM_MAX_FD) {
		errno = ENFILE;
		return -1;
	}
	sym_file_t *file = malloc(sizeof(sym_file_t));
	file->stat.st_size = 0;
	file->contents = NULL;
	file->stat.st_mode = (S_IFREG | mode | S_IROTH); // For now, I'm only making regular files (S_IFREG) which are readable by others (S_IROTH)
	// I probably have to set some more fields here.

	__COMMENT("Adding file");
	__EVALSTR(filename,strlen(filename));

	IOSIM_file_name[IOSIM_num_file] = strdup(filename);
	IOSIM_file[IOSIM_num_file] = file;
	IOSIM_num_file++;

	return file;
}

int IOSIM_newfd(){
	if (IOSIM_num_fd >= IOSIM_MAX_FD) {
		__COMMENT("Too many file descriptors");
		exit(1);
	}
	int ret = IOSIM_num_fd;
	IOSIM_num_fd ++;
	return ret;
}

//int IOSIM_attach(int fildes, sym_file_stream_t* s){
//	IOSIM_fd[fildes] = s;
//	return 1;
//}

int IOSIM_close(int fildes) {
	if (fildes < 0 || fildes >= IOSIM_num_fd || IOSIM_fd[fildes] == NULL) {
		errno = EBADF;
		return -1;
	}
	IOSIM_fd[fildes] = NULL;
	return 0;
}

int IOSIM_read(int fildes, void *buf, int nbyte){
	int n,cur,len;
	char* cbuf = buf;

	if(nbyte == 0) return 0;
	//	if(fildes == 0) return -1;

	sym_file_stream_t* in = IOSIM_fd[fildes];
	cur = in->offset;
	len = in->sym_file->stat.st_size;
	for(n=0;n<nbyte;n++){
		if(cur>=len) break;
		cbuf[n] = in->sym_file->contents[cur];
		cur++;
	}
	in->offset = cur;
	if (n < 0) {
		__COMMENT("n is negative in IOSIM_read; this shouldn't be");
		exit(1);
	}
	return n;
}
int IOSIM_write(int fildes, const void *buf, int nbyte){
	int n,cur,len;
	char* cbuf = buf;

	static int numWritten[IOSIM_MAX_FD]; // Initialized to 0

	if(nbyte == 0) return 0;
	if(fildes == 0) return -1;

	sym_file_stream_t* out = IOSIM_fd[fildes];
	cur = out->offset;
	len = out->sym_file->stat.st_size;

	// Enlarge file if necessary
	if (cur + nbyte > len) {
		out->sym_file->contents = realloc(out->sym_file->contents, cur + nbyte);
		out->sym_file->stat.st_size = cur + nbyte;
		len = cur + nbyte;
	}

	memcpy(out->sym_file->contents + cur, cbuf, nbyte);
//	for(n=0;n<nbyte;n++){
//		if(cur>=len) break; // I don't think this ever happens
//		out->sym_file->contents[cur] = cbuf[n];
//		cur++;
//	}

	numWritten[fildes] += nbyte;
	__EVALSTR(out->sym_file->contents,numWritten[fildes]);

	// Update offset pointer
	out->offset += nbyte;

	return nbyte;
}
		
int chdir(const char *path) {
	return 0;
}

int chroot(const char *path) {
	return 0;
}

DIR *opendir(const char *dirname) {
	return 0;
}
