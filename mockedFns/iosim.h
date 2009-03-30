#ifndef __IOSIM_H__
#define __IOSIM_H__

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>

// This symbolic file system borrows a bit from KLEE's

typedef struct {
  char* contents;
  struct stat stat;
} sym_file_t;

typedef struct {
	//	FILE *stream; // Pointer to the FILE object
	int fd; // The fd for this stream
	//	unsigned flags; // I'm not sure what these are
	off_t offset; // The offset into the file
  sym_file_t* sym_file;   /* ptr to the file on disk */
} sym_file_stream_t;

#define	IOSIM_MAX_FD	1024
extern sym_file_stream_t* IOSIM_fd[IOSIM_MAX_FD];
extern int		IOSIM_num_fd;

sym_file_t* IOSIM_newbuf(int len, char* buf);
sym_file_t* IOSIM_findfile(const char *file);
sym_file_t* IOSIM_addfile(const char *file, mode_t);

int IOSIM_newfd();
//int IOSIM_attach(int fildes, sym_file_stream_t* s);
int IOSIM_read(int fildes, void *buf, int nbyte);
int IOSIM_write(int fildes, const void *buf, int nbyte);
int IOSIM_close(int fildes);

#endif
