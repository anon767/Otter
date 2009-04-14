#ifndef __IOSIM_H__
#define __IOSIM_H__

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

// This symbolic file system borrows a bit from KLEE's

typedef struct {
  char* contents;
  struct stat stat;  // stat.st_size will be changed over time
  off_t* size; // an array of sizes over time
} sym_file_t;

#define IOSIM_FD_FILE  0
#define IOSIM_FD_SOCK  1
#define IOSIM_FD_SSOCK  2

typedef struct {
	int fd_type;
	//	FILE *stream; // Pointer to the FILE object
	int fd; // The fd for this stream
	//	unsigned flags; // I'm not sure what these are
	
	// if fd_type==SSOCK, sym_file->contents will be casted to (int*) (an array of fds) 
	off_t offset; // The offset into the file
	sym_file_t* sym_file;   /* ptr to the file on disk */
	off_t offsetout; // The offset into the fileout
	sym_file_t* sym_fileout;   /* ptr to the fileout */

	char* buffer;
} sym_file_stream_t;

#define	IOSIM_MAX_FD	1024
extern sym_file_stream_t* IOSIM_fd[IOSIM_MAX_FD];
extern int		IOSIM_num_fd;

sym_file_t* IOSIM_newbuf(int len, char* buf);
sym_file_t* IOSIM_findfile(const char *file);
sym_file_t* IOSIM_addfile(const char *file, mode_t);

int IOSIM_newfd();
int IOSIM_read(int fildes, void *buf, int nbyte);
int IOSIM_ungetc(int c, int fildes);
int IOSIM_write(int fildes, const void *buf, int nbyte);
int IOSIM_close(int fildes);
int IOSIM_eof(int fildes);
int IOSIM_openWithMode(const char *pathname, int flags, mode_t mode);
int IOSIM_open(const char *pathname, int flags);

void IOSIM_updatesize(int fildes, int t);

char *IOSIM_getcwd(char *buf, size_t size);
int IOSIM_chdir(const char *path);
DIR *IOSIM_opendir(const char *dirname);
int IOSIM_closedir(DIR *dir);
struct dirent *IOSIM_readdir(DIR *dir);
int IOSIM_dirfd(DIR *dir);
#endif
