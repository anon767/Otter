#ifndef __IOSIM_H__
#define __IOSIM_H__

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

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
  char* buffer;
} sym_file_stream_t;

#define	IOSIM_MAX_FD	1024
extern sym_file_stream_t* IOSIM_fd[IOSIM_MAX_FD];
extern int		IOSIM_num_fd;

sym_file_t* IOSIM_newbuf(int len, char* buf);
sym_file_t* IOSIM_findfile(const char *file);
sym_file_t* IOSIM_addfile(const char *file, /*const char *contents,*/ mode_t mode);

char *IOSIM_toAbsolute(const char *name);

int IOSIM_newfd();
int IOSIM_read(int fildes, void *buf, int nbyte);
int IOSIM_ungetc(int c, int fildes);
int IOSIM_write(int fildes, const void *buf, int nbyte);
int IOSIM_close(int fildes);
int IOSIM_eof(int fildes);
int IOSIM_openWithMode(const char *pathname, int flags, mode_t mode);
int IOSIM_open(const char *pathname, int flags);
int IOSIM_rename(const char *old, const char *new);
int IOSIM_unlink(const char *pathname);

char *IOSIM_getcwd(char *buf, size_t size);
int IOSIM_chdir(const char *path);
DIR *IOSIM_opendir(const char *dirname);
int IOSIM_closedir(DIR *dir);
struct dirent *IOSIM_readdir(DIR *dir);
int IOSIM_dirfd(DIR *dir);
#endif
