#ifndef __IOSIM_H__
#define __IOSIM_H__

struct __buf {
	int cur;
	int len;
	char* buf;
};
typedef	struct __buf	IO_BUF;


#define	IOSIM_MAX_FD	1024
extern IO_BUF*	IOSIM_fd[IOSIM_MAX_FD];
extern int		IOSIM_num_fd;

IO_BUF* IOSIM_newbuf(int len, char* buf);
IO_BUF* IOSIM_findfile(const char *file);
void IOSIM_addfile(const char *file,IO_BUF*);

int IOSIM_newfd();
int IOSIM_attach(int fildes, IO_BUF* s);
int IOSIM_read(int fildes, void *buf, int nbyte);
int IOSIM_write(int fildes, const void *buf, int nbyte);
//int IOSIM_close(int fildes);


#endif
