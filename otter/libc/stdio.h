#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h>
#include <unistd.h>
#include <fcntl.h>

typedef struct
{
	size_t pos;	
} fpos_t;

struct FILE
{
	int desc;
	int eof;
	int error;
	int offset;
	int bufmode;
	char* buf;
};

#define _IOFBF 1 /* block buffered */
#define _IOLBF 2 /* line buffered */
#define _IONBF 3 /* no buffer */

#define EOF (-1)
#define FOPEN_MAX __otter_fs_MAXOPEN
#define FILENAME_MAX 128
#define L_tmpnam 32
#define TMP_MAX 32767
#define BUF_SIZ 256

#define SEEK_CUR 1
#define SEEK_END 2
#define SEEK_SET 3

#define STDIN_FILENO 0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

struct FILE* stdout;
struct FILE* stdin;
struct FILE* stderr;

int remove(const char* filename);

#endif
