#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>

typedef struct
{
	size_t pos;	
} fpos_t;

typedef struct
{
	int desc;
	int eof;
	int error;
	int offset;
	int bufmode;
	char* buf;
} FILE;

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

/* copied function prototypes from http://opengroup.org/onlinepubs/007908775/xsh/stdio.h.html */
/* functions marked with EXISTS have a (potentially incomplete) implimentation */
void     clearerr(FILE *);
char    *ctermid(char *);
char    *cuserid(char *); /*(LEGACY)*/
int      fclose(FILE *);
FILE    *fdopen(int, const char *);
int      feof(FILE *);
int      ferror(FILE *);
int      fflush(FILE *);
int      fgetc(FILE *);
int      fgetpos(FILE *, fpos_t *);
char    *fgets(char *, int, FILE *);
int      fileno(FILE *);
void     flockfile(FILE *);
FILE    *fopen(const char *, const char *);
int      fprintf(FILE *, const char *, ...);
int      fputc(int, FILE *);
int      fputs(const char *, FILE *);
size_t   fread(void *, size_t, size_t, FILE *);
FILE    *freopen(const char *, const char *, FILE *);
int      fscanf(FILE *, const char *, ...);
int      fseek(FILE *, long int, int);
int      fseeko(FILE *, off_t, int);
int      fsetpos(FILE *, const fpos_t *);
long int ftell(FILE *);
off_t    ftello(FILE *);
int      ftrylockfile(FILE *);
void     funlockfile(FILE *);
size_t   fwrite(const void *, size_t, size_t, FILE *);
int      getc(FILE *);
int      getchar(void);
int      getc_unlocked(FILE *);
int      getchar_unlocked(void);
char    *gets(char *);
int      getw(FILE *);
int      pclose(FILE *);
void     perror(const char *);
FILE    *popen(const char *, const char *);
int      printf(const char *, ...);
int      putc(int, FILE *);
int      putchar(int);
int      putc_unlocked(int, FILE *);
int      putchar_unlocked(int);
int      puts(const char *);
int      putw(int, FILE *);
int      remove(const char *); /*EXISTS*/
int      rename(const char *, const char *);
void     rewind(FILE *);
int      scanf(const char *, ...);
void     setbuf(FILE *, char *);
int      setvbuf(FILE *, char *, int, size_t);
int      snprintf(char *, size_t, const char *, ...); /*EXISTS*/
int      sprintf(char *, const char *, ...); /*EXISTS*/
int      sscanf(const char *, const char *, ...);
char    *tempnam(const char *, const char *);
FILE    *tmpfile(void);
char    *tmpnam(char *);
int      ungetc(int, FILE *);
int      vfprintf(FILE *, const char *, va_list);
int      vprintf(const char *, va_list);
int      vsnprintf(char *, size_t, const char *, va_list); /*EXISTS*/
int      vsprintf(char *, const char *, va_list); /*EXISTS*/

#endif
