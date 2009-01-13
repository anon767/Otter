
/* Mock functions are defined in this file, and included with all source files via gcc -include (or cilly -include)
 * 
 * For each function to mock, #define the mock object and declare its prototype:
 * #define fn(i) mock_fn(i)
 * int mock_fn(int);
 * 
 * or alternatively, the following handles any number of arguments:
 * #define fn(...) mock_fn(__VA_ARGS__) 
 * int mock_fn(int);
 */
#ifndef SYMTEST_MOCK_H
#define SYMTEST_MOCK_H

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <stdio.h>

/* file I/O */
char* mock_stat_file;
struct stat mock_stat_buf;
#define stat(...) mock_stat(__VA_ARGS__)
int mock_stat(const char *path, struct stat *buf);
//extern int (*stat)(const char *restrict path, struct stat *restrict buf);

FILE *mock_fopen_file;
#define fopen(...) mock_fopen(__VA_ARGS__)
FILE *mock_fopen(const char *filename, const char *mode);
//extern FILE *(*fopen)(const char *filename, const char *mode);

#define fclose(...) mock_fclose(__VA_ARGS__)
int mock_fclose(FILE *stream);
//extern int (*fclose)(FILE *stream);

char *mock_fgets_buf;
int mock_fgets_len;
char *mock_fgets_ptr;
#define fgets(...) mock_fgets(__VA_ARGS__)
char *mock_fgets(char *s, int n, FILE *stream);
//extern char *(*fgets)(char *s, int n, FILE *stream);

int mock_feof_EOF;
#define feof(...) mock_feof(__VA_ARGS__)
int mock_feof(FILE *stream);
//extern int (*feof)(FILE *stream);

/* socket I/O */
char *mock_recv_buf;
size_t mock_recv_len;
char *mock_recv_ptr;
#define recv(...) mock_recv(__VA_ARGS__)
ssize_t mock_recv(int socket, void *buffer, size_t length, int flags);
//extern ssize_t (*recv)(int socket, void *buffer, size_t length, int flags);

char mock_send_buf[10240];
char *mock_send_ptr;
#define send(...) mock_send(__VA_ARGS__)
ssize_t mock_send(int socket, const void *buffer, size_t length, int flags);
//extern ssize_t (*send)(int socket, const void *buffer, size_t length, int flags);


#define main(...) symtest_actual_main(__VA_ARGS__)

#endif
