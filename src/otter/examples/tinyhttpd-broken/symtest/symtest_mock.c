#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include "symtest_mock.h"

/* mock stubs storage */
//int (*mock_stat)(const char *restrict path, struct mock_stat *restrict buf);
//FILE *(*mock_fopen)(const char *filename, const char *mode);
//int (*mock_fclose)(FILE *stream);
//char *(*mock_fgets)(char *s, int n, FILE *stream);
//int (*feof)(FILE *stream);
//ssize_t (*mock_recv)(int socket, void *buffer, size_t length, int flags);
//ssize_t (*mock_send)(int socket, const void *buffer, size_t length, int flags);



/* mock functions */
char *mock_recv_buf;
size_t mock_recv_len;
char *mock_recv_ptr;
ssize_t mock_recv(int socket, void *buffer, size_t length, int flags) {
    ssize_t i;
    for (i = 0; i < length && mock_recv_ptr < mock_recv_buf + mock_recv_len; i++) {
        ((char*)buffer)[i] = *mock_recv_ptr++;
    }
    return i;
}

char* mock_stat_file;
struct stat mock_stat_buf;
int mock_stat(const char *path, struct stat *buf) {
	if(strcmp(path,mock_stat_file)==0){
    	*buf = mock_stat_buf;
		return 0;
	}
	else
		return -1;
}

FILE *mock_fopen_file;
FILE *mock_fopen(const char *filename, const char *mode) {
    return mock_fopen_file;
}

char *mock_fgets_buf;
int mock_fgets_len;
char *mock_fgets_ptr;
int mock_feof_EOF;
char *mock_fgets(char *s, int n, FILE *stream) {
    ssize_t i;
    for (i = 0; i < n - 1 && mock_fgets_ptr < mock_fgets_buf + mock_fgets_len; i++) {
        s[i] = *mock_fgets_ptr++;
        if (s[i] == '\n') break;
    }
    if (i) {
        s[i] = '\0';
        return s;
    } else {
        mock_feof_EOF = 1;
        return NULL;
    }
}

int feof(FILE *stream) {
    return mock_feof_EOF;
}

int mock_fclose(FILE *stream) {
    return 0;
}

char mock_send_buf[10240];
char *mock_send_ptr;
ssize_t mock_send(int socket, const void *buffer, size_t length, int flags) {
    ssize_t i;
    for (i = 0; i < length && mock_send_ptr < mock_send_buf + sizeof(mock_send_buf); i++) {
        *mock_send_ptr++ = ((char*)buffer)[i];
    }
    return i;
}

