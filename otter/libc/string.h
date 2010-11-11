#ifndef _STRING_H
#define _STRING_H

#include <stddef.h>

/* Copying (7.21.2) */
void* memcpy(void* s1, const void* s2, size_t n);
void* memmove(void* s1, const void* s2, size_t n);
char* strcpy(char* s1, const char* s2);
char* strncpy(char* s1, const char* s2, size_t n);

/* Concatination (7.21.3) */
char* strcat(char* s1, const char* s2);
char* strncat(char* s1, const char* s2, size_t n);

/* Comparison (7.21.4) */
int memcmp(const void* s1, const void* s2, size_t n);
int strcmp(const char* s1, const char* s2);
int strcoll(const char* s1, const char* s2);
int strxfrm(char* s1, const char* s2, int n);

/* Search (7.21.5) */
void* memchr(const void* s, int c, size_t n);
char* strchr(const char* s, int c);
size_t strcspn(const char* s1, const char* s2);
char* strpbrk(const char* s1, const char* s2);
char* strrchr(const char* s, int c);
size_t strspn(const char* s1, const char* s2);
char* strstr(const char* s1, const char* s2);
char* strtok(char* s1, const char* s2);

/* Misc (7.21.6) */
void* memset(void* s, int c, size_t n);
char* strerror(int errnum);
size_t strlen(const char* s);

/* Not in the Standard, but they're around. */
char *strdup(const char *);
char *stpcpy(char * dst, const char * src);

#endif
