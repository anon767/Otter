#ifndef _STRING_H
#define _STRING_H

#include <stddef.h>

/* Copying (7.21.2) */
void* memcpy(void* s1, const void* s2, int n);
void* memmove(void* s1, const void* s2, int n);
char* strcpy(char* s1, const char* s2);
char* strncpy(char* s1, const char* s2, int n);

/* Concatination (7.21.3) */
char* strcat(char* s1, const char* s2);
char* strncat(char* s1, const char* s2, int n);

/* Comparison (7.21.4) */
int memcmp(const void* s1, const void* s2, int n);
int strcmp(const char* s1, const char* s2);
int strcoll(const char* s1, const char* s2);
int strxfrm(char* s1, const char* s2, int n);

/* Search (7.21.5) */
void* memchr(const void* s, int c, int n);
char* strchr(const char* s, int c);
int strcspn(const char* s1, const char* s2);
char* strpbrk(const char* s1, const char* s2);
char* strrchr(const char* s, int c);
int strspn(const char* s1, const char* s2);
char* strstr(const char* s1, const char* s2);
char* strtok(char* s1, const char* s2);

/* Misc (7.21.6) */
void* memset(void* s, int c, int n);
char* strerror(int errnum);
int strlen(const char* s);


#endif
