#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

#define MB_CUR_MAX 2147483647

typedef struct
{
	int quot;
	int rem;
} div_t;

typedef struct
{
	long quot;
	long rem;
} ldiv_t;

typedef struct
{
	long long quot;
	long long rem;
} lldiv_t;

/* Numeric conversion functions (7.20.1) */
/*double atof(const char* nptr); double not supported*/
int atoi(const char* nptr);
long atol(const char* nptr);
long long atoll(const char* nptr);
/*double strtod(const char* nptr, char** endptr) double not supported*/
/*float strtof(const char* nptr, char** endptr) double not supported*/
/*long double strtold(const char* nptr, char** endptr) double not supported*/
long strtol(const char* nptr, char** endptr, int base);
long long strtoll(const char* nptr, char** endptr, int base);
unsigned long strtoul(const char* nptr, char** endptr, int base);
unsigned long long strtoull(const char* nptr, char** endptr, int base);


/* Random number generator (7.20.2) */
#define RAND_MAX 32767
int rand();
int srand();

/* Memory managment (7.20.3) */
void* malloc(int bytes);
void* calloc(int amount, int bytes);
void* realloc(void* ptr, int bytes);
void free(void* ptr);
int __libc_get_block_size(void* ptr);

/* System environment (7.20.4) */
#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0
void abort();
int atexit(void (*func)());
void exit(int status);
void _Exit(int status);
char* getenv(const char* name);
int system(const char* command);


#endif
