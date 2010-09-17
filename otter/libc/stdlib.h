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
#define RAND_MAX 0xffff
int rand();
int srand();

/* Memory managment (7.20.3) */
void* malloc(int bytes);
void* calloc(size_t amount, size_t bytes);
void* realloc(void* ptr, size_t bytes);
void free(void* ptr);
size_t __libc_get_block_size(void* ptr);

/* System environment (7.20.4) */
#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0
void abort();
int atexit(void (*func)());
void exit(int status);
void _Exit(int status);
char* getenv(const char* name);
int system(const char* command);

/* Search and Sort (7.20.5) */
void* bsearch(const void* key, const void* base, size_t nmemb, size_t size, int (*compar)(const void*, const void*));
void qsort(void* base, size_t nmemb, size_t size, int (*compar)(const void*, const void*));

/* Integer arithmatic (7.20.6) */
int abs(int i);
long labs(long i);
long long llabs(long long i);
/* compute p/q and p%q */
div_t div(int p, int q);
ldiv_t ldiv(long p, long q);
lldiv_t lldiv(long long p, long long q);

/* Multibyte <-> wide char conversion functions (7.20.7, 7.20.8) */
/* No multibyte character codes are defined so these are fancy type casts*/
int mblen(const char* s, size_t n);
int mbtowc(wchar_t* pwc, const char* s, size_t n);
int wctomb(char* s, wchar_t wc);
size_t mbstowcs(wchar_t* pwcs, const char* s, size_t n);
size_t wcstombs(char* s, const wchar_t* pwcs, size_t n);

#endif
