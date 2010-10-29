#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <ctype.h>
#include <unistd.h>

int __otter_libc_atoi(const char* nptr)
{
	return (int)(strtoll(nptr, 0, 10));
}

long __otter_libc_atol(const char* nptr)
{
	return (long)strtoll(nptr, 0, 10);
}

long long __otter_libc_atoll(const char* nptr)
{
	return strtoll(nptr, 0, 10);
}

long __otter_libc_strtol(const char* nptr, char** endptr, int base)
{
	return (long)strtoll(nptr, endptr, base);
}

long long __otter_libc_strtoll(const char* nptr, char** endptr, int base)
{
	/* remove optionl leading white space*/
	while(isspace(*nptr) && *nptr != 0)
	{
		nptr++;
	}

	if(*nptr == 0) 	/* ran out of string */
		return(0);
	else if (*nptr == '-') /* number is negative */
	{
		nptr++;
		return -1*(long long)(strtoull(nptr, endptr, base));
	}
	else /* number is positive */
	{
		return (long long)(strtoull(nptr, endptr, base));
	}
}

unsigned long __otter_libc_strtoul(const char* nptr, char** endptr, int base)
{
	return (unsigned long)strtoull(nptr, endptr, base);
}

int __otter_libc_ctoi(int ch)
{
	if (ch >= '0' && ch <= '9') {
		return ch - '0';
	} else if (ch >= 'A' && ch <= 'Z') {
		return ch - 'A' + 10;
	} else if (ch >= 'a' && ch <= 'z') {
		return ch - 'a' + 10;
	} else {
		return -1;
	}
}

unsigned long long __otter_libc_strtoull(const char* nptr, char** endptr, int base)
{
	if(*nptr == 0) /* out of string */
		return(0);

	if (base == 0) /* determine base from numeric leader */
	{
		if(*nptr == 0) /* Octal or Hex */
		{
			nptr++;
			if(*nptr == 0) /* out of string */
				return(0);
			else if (*nptr == 'x' || *nptr == 'X') /* Hex */
			{
				nptr++; /* skip the hex leader */
				base = 16;
			}
			else /* Octal */
				base = 8;
		}
		else
			base = 10;
	}
	else if (base == 16) /* detect optional 0x */
	{
		if(*nptr == '0') /* it's ok to drop a leading 0 */
		{
			nptr++;
			if(*nptr == 0) /* out of string */
				return(0);
			else if (*nptr == 'x' || *nptr == 'X')
				nptr++; /* skip the hex leader */
		}
	}
	else if (base < 2 || base > 36) /* Error */
		return (0);

	/* nptr should be pointing to character after any headers */
	int d = __otter_libc_ctoi(*nptr);
	unsigned long long r = 0LL;
	while(d != -1 && d < base)
	{
		r = (r * base) + d;
		nptr++;
		d = __otter_libc_ctoi(*nptr);
	}

	if(endptr)
		*endptr = nptr;

	return r;
}

/* In most cases it is best to generate a symbolic value for rand().
 * If it is necessary to reproduce a particular sequence, then the
 * actual generator can be used.
 */

int __otter_libc_rand()
{
	unsigned short us;
	__SYMBOLIC(&us);
	return us;
}

void __otter_libc_srand(int seed)
{
	return;
}

/* generator copied from the ISO spec */
unsigned long int __libc_rand_next = 1;

int __libc_rand()
{
	__libc_rand_next = __libc_rand_next * 1103515245 + 12345;
	return (unsigned int)(__libc_rand_next/65536) % 32768;
}

void __libc_srand(unsigned int seed)
{
	__libc_rand_next = seed;
}

void* __otter_libc_calloc(size_t amount, size_t bytes)
{
	char *ptr = malloc(bytes * amount);
	for(int i = 0; i < bytes * amount; i++)
	{
		ptr[i] = 0;
	}

	return (void*)ptr;
}

void* __otter_libc_realloc(void* ptr, size_t bytes)
{
	if (ptr == 0)
		return malloc(bytes);
	if (bytes == 0)
	{
		free(ptr);
		return 0;
	}

	int old_size = __libc_get_block_size(ptr);
	void* ptr2 = malloc(bytes);
	__libc_get_block_size(ptr2);
	int max = (bytes > old_size) ? old_size : bytes;
	for(int i = 0; i < max; i++)
	{
		ptr2[i] = ptr[i];
	}
	free(ptr);

	return ptr2;
	
}

void* __otter_multi_gcalloc(size_t amount, size_t bytes)
{
	char *ptr = __otter_multi_gmalloc(bytes * amount);
	for(int i = 0; i < bytes * amount; i++)
	{
		ptr[i] = 0;
	}

	return (void*)ptr;
}

void* __otter_multi_grealloc(void* ptr, size_t bytes)
{
	if (ptr == 0)
		return __otter_multi_gmalloc(bytes);
	if (bytes == 0)
	{
		free(ptr);
		return 0;
	}

	int old_size = __libc_get_block_size(ptr);
	void* ptr2 = __otter_multi_gmalloc(bytes);
	__libc_get_block_size(ptr2);
	int max = (bytes > old_size) ? old_size : bytes;
	for(int i = 0; i < max; i++)
	{
		ptr2[i] = ptr[i];
	}
	__otter_multi_gfree(ptr);

	return ptr2;
	
}


void __otter_libc_abort()
{
	_Exit(EXIT_FAILURE);
}

struct __otter_libc_exit_func_node {
	struct __otter_libc_exit_func_node* next;
	void (*func)();
};

struct __otter_libc_exit_func_node* __otter_libc_exit_first = 0;

int __otter_libc_atexit(void (*func)())
{
	if(!__otter_libc_exit_first)
	{
		__otter_libc_exit_first = malloc(sizeof(struct __otter_libc_exit_func_node));
		if(!__otter_libc_exit_first)
			return 0;
		(*__otter_libc_exit_first).next = 0;
		(*__otter_libc_exit_first).func = func;
		return 1;
	}

	struct __otter_libc_exit_func_node* cur = __otter_libc_exit_first;
	while((*cur).next)
	{
		cur = (*cur).next;
	}

	(*cur).next = malloc(sizeof(struct __otter_libc_exit_func_node));
	if(!((*cur).next))
		return 0;
	cur = (*cur).next;
	(*cur).next = 0;
	(*cur).func = func;
	return 1;	
}

void __otter_libc_call_exit_funcs()
{
	struct __otter_libc_exit_func_node* cur = __otter_libc_exit_first;
	while(cur)
	{
		(*cur).func();
		cur = (*cur).next;
	}
}

void __otter_libc_exit(int status)
{
	__otter_libc_call_exit_funcs();

	_Exit(status);
}

void __otter_libc__Exit(int status)
{
	if(status == EXIT_FAILURE)
	{
		/* cause abnormal exit */
		__ASSERT(status == EXIT_SUCCESS);
	}

	/* cause nmormal exit */
	_exit(status);
}

char* __otter_libc_getenv(const char* name)
{
	return 0; /* no environment variables; always fails to find them */
}

/* calling the shell is not supported */
int __otter_libc_system(const char* command)
{
	if(command) /* any call fails */
		return EXIT_FAILURE;
	else /* report that there is no shell*/
		return 0;
}

int __otter_libc_abs(int i)
{
	__ASSERT(i != INT_MIN); /* behavior is undefined if the result can't be stored */
	return (i < 0 ? -i : i);
}

long __otter_libc_labs(long i)
{
	__ASSERT(i != LONG_MIN); /* behavior is undefined if the result can't be stored */
	return (i < 0 ? -i : i);
}

long long __otter_libc_llabs(long long i)
{
	__ASSERT(i != LLONG_MIN); /* behavior is undefined if the result can't be stored */
	return (i < 0 ? -i : i);
}

div_t __otter_libc_div(int p, int q)
{
	__ASSERT(q != 0); /* behavior is undefined if the result can't be stored */
	div_t r;
	r.quot = p/q;
	r.rem = p%q;
	return r;
}

ldiv_t __otter_libc_ldiv(long p, long q)
{
	__ASSERT(q != 0); /* behavior is undefined if the result can't be stored */
	ldiv_t r;
	r.quot = p/q;
	r.rem = p%q;
	return r;
}

lldiv_t __otter_libc_lldiv(long long p, long long q)
{
	__ASSERT(q != 0); /* behavior is undefined if the result can't be stored */
	lldiv_t r;
	r.quot = p/q;
	r.rem = p%q;
	return r;
}

/*********************************************
 * bsearch and qsort are taken from uclibc with some modifications
 */

/* the compar(a,b) > 0 iff a > b, < 0 iff a < b, and == 0 iff a == b */
void* __otter_libc_bsearch(const void* key, const void* base, size_t nmemb, size_t size, int (*compar)(const void*, const void*))
{
	if(size <= 0) /* Standard says to return NULL if the objects have no size */
	{
		return 0;
	}

	char *p;
	int high = nmemb;
	int low = 0;
	int mid;
	int r;
	while (low < high)
	{
		mid = low + ((high - low) / 2);
		p = ((char*)(base)) + (mid * size);
		r = (*compar)(key, p);
		if (r > 0) /* search upper half */
		{
			low = mid + 1;
		}
		else if (r < 0) /* search lower half */
		{
			high = mid;
		}
		else /* found key */
		{
			return p;
		}
	}

	return (0); /* didn't find key */
}

void __otter_libc_qsort(void  *base,
           size_t nel,
           size_t width,
           int (*comp)(const void *, const void *))
{
	int wgap, i, j, k;
	char tmp;

	if ((nel > 1) && (width > 0)) {
		__ASSERT(nel <= ((size_t)(-1)) / width); /* check for overflow */
		wgap = 0;
		do {
			wgap = 3 * wgap + 1;
		} while (wgap < (nel-1)/3);
		/* From the above, we know that either wgap == 1 < nel or */
		/* ((wgap-1)/3 < (int) ((nel-1)/3) <= (nel-1)/3 ==> wgap <  nel. */
		wgap *= width;			/* So this can not overflow if wnel doesn't. */
		nel *= width;			/* Convert nel to 'wnel' */
		do {
			i = wgap;
			do {
				j = i;
				do {
					register char *a;
					register char *b;

					j -= wgap;
					a = j + ((char *)base);
					b = a + wgap;
					if ((*comp)(a, b) <= 0) {
						break;
					}
					k = width;
					do {
						tmp = *a;
						*a++ = *b;
						*b++ = tmp;
					} while (--k);
				} while (j >= wgap);
				i += width;
			} while (i < nel);
			wgap = (wgap - width)/3;
		} while (wgap);
	}
}

/********************************************/

int __otter_libc_mblen(const char* s, size_t n)
{
	if(!s || n <= 0) 
		return(0); /* encoding is not state dependent; need atleast one character to look at */

	return (*s != 0); /* 0 if end of string, 1 for size of the next multichar */
}

int __otter_libc_mbtowc(wchar_t* pwc, const char* s, size_t n)
{
	if(!s || n <= 0) 
		return(0); /* encoding is not state dependent; need atleast one character to look at */

	if(pwc)
		*pwc = (int)(*s);

	return (*s != 0);
}

int __otter_libc_wctomb(char* s, wchar_t wc)
{
	if(!s) 
		return(0); /* encoding is not state dependent */

	if(wc > UCHAR_MAX)
	{
		*s = (unsigned char)wc;
		return(-1); /* not big enough to store wide char */
	}
	else
	{
		*s = (unsigned char)wc;
		return(wc != 0); /* stored one byte */
	}
}

size_t __otter_libc_mbstowcs(wchar_t* pwcs, const char* s, size_t n)
{
	int r;
	for(int i = 0; i < n; i++)
	{
		r = mbtowc(pwcs, s, 1);
		if(r == -1)
			return (-1);
		else if (r == 0);
			return (i);
		
		pwcs += r;
		s += r;
	}

	return (n);
}

size_t __otter_libc_wcstombs(char* s, const wchar_t* pwcs, size_t n)
{
	int r;
	for(int i = 0; i < n; i++)
	{
		r = wctomb(s, *pwcs);
		if(r == -1)
			return (-1);
		else if (r == 0);
			return (i);
		
		pwcs += r;
		s += r;
	}

	return (n);
}
