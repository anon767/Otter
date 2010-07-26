#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <ctype.h>

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
	return __SYMBOLIC(4)%RAND_MAX;
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

void* __otter_libc_calloc(int amount, int bytes)
{
	char *ptr = malloc(bytes * amount);
	for(int i = 0; i < bytes * amount; i++)
	{
		ptr[i] = 0;
	}

	return (void*)ptr;
}

void* __otter_libc_realloc(void* ptr, int bytes)
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
	__libc_get_block_size(ptr2);
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
