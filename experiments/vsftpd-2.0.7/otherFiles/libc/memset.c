/*
 * memset.c
 */

#include <string.h>
//#include <stdint.h>

void *memset(void *dst, int c, size_t n)
{
//#ifdef __STUB_SIMPLIFY__
//	return dst;
//#else
	char *q = dst;

	while (n--) {
		*q++ = c;
	}
	return dst;
//#endif
}
