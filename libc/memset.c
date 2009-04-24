/*
 * memset.c
 */

#include <string.h>

void *memset(void *dst, int c, size_t n)
{
	char *q = dst;

#ifdef __FAVORITE_SE__
	// this is better to the SE since if n is symbolic (over a "small" set of integers),
	// keep doing n-- will blow up the symbolic expression
	int i;
	for(i=0;i<n;i++){
		q[i] = c;
	}
#else
	while (n--) {
		*q++ = c;
	}
#endif
	return dst;
}
