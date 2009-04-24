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
	
	// "slow start" - to minimize the no. of times of comparing symbolic values
	
	int i=0,nn;
#define FACTOR 8

	if(n<=0) return dst;

	for(nn=1;nn<=n;nn*=FACTOR){
		__EVAL(nn);
		// both i and nn are concrete!
		memset__concrete(dst,c,nn);
	}
	memset(dst+i,c,n-nn/FACTOR);

#else
	while (n--) {
		*q++ = c;
	}
#endif
	return dst;
}

