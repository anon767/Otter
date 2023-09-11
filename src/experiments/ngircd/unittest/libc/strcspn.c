/*
 * strcspn
 *
 * A not-so-efficient but simple implementation, by Martin.
 */

#include<string.h>

size_t strcspn(const char *s, const char *reject)
{
	char* r;
	int count = 0;
	for(;*s!=0;s++,count++)
		for(r=reject;*r!=0;r++)
			if(*s==*r) goto FAIL;
FAIL:	
	return count;
}

