/*
 * strstr.c
 */

#include <string.h>
#include "memmem.c"

char *strstr(const char *haystack, const char *needle)
{
	return (char *)memmem(haystack, strlen(haystack), needle,
			      strlen(needle));
}
