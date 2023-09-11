// Taken (and slightly modified) from uClibc
#include <string.h>

char *strrchr(const char *s, int c)
{
	const char *p = NULL;

	do {
		if (*s == (char) c) {
			p = s;
		}
	} while (*s++);

	return (char *) p;
}
