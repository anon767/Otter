#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include "strntoumax.c"

long atol(const char *nptr)
{
	return (long) strntoumax(nptr, (char **)0, 10, ~(size_t) 0);
}
