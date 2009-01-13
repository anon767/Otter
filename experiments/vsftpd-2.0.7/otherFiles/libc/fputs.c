/*
 * fputs.c
 *
 * This isn't quite fputs() in the stdio sense, since we don't
 * have stdio, but it takes a file descriptor argument instead
 * of the FILE *.
 */

#include <stdio.h>
#include <string.h>
#include "fwrite.c"

int fputs(const char *s, FILE *file)
{
	return _fwrite(s, strlen(s), file);
}
