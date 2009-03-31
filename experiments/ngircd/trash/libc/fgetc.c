/*
 * fgetc.c
 *
 * same as getc
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

int fgetc(FILE *f)
{
	return getc(f);
}
