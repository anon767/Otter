/*
 * fopen.c
 */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

/* This depends on O_RDONLY == 0, O_WRONLY == 1, O_RDWR == 2 */

int fclose(FILE * f)
{
	return close(fileno(f));
}
