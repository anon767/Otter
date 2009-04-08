#include <stdio.h>

int feof(FILE *stream)
{
	return IOSIM_eof(stream);
}
