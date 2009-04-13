#include <stdio.h>
#include "iosim.h"

int feof(FILE *stream)
{
	sym_file_stream_t *streamAsSymStream = stream;
	return IOSIM_eof(streamAsSymStream->fd);
}
