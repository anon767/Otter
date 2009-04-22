#include "iosim.h"
#include <stdio.h>

int ungetc(int c, FILE *stream){
	sym_file_stream_t *streamAsSymStream = stream;
	return IOSIM_ungetc(c, streamAsSymStream->fd);
}
