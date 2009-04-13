#include "iosim.h"
#include <stdio.h>

int getc(FILE *stream){
	char b[1];
	sym_file_stream_t *streamAsSymStream = stream;
	int flag = IOSIM_read(streamAsSymStream->fd,b,1); //TODO: it's slow
	if(flag<0) return EOF;
	else return b[0];
}
