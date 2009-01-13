#include <iosim.h>
#include <stdio.h>

int getc(FILE *stream){
	char b[1];
	int flag = IOSIM_read(stream,b,1); //TODO: it's slow
	if(flag<0) return EOF;
	else return b[0];
}
