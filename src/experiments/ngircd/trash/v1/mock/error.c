#include <errno.h>
#include <stdlib.h>
#include <string.h>

int moc__error_ret = 1;
int *__error(void){
	return &moc__error_ret;
}

char *strerror(int errnum){
	return "(mock)error";
}


