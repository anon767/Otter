#include "iosim.h"
#include <stdio.h>

int ungetc(int c, FILE *stream){
	return IOSIM_ungetc(c, stream);
}
