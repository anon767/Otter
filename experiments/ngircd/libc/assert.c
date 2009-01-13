#include <stdlib.h>

int assert(int expression){
	if(expression)
		return 1;
	else
		exit(1);
}
