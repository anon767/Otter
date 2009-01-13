#include <stdlib.h>

void *realloc(void *ptr, size_t size){
	if(ptr==0)
		return malloc(size);
	else
		// currently memory block has unbounded length
		return ptr;
}
