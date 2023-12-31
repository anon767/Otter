#pragma no_other_abandoned

#include <stdlib.h>

int main()
{
	char* p = calloc(1, 4);
	__ASSERT(__otter_get_allocated_size(p) == 4);
	p[0] = 70;
	p = realloc(p, 2);
	__ASSERT(__otter_get_allocated_size(p) == 2);
	__ASSERT(p[1] == 0);
	__ASSERT(p[0] == 70);
	p = realloc(p, 8);
	__ASSERT(__otter_get_allocated_size(p) == 8);
	free(p);

	return (0);
}
