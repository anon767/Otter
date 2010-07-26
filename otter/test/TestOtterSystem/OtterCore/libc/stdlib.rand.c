#include <stdlib.h>

int main()
{
	srand(3);
	int i = rand();

	__ASSERT(i >= 0);
	__ASSERT(i < RAND_MAX);
	
	return(0);
}
