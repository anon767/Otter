#include <assert.h>

int main()
{
	assert(0);
	__ASSERT(0); /* shouldn't reach this line */
	return(0);
}
