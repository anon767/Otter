#pragma expect_abandoned(assertion_failure, x == 1)
#pragma no_other_results

#include <assert.h>

int x;

int main()
{
	x = 1;
	assert(0);
	x = 2;
	__ASSERT(0); /* shouldn't reach this line */
	return(0);
}
