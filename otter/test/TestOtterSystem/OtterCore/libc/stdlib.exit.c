#pragma has_failing_assertions
#pragma expect_abandoned("Assertion was false", i == 3)
#pragma no_other_results

#include <stdlib.h>

int i = 0;

void foo()
{
	i++;
	__ASSERT(i != 3); /* This should fail */
}

int main()
{
	atexit(foo);
	atexit(foo);
	atexit(foo);

	exit(0);

	return(0);
}
