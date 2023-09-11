#pragma no_other_abandoned

#include <stdbool.h>

int main()
{
	bool b = true;

	__ASSERT(!b == false);

	return(0);
}
