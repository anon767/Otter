#pragma expect_return()
#pragma expect_return()
#pragma no_other_abandoned

#include <__otter/otter_scheduler.h>

int main()
{
	int *p = __otter_multi_gmalloc(sizeof(int));
	*p = 0;
	
	if(fork())
	{
		*p = 1;
	}
	else
	{
		__otter_multi_block_while_condition(*p != 1, p);
	}
	
	return(0);
}
