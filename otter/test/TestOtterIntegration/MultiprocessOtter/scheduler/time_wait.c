#pragma expect_return()
#pragma expect_return()
#pragma no_other_abandoned

int main()
{
	int *p = __otter_multi_gmalloc(sizeof(int));
	*p = 0;
	
	if(__otter_multi_fork())
	{
		for(int i = 0; i < 10; i++)
			(*p)++;

	}
	else
	{
		__otter_multi_time_wait(100);
		
		(*p) = *p * 10;
		__ASSERT(*p == 100);
	}
	
	return(0);
}
