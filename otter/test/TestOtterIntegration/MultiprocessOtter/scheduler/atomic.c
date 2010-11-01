#pragma expect_return()
#pragma expect_return()
#pragma no_other_abandoned

int main()
{
	int *p = __otter_multi_gmalloc(sizeof(int));
	*p = 0;
	
	__otter_multi_begin_atomic();
	if(__otter_multi_fork())
	{
		for(int i = 0; i < 10; i++)
			(*p)++;

		__otter_multi_end_atomic();
	}
	else
	{
		__otter_multi_end_atomic();
		
		(*p) = *p * 10;
		__ASSERT(*p == 100);
	}
	
	return(0);
}
