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
		__otter_multi_end_atomic();
		*p = 1;
	}
	else
	{
		__otter_multi_io_block(p);
	}
	
	return(0);
}
