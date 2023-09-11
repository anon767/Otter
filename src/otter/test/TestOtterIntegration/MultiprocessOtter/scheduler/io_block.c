#pragma expect_return()
#pragma expect_return()
#pragma no_other_abandoned

int main()
{
	int *p = __otter_multi_gmalloc(sizeof(int));
	*p = 0;
	
	if(__otter_multi_fork())
	{
		for(int i = 0; i < 5; i++); /* delay a moment so that the block occurs when *p is 0 */
		*p = 1;
	}
	else
	{
		__otter_multi_io_block(p);
	}
	
	return(0);
}
