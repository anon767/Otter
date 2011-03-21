/* Make sure io_block ends an atomic section */
#pragma expect_return()
#pragma expect_return()
#pragma no_other_abandoned

int main()
{
	int *p = __otter_multi_gmalloc(sizeof(int));
	*p = 0;
	
	if(__otter_multi_fork())
	{
		// Wait until the other process increments *p
		__otter_multi_begin_atomic();
		while (*p == 0) {
			__otter_multi_io_block(p);
			__otter_multi_begin_atomic();
		}
		__ASSERT(*p == 1); /* Step 3 */
		(*p)++; /* Step 4 */
	}
	else
	{
		__otter_multi_begin_atomic();
		__ASSERT(*p == 0); /* Step 1 */
		(*p)++; /* Step 2 */
		__otter_multi_io_block(p); // This should end the atomic section
		__ASSERT(*p == 2); /* Step 5 */
	}
	
	return(0);
}
