/* As of r12213, io_block wipes out atomic entirely, no matter how many levels
   deep. Better would be to have the blocked process resume at the same atomic
   depth as when it blocked. */
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
		__otter_multi_begin_atomic();
		while (*p == 0) {
			__otter_multi_io_block(p);
		}
    __otter_multi_end_atomic();
		(*p)++; /* Step 3 */
	}
	else
	{
		__otter_multi_begin_atomic();
		__ASSERT(*p == 0); /* Step 1 */
		(*p)++; /* Step 2 */
		__otter_multi_io_block(p); // This should end the atomic section
		__ASSERT(*p == 2); /* Step 4 */
	}
	
	return(0);
}
