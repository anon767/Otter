#ifndef __OTTER_SCHEDULER_H
#define __OTTER_SCHEDULER_H

/* expr should evaluate to true as long as the process should block.
 * ptrs should be a list of pointers that may be relavent to the computation of expr.
 */
#define __otter_multi_block_while_condition(expr, ptrs...) \
({ \
	__otter_multi_begin_atomic(); \
	while(expr) \
	{ \
		__otter_multi_io_block(ptrs); \
		__otter_multi_begin_atomic(); \
	} \
	__otter_multi_end_atomic(); \
})

/* prototypes for scheduler functions */
void __otter_multi_begin_atomic();
void __otter_multi_end_atomic();
void __otter_multi_time_wait(int ticks);
void __otter_multi_io_block(void*, ...);

#endif
