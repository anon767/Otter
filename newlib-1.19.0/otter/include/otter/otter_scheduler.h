#ifndef __OTTER_SCHEDULER_H
#define __OTTER_SCHEDULER_H

#include <stddef.h>

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
void __otter_multi_begin_atomic(void);
void __otter_multi_end_atomic(void);
void __otter_multi_time_wait(unsigned int ticks);
void __otter_multi_io_block(void*, ...);
void __otter_multi_io_block_array(void *ptrarray[], size_t len);

#endif
