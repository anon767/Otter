#include <pthread.h>

#include <otter/otter_builtins.h>

// Make sure we use this only with 0, which pthread_self returns.
int pthread_equal(pthread_t t1, pthread_t t2) {
	__ASSERT(t1 == 0 && t2 == 0);
	return 1;
}
