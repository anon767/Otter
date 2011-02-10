#include <otter/otter_scheduler.h>
#include <unistd.h>

/* nanosleep also uses __otter_multi_time_wait, so for consistency, its argument
	 is in nanoseconds. To prevent overflow, we don't multiply by a billion, but
	 instead wait one second each time through a loop. If someone actually uses
	 this and gets bored waiting, define NANOS_PER_SECOND_FOR_SLEEP to a smaller
	 number. */

#ifndef NANOS_PER_SECOND_FOR_SLEEP
#define NANOS_PER_SECOND_FOR_SLEEP 1000000000
#endif

unsigned int sleep(unsigned int seconds) {
	while (seconds--) {
		__otter_multi_time_wait(NANOS_PER_SECOND_FOR_SLEEP);
	}
	return 0;
}
