#include <unistd.h>
#include <time.h>

unsigned int sleep(unsigned int seconds) {
	struct timespec t = {.tv_sec = seconds, .tv_nsec = 0};
	nanosleep(&t, NULL);
	return 0;
}
