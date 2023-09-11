#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string.h>

int time_epoch = 0;
time_t time(time_t *__timer ) {
	return time_epoch++;
}

//void tzset() {
//}

struct tm* localtime(const time_t *timer) {
	struct tm* x = malloc(sizeof(struct tm));
	x->tm_sec   = *timer;
	x->tm_min   = 0;
	x->tm_hour  = 0;
	x->tm_mday  = 0;
	x->tm_mon   = 0;
	x->tm_year  = 0;
	x->tm_wday  = 0;
	x->tm_yday  = 0;
	x->tm_isdst = 0;
	x->tm_gmtoff= 0;
	x->tm_zone  = "my tz";
	return x;
}

//struct tm *gmtime(time_t const   *__timer ) {
//	return localtime(__timer);
//}
//
size_t strftime(char *restrict s, size_t maxsize,
								const char *restrict format, const struct tm *restrict timeptr) {
	if (maxsize >= 6) {
		strcpy(s, "+0123");
		return 5;
	}
	return 0;
}
//
//int gettimeofday(struct timeval *tv, struct timezone *tz ) {
//	tv->tv_sec = 123;
//	tv->tv_usec = 456;
//	return 0;
//}
//
//int nanosleep(struct timespec  const  *__requested_time , struct timespec *__remaining ) {
//	__COMMENT("Sleeping");
//	return 0;
//}
