#include<time.h>

int current_time = 0;
time_t time(time_t* timer){
	// Funny: the server has some ways to force clients not to send request too rapid
	current_time += 1 ; 

	if(timer!=0) *timer = current_time;
	return (time_t)current_time;
}

struct tm localtime_stub;
struct tm* localtime(const time_t* timer){
	return 0;
	//return &localtime_stub;
}

size_t strftime(char *s, size_t maxsize, const char *format,
		    const struct tm *timptr){
	return maxsize;
}
