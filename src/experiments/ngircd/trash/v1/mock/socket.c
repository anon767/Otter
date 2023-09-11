#include <sys/socket.h>
#include <string.h>
#include "iosim.h"


#if __HAVE_socket__
int socket(int domain, int type, int protocol){
	return IOSIM_newfd();
}
#endif

#if __HAVE_setsockopt__
int setsockopt(int socket, int level, int option_name, const void
		                *option_value, socklen_t option_len){
	// NOTHING?
	return 0;
}
#endif

#if __HAVE_bind__
int bind(int socket, const struct sockaddr *address,
		     socklen_t address_len){
	return 0;
}
#endif

#if __HAVE_listen__
int listen(int socket, int backlog){
	return 0;
}
#endif

