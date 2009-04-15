#include <sys/socket.h>
#include <string.h>

int getpeername(int socket, struct sockaddr *restrict address,
								socklen_t *restrict address_len){
	address->sa_family = AF_INET;

	address->sa_data[0] = 0;//__SYMBOLIC(0);
	address->sa_data[1] = 0;//__SYMBOLIC(0);

	// Here's a fake IP address
	address->sa_data[2] = 10;
	address->sa_data[3] = 20;
	address->sa_data[4] = 30;
	address->sa_data[5] = 40;
	*address_len = 8;
	return 0;
}
