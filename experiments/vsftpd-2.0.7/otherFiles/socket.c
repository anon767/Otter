#include <sys/socket.h>
#include "iosim.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int setsockopt(int socket, int level, int option_name,
							 const void *option_value, socklen_t option_len) {
	return 0;
}

int socket(int domain, int type, int protocol) {
#define DEFAULT_BUF_SIZE 1024
	IO_BUF* new_buffer = IOSIM_newbuf(DEFAULT_BUF_SIZE,malloc(DEFAULT_BUF_SIZE));
	int fd = IOSIM_newfd();
	char *filename = malloc(18); // 18 == strlen("socket_at_fd_9999*"). This should allow fds up to 9999
	if (sprintf(filename,"socket_at_fd_%d",fd) < 0) {
		__EVALSTR("Problem creating socket",23);
		exit(); // This should cause the symbolic executor to halt with 'Failure: hd'
	}
	IOSIM_addfile(filename, new_buffer);
	IOSIM_attach(fd, new_buffer);
	return fd;
}

int bind(int socket, const struct sockaddr *address,
				 socklen_t address_len) {
	
	return 0;
}

int listen(int socket, int backlog) {
	return 0;
}

int getsockname(int __fd, struct sockaddr *__addr, socklen_t * __restrict  __len ) {
	char fakeName[] = "somesockname";
	strcpy(__addr->sa_data, fakeName);
	__addr->sa_family = AF_INET;
	*__len = sizeof(__addr->sa_family) + strlen(fakeName) + 1;
	return 0;
}

int connect(int socket, const struct sockaddr *address,
						socklen_t address_len) {
	return 0;
}

int accept(int socket, struct sockaddr *restrict address,
					 socklen_t *restrict address_len) {
	address->sa_family = AF_INET;

	address->sa_data[0] = __SYMBOLIC();
	address->sa_data[1] = __SYMBOLIC();

	// Here's a fake IP address
	address->sa_data[2] = 10;
	address->sa_data[3] = 20;
	address->sa_data[4] = 30;
	address->sa_data[5] = 40;
	*address_len = 8;
	return 0;
}

int socketpair(int domain, int type, int protocol, int socket_vector[2]) {
	socket_vector[0] = socket(domain, type, protocol);
	socket_vector[1] = IOSIM_newfd();
	if (socket_vector[0] < 0 || socket_vector[1] < 0) {
		return -1;
	}
	IOSIM_attach(socket_vector[1],IOSIM_fd[socket_vector[0]]);
	return 0;
}

int select(int nfds, fd_set *restrict readfds,
					 fd_set *restrict writefds, fd_set *restrict errorfds,
					 struct timeval *restrict timeout) {
	return 1;
}
