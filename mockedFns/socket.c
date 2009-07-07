#include <sys/socket.h>
#include "iosim.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define DEFAULT_BUF_SIZE 1024

int setsockopt(int socket, int level, int option_name,
							 const void *option_value, socklen_t option_len) {
	return 0;
}

int socket(int domain, int type, int protocol) {
	return IOSIM_newfd();
//#define DEFAULT_BUF_SIZE 1024
//	IO_BUF* new_buffer = IOSIM_newbuf(DEFAULT_BUF_SIZE,malloc(DEFAULT_BUF_SIZE));
//	int fd = IOSIM_newfd();
//	char *filename = malloc(18); // 18 == strlen("socket_at_fd_9999*"). This should allow fds up to 9999
//	if (sprintf(filename,"socket_at_fd_%d",fd) < 0) {
//		__COMMENT("Problem creating socket");
//		exit(1);
//	}
//	IOSIM_addfile(filename, new_buffer);
//	IOSIM_attach(fd, new_buffer);
//	return fd;
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

int accept(int sockfd, struct sockaddr *restrict address,
					 socklen_t *restrict address_len) {
	address->sa_family = AF_INET;

//	address->sa_data[0] = __SYMBOLIC(0);
//	address->sa_data[1] = __SYMBOLIC(0);

	// Here's a fake IP address
	address->sa_data[2] = 10;
	address->sa_data[3] = 20;
	address->sa_data[4] = 30;
	address->sa_data[5] = 40;
//	*address_len = 8;

	return IOSIM_newfd();
}

int socketpair(int domain, int type, int protocol, int socket_vector[2]) {
	// Make a new stream and fd for a new socket
	int fd0 = IOSIM_newfd(); // socket(domain, type, protocol);
	IOSIM_fd[fd0] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[fd0]->offset = 0;
	// Make a new socket
	IOSIM_fd[fd0]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[fd0]->sym_file->contents = NULL;
	IOSIM_fd[fd0]->sym_file->stat.st_size = 0;
	IOSIM_fd[fd0]->sym_file->stat.st_mode = S_IFSOCK;

	// Make a second stream and fd, and associate them with the *same* socket
	int fd1 = IOSIM_newfd();
	IOSIM_fd[fd1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[fd1]->offset = 0;
	IOSIM_fd[fd1]->sym_file = IOSIM_fd[fd0]->sym_file;

	socket_vector[0] = fd0;
	socket_vector[1] = fd1;

	return 0;
}

int select(int nfds, fd_set *restrict readfds, fd_set *restrict writefds,
		fd_set *restrict errorfds, struct timeval *restrict timeout)
{
	return 1;
}
