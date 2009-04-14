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
	int fd = IOSIM_newfd();
	// reading stream
	IOSIM_fd[fd] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[fd]->fd_type = IOSIM_FD_SOCK;
	IOSIM_fd[fd]->fd = fd;
	IOSIM_fd[fd]->offset = 0;
	IOSIM_fd[fd]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[fd]->offsetout = 0;
	IOSIM_fd[fd]->sym_fileout = malloc(sizeof(sym_file_t));
	return fd;
}

int bind(int socket, const struct sockaddr *address,
				 socklen_t address_len) {
	
	return 0;
}

int listen_queue[100];
int listen_queue_size;
int listen(int socket, int backlog) {
	IOSIM_fd[socket]->fd_type = IOSIM_FD_SSOCK;
	IOSIM_fd[socket]->sym_file->contents  = listen_queue;
	//listen_ssock_size = &(IOSIM_fd[socket]->sym_file->stat.st_size);
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

	sym_file_stream_t* sock = IOSIM_fd[sockfd];
	if(sock->fd_type!=IOSIM_FD_SSOCK ||
	   sock->offset>=sock->sym_file->stat.st_size){
		__COMMENT("Error in accept(). Either a non-ssock is passed to accept(), or there's no socket available to be accepted (accept() called too early).");
		exit(1);
	}
	int fd = ((int*)sock->sym_file->contents)[sock->offset];
	sock->offset++;

	address->sa_family = AF_INET;

//	address->sa_data[0] = __SYMBOLIC(0);
//	address->sa_data[1] = __SYMBOLIC(0);

	// Here's a fake IP address
	address->sa_data[2] = 10;
	address->sa_data[3] = 20;
	address->sa_data[4] = 30;
	address->sa_data[5] = 40;
//	*address_len = 8;

	return fd;
	//return accept_fd[fd];
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

int select(int nfds, fd_set *restrict readfds,
					 fd_set *restrict writefds, fd_set *restrict errorfds,
					 struct timeval *restrict timeout) {
	return 1;
}
