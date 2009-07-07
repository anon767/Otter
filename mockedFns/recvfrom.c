#include <sys/socket.h>

ssize_t recvfrom (int sockfd, void *buffer, size_t len, int flags,
	__SOCKADDR_ARG to, socklen_t *tolen)
{
	return recv(sockfd, buffer, len, flags);
}
