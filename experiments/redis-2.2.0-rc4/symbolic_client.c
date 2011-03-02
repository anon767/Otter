#include <otter/otter_builtins.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#ifndef LEN
#define LEN 100
#endif

void client_main()
{
	int fd = socket(AF_INET, SOCK_STREAM, 0);
	__ASSERT(fd >= 0);
	struct sockaddr_in* addr = calloc(sizeof(struct sockaddr_in), 1);
	addr->sin_family = AF_INET;
	addr->sin_port = htons(PORT_NUMBER);
	addr->sin_addr.s_addr = htonl(0x7f000001);
	int r = connect(fd, addr, sizeof(struct sockaddr_in));
	__ASSERT(r != -1);
	char server_input[LEN];
	__SYMBOLIC(&server_input);
	write(fd, server_input, LEN);
	char server_output[100];
	/* As long as the server is generating data, this read will block
	waiting for it. When the server is done processing all of the input,
	this will terminate with "`Failure:Deadlock" */
	while (1) {
		int read_length = read(fd, server_output, sizeof(server_output));
		if (read_length == -1) return; // The server closed the socket
		__EVALSTR(server_output, read_length);
	}
}
