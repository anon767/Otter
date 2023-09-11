#include <otter/otter_builtins.h>
#include <netinet/in.h>
#include <arpa/inet.h>

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
  /* Fork to have one process write symbolic data and the other read responses
     (to keep the receiving buffer clear) */
  if (fork()) {
      char server_input[100];
      __SYMBOLIC(&server_input);
      int write_length = write(fd, server_input, sizeof(server_input));
      /* Loop until the server closes the socket. At some point, the write will
         fill the buffer and block until the server reads more data. So this
         isn't really a busy loop. */
      while (write_length >= 1) {
          __SYMBOLIC(&server_input);
          write_length = write(fd, server_input, sizeof(server_input));
      }
  } else {
      char server_output[100];
      /* Block until the server has written some data, then read the data. Do
         this until the server closes the socket. */
      int read_length = read(fd, server_output, sizeof(server_output));
      while (read_length >= 1) {
          __EVALSTR(server_output, read_length);
          read_length = read(fd, server_output, sizeof(server_output));
      }
  }
}
