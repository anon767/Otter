#include <otter/otter_builtins.h>
#include <netinet/in.h>
#include <arpa/inet.h>

void client_main()
{
	int fd = socket(AF_INET, SOCK_STREAM, 0);
	__ASSERT(fd >= 0);
	struct sockaddr_in* addr = calloc(sizeof(struct sockaddr_in), 1);
	addr->sin_family = AF_INET;
	addr->sin_port = htons(6379);
	addr->sin_addr.s_addr = htonl(0x7f000001);
	int r = connect(fd, addr, sizeof(struct sockaddr_in));
	__ASSERT(r != -1);
#ifndef LEN
#define LEN 100
#endif
	char buf[LEN];
	__SYMBOLIC(&buf);
	write(fd, buf, LEN);
        char outbuf[LEN*10];
        int i;
        for (i = 0; i < 10; i++) {
          read(fd, outbuf, LEN*10);
          __EVALSTR(outbuf, LEN*10);
        }
}
