#include <otter/otter_builtins.h>
#include <netinet/in.h>

void client_main()
{
	int fd = socket(AF_INET, SOCK_STREAM, 0);
	__ASSERT(fd >= 0);
	struct sockaddr_in* addr = calloc(sizeof(struct sockaddr_in), 1);
	addr->sin_family = AF_INET;
	addr->sin_port = 6379;
	addr->sin_addr.s_addr = 0x7F000001;
	int r = connect(fd, addr, sizeof(struct sockaddr_in));
	__ASSERT(r != -1);
#define LEN 100
	char buf[LEN];
	__SYMBOLIC(&buf);
	write(fd, buf, LEN);
	read(fd, buf, LEN);
	__EVALSTR(buf, LEN);
}
