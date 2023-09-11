#include <netinet/in.h>
#include <arpa/inet.h>

int client_main()
{
	int fd = socket(AF_INET, SOCK_STREAM, 0);
	__ASSERT(fd >= 0);
	struct sockaddr_in* addr = calloc(sizeof(struct sockaddr_in), 1);
	addr->sin_family = AF_INET;
	addr->sin_port = htons(21);
	addr->sin_addr.s_addr = htonl(0x7F000001);
	int r = connect(fd, addr, sizeof(struct sockaddr_in));
	__ASSERT(r != -1);
	char buf[20];
	for(int i = 0; i < 5; i++)
	{
		for(int j = 0; j < 20; j++)
			buf[j] = __SYMBOLIC(1);
		write(fd, buf, 20);
		read(fd, buf, 20);
	}
	
	return(0);
}
