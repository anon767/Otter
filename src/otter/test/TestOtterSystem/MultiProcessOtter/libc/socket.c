#pragma no_other_abandoned

#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>

int main()
{
	__otter_libc_init();
	
	int* flag = __otter_multi_gmalloc(sizeof(int));
	*flag = 0;
	
	if(fork())
	{
		int fd = socket(AF_INET, SOCK_STREAM, 0);
		__ASSERT(fd > 0);
		
		struct sockaddr_in* addr = calloc(sizeof(struct sockaddr_in), 1);
		addr->sin_family = AF_INET;
		addr->sin_port = 5000;
		addr->sin_addr.s_addr = 0x7F000001;
		int r = bind(fd, addr, sizeof(struct sockaddr_in));
		__ASSERT(r != -1);
		r = listen(fd, 5);
		__ASSERT(r != -1);
			
		*flag = 1;
		__otter_multi_end_atomic();
		
		int sock = accept(fd, NULL, 0);
		
		char buf[] = "abcdefghi";
		r = write(sock, buf, 9);
		__ASSERT(r == 9);
		
		int off = 0;
		do
		{
			r = read(sock, buf + off, 9 - off);
			__ASSERT(r != -1);
			off += r;
		}
		while(off < 9);
		
		int cmp = strcmp(buf, "123456789");
		__ASSERT(cmp == 0);
		
		r = close(sock);
		__ASSERT(r != -1);
		r = close(fd);
		__ASSERT(r != -1);
	}
	else
	{
		__otter_multi_begin_atomic();
		while(*flag == 0)
		{
			__otter_multi_io_block(flag);
			__otter_multi_begin_atomic();
		}
		__otter_multi_end_atomic();
		
		int fd = socket(AF_INET, SOCK_STREAM, 0);
		__ASSERT(fd > 0);
		struct sockaddr_in* addr = calloc(sizeof(struct sockaddr_in), 1);
		addr->sin_family = AF_INET;
		addr->sin_port = 5000;
		addr->sin_addr.s_addr = 0x7F000001;
		int r = connect(fd, addr, sizeof(struct sockaddr_in));
		__ASSERT(r != -1);
		
		char buf[] = "123456789";
		r = write(fd, buf, 9);
		__ASSERT(r == 9);
		
		int off = 0;
		do
		{
			r = read(fd, buf + off, 9 - off);
			__ASSERT(r != -1);
			off += r;
		}
		while(off < 9);
		
		int cmp = strcmp(buf, "abcdefghi");
		__ASSERT(cmp == 0);
		
		r = close(fd);
		__ASSERT(r != -1);
	}
	
	return (0);
}
