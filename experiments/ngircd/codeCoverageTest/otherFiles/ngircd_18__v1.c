// try to set pipe's fildes[0]
#include<arpa/inet.h>
extern int pipe_fildes0;
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK nick\r\n",t++);
	event_recv(client_fd1,"USER user x x :user\r\n",t++);

	event_end(t++);
	{
	char buf[sizeof(struct sockaddr)];
	struct sockaddr* address = buf;
	address->sa_family = AF_INET;
	address->sa_data[0] = 0;//__SYMBOLIC(0);
	address->sa_data[1] = 0;//__SYMBOLIC(0);
	address->sa_data[2] = 10;
	address->sa_data[3] = 20;
	address->sa_data[4] = 30;
	address->sa_data[5] = 40;
	event_recv(pipe_fildes0,buf,0);
	}
}

