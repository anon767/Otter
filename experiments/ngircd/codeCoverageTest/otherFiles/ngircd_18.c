// try to set pipe's fildes[0]
#include<sys/socket.h>

extern int (*pipe_fildes0_callback)(int);

int pipe_fildes0_callback_imp(int fd){
	char buf[sizeof(struct sockaddr)];
	struct sockaddr* address = buf;
	memset(address,0,sizeof(struct sockaddr));
	address->sa_family = AF_INET;
	address->sa_data[0] = 0;//__SYMBOLIC(0);
	address->sa_data[1] = 0;//__SYMBOLIC(0);
	address->sa_data[2] = 10;
	address->sa_data[3] = 20;
	address->sa_data[4] = 30;
	address->sa_data[5] = 40;
	event_recv(fd,buf,0);
}
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);
	//pipe_fildes0 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK nick\r\n",t++);
	event_recv(client_fd1,"USER user x x :user\r\n",t++);

	event_end(t++);
	pipe_fildes0_callback = pipe_fildes0_callback_imp;
}
