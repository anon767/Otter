// INVITE
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);
	int client_fd2 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK nick1\r\n",t++);
	event_recv(client_fd1,"USER user x x :user\r\n",t++);
	event_accept(client_fd2,t++);
	event_recv(client_fd2,"NICK nick2\r\n",t++);
	event_recv(client_fd2,"USER user2 x x :user\r\n",t++);

	event_recv(client_fd1,"INVITE nick2 #ch\r\n",t++);

	event_end(t++);
}

