// KICK
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK nick1\r\n",t++);
	event_recv(client_fd1,"USER user x x :user\r\n",t++);

	event_recv(client_fd1,"JOIN #ch\r\n",t++);
	event_recv(client_fd1,"KICK #ch nick2 :I'm loving it\r\n",t++);

	event_end(t++);
}

