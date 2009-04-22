// Login
int symtest(){
	
	int t;

	// setup client
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK martin\r\n",t++);
	event_recv(client_fd1,"USER kkma x x :kin keung ma\r\n",t++);
	event_send(client_fd1,t++);

	event_end(t++);

	return 0;
}

