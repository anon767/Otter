//  MOTD
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"MOTD\r\n",t++);

	event_end(t++);
}

