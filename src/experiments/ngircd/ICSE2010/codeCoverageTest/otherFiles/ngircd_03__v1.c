// print invite/ban lists
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK whatever\r\n",t++);
	event_recv(client_fd1,"USER whatever x x :whatever\r\n",t++);
	event_recv(client_fd1,"JOIN #ch\r\n",t++);
	event_recv(client_fd1,"MODE #ch +imI *!*@*.fi\r\n",t++);
	event_recv(client_fd1,"MODE #ch -s\r\n",t++);
	event_recv(client_fd1,"MODE #ch +l 10\r\n",t++);

	event_end(t++);
}

