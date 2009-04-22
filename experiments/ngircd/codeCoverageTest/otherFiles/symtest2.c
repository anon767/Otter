// remote server
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);
	int client_fd2 = socket(0,0,0);
	int client_fd3 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"SERVER test.oulu.fi 1 :[tolsun.oulu.fi] Experimental server\r\n",t++);
	event_send(client_fd1,t++);

	event_accept(client_fd2,t++);
	event_recv(client_fd2,"NICK elnatan\r\n",t++);
	event_recv(client_fd2,"USER reisner x x :elnatan b reisner\r\n",t++);
	event_send(client_fd2,t++);

	event_accept(client_fd3,t++);
	event_recv(client_fd3,"NICK charles\r\n",t++);
	event_recv(client_fd3,"USER csfalson x x :charles song\r\n",t++);
	event_send(client_fd3,t++);

	event_end(t++);
}

