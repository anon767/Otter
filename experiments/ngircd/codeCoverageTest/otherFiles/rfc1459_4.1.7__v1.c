// server quit  (TODO: make successful establishment)
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"SERVER test.oulu.fi 1 :[tolsun.oulu.fi] Experimental server\r\n",t++);
	event_recv(client_fd1,"QUIT tolsun.oulu.fi :Bad Link ?\r\n",t++);

	event_end(t++);
}

