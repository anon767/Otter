// ERROR
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK nick1\r\n",t++);
	event_recv(client_fd1,"USER user x x :user\r\n",t++);
	//event_recv(client_fd1,"PASS password 0210-IRC+ IRC|aBgH$ Z\r\n",t++);
	//event_recv(client_fd1,"SERVER irc2.the.net  1 :Experimental server\r\n",t++);
	event_recv(client_fd1,"ERROR :error msg\r\n",t++);

	event_end(t++);
}

