// RESTART, with connected server
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);
	int client_fd2 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd2,t++);
	event_recv(client_fd2,"PASS password 0210-IRC+ IRC|aBgH$ Z\r\n",t++);
	event_recv(client_fd2,"SERVER irc2.the.net  1 :Experimental server\r\n",t++);
	event_accept(client_fd2,t++);

	event_recv(client_fd1,"NICK nick1\r\n",t++);
	event_recv(client_fd1,"USER user x x :user\r\n",t++);
	event_send(client_fd1,t++);
	event_recv(client_fd1,"OPER TheOper ThePwd\r\n",t++);
	event_send(client_fd1,t++);
	
	event_recv(client_fd1,"RESTART\r\n",t++);
	event_send(client_fd1,t++);

	event_end(t++);
}

