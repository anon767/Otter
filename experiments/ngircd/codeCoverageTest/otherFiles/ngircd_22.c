// ERROR
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);
	int client_fd2 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"PASS password 0210-IRC+ IRC|aBgH$ Z\r\n",t++);
	event_recv(client_fd1,"SERVER irc2.the.net  1 :Experimental server\r\n",t++);

	event_accept(client_fd2,t++);
	event_recv(client_fd2,"NICK whatever\r\n",t++);
	event_recv(client_fd2,"USER whatever x x :whatever\r\n",t++);

	event_recv(client_fd1,":irc2.the.net 300 whatever :hello\r\n",t++);

	event_end(t++);
}

