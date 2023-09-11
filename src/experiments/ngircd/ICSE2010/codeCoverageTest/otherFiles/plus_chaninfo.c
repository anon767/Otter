// server message  (CHANINFO)
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"PASS password 0210-IRC+ IRC|aBgH$ Z\r\n",t++);
	event_recv(client_fd1,"SERVER irc2.the.net  1 :Experimental server\r\n",t++);
	// BUS ERROR!
	//event_recv(client_fd1,"CHANINFO #ch1 + :topic\r\n",t++);
	event_recv(client_fd1,"CHANINFO #ch1 +im * 0 :topic\r\n",t++);

	event_send(client_fd1,t++);

	event_end(t++);
}

