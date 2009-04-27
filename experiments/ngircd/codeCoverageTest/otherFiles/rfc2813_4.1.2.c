// server message  
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"PASS PASSWORD 0210 IRC|aBgH$ Z\r\n",t++);
	event_recv(client_fd1,"SERVER test.oulu.fi 1 1 :Experimental server\r\n",t++);
	event_recv(client_fd1,"NICK syrk 5 kalt millennium.stealth.net 34 +i :Christophe Kalt\r\n",t++);
	event_recv(client_fd1,"SERVICE dict@irc.fr 9 *.fr 0 1 :French Dictionary\r\n",t++);

	event_send(client_fd1,t++);

	event_end(t++);
}

