int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);
	int client_fd2 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK martin\r\n",t++);
	event_recv(client_fd1,"USER kkma x x :kin keung ma\r\n",t++);
	event_send(client_fd1,t++);
	event_accept(client_fd2,t++);
	event_recv(client_fd2,"NICK jeff\r\n",t++);
	event_recv(client_fd2,"USER jfoster x x :jeff foster\r\n",t++);
	event_send(client_fd2,t++);
	event_recv(client_fd1,"JOIN #channel\r\n",t++);
	event_recv(client_fd1,"MODE #channel +im\r\n",t++);
	event_recv(client_fd2,"JOIN #channel\r\n",t++);
	event_recv(client_fd1,"INVITE jeff #channel\r\n",t++);
	event_recv(client_fd2,"JOIN #channel\r\n",t++);
	event_recv(client_fd1,"PRIVMSG #channel :hello\r\n",t++);
	event_recv(client_fd2,"PART #channel\r\n",t++);
	event_recv(client_fd1,"NAMES\r\n",t++);
	event_recv(client_fd1,"LIST\r\n",t++);
	event_recv(client_fd2,"WHOIS martin\r\n",t++);
	event_recv(client_fd2,"STATS m\r\n",t++);
	event_recv(client_fd2,"AWAY :Gone to lunch\r\n",t++);
	event_recv(client_fd2,"QUIT\r\n",t++);
	event_recv(client_fd1,"QUIT\r\n",t++);
	event_end(t++);
}

