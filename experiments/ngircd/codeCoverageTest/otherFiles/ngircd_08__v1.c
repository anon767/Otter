int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);
	int client_fd2 = socket(0,0,0);

	t = 0;
		

	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK nick1\r\n",t++);
	event_recv(client_fd1,"USER user x x :user\r\n",t++);
	event_recv(client_fd1,"OPER TheOperx ThePwd\r\n",t++);
	event_recv(client_fd1,"OPER TheOper ThePwdx\r\n",t++);
	event_recv(client_fd1,"OPER TheOper ThePwd\r\n",t++);

	event_end(t++);
}

