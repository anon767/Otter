/*
Note that this message accepts a special argument ("0"), which is
   a special request to leave all channels the user is currently a member
   of.  The server will process this message as if the user had sent
   a PART command (See Section 3.2.2) for each channel he is a member
   of.
*/
int symtest(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK nick1\r\n",t++);
	event_recv(client_fd1,"USER user1 x x :user1\r\n",t++);
	event_send(client_fd1,t++);

	event_recv(client_fd1,"JOIN #ch\r\n",t++);
	event_recv(client_fd1,"JOIN 0\r\n",t++);


	event_end(t++);
}

