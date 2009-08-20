// ERROR
extern char* confString_return;

int symtest(){
	static char confStr[] = "[Global]
	Name = irc.the.net
	Info = Server Info Text
	;Password = abc
	;AdminInfo1 = Description
	;AdminInfo2 = Location
	;AdminEMail = admin@irc.server
	Ports = 6667
	;Listen = 1.2.3.4
	MotdFile = /usr/local/etc/ngircd.motd
	MotdPhrase = \"Hello world!\"
	;ServerUID = 65534
	;ServerGID = 65534
	;ChrootDir = /var/empty
	;PidFile = /var/run/ngircd/ngircd.pid
	PingTimeout = 1200
	PongTimeout = 200
	;ConnectRetry = 60
	;OperCanUseMode = no
	;OperServerMode = no
	;PredefChannelsOnly = no
	;NoDNS = no
	;ListenIPv6 = yes
	;ListenIPv4 = yes
	;ConnectIPv6 = yes
	;ConnectIPv4 = yes
	MaxConnections = 0
	MaxConnectionsIP = 0
	;MaxJoins = 10
	MaxNickLength = 9
[Channel]";
	confString_return  = confStr;
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK whatever\r\n",t++);
	event_recv(client_fd1,"USER whatever x x :whatever\r\n",t++);

	event_end(t++);
}

