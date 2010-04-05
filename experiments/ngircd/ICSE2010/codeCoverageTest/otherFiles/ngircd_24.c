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
	;MotdPhrase = \"Hello world!\"
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
[Operator]
	Name = TheOper
	Password = ThePwd
	Mask = *
[Operator]
[Server]
	Name = irc2.the.net
	Host = connect-to-host.the.net
	Bind = 10.0.0.1
	Port = 6667
	MyPassword = password
	PeerPassword = PeerSecret
	Group = 123
	Passive = no
[Server]
	Name = irc3.the.net
	Host = connect-to-host3.the.net
	Bind = 10.0.0.1
	Port = 6667
	MyPassword = password
	PeerPassword = PeerSecret
	Group = 123
	Passive = no
[Server]
[Channel]
	Name = #TheName
	Topic = a great topic
	Modes = tnk
	Key = Secret
	MaxUsers = 23
[Channel]
	Name = TheName oh 
	Topic = a great topic
	Modes = tnk
	Key = Secret
	MaxUsers = 23
[Channel]";
	confString_return  = confStr;
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);
	int client_fd2 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"PASS password 0210-IRC+ IRC|aBgH$ Z\r\n",t++);
	event_recv(client_fd1,"SERVER irc2.the.net  1 :Experimental server\r\n",t++);

	event_accept(client_fd2,t++);
	event_recv(client_fd2,"PASS password 0210-IRC+ IRC|aBgH$ Z\r\n",t++);
	event_recv(client_fd2,"SERVER irc3.the.net  1 :Experimental server3\r\n",t++);
	event_recv(client_fd2,":irc3.the.net PING :irc2.the.net\r\n",t++);

	event_end(t++);
}

