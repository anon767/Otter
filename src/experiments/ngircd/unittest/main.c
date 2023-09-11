#define main main_ngircd
#include "ngircd-0.12.0-set.c"
#undef main

REQUEST* make_privmsg_request(){
	REQUEST *request = malloc(sizeof(REQUEST));
	request->prefix = "";
	request->command = "PRIVMSG";

	request->argv[0] = malloc(10);
	request->argv[1] = malloc(10);
	__SYMBOLIC_STRING(request->argv[0]);
	__SYMBOLIC_STRING(request->argv[1]);
	request->argc = 2;
	return request;
}

int main(){
	/* initialization */
	Client_Init();
	Channel_Init();

	CLIENT *client;
	__SET_FOREACH(&client,&My_Clients);

	REQUEST *request = make_privmsg_request();

	IRC_PRIVMSG(client,request);

	return 0;
}
