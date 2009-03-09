#define main main_ngircd
#include "ngircd-0.12.0-set.c"
#undef main

REQUEST* make_part_request(){
	REQUEST *request = malloc(sizeof(REQUEST));
	request->prefix = "";
	request->command = "PART";
	request->argv[0] = malloc(10);
	__SYMBOLIC_STRING(request->argv[0]);
	request->argc = 1;
	return request;
}

int main(){
	/* initialization */
	__SET_INIT(&My_Clients,Client_Symbolic(),Client_Clone);
	Channel_Init();

	CLIENT *client;
	__SET_FOREACH(&client,&My_Clients);

	REQUEST *request = make_part_request();

	// Channels: C
	IRC_PART(client,request);

	return 0;
}
