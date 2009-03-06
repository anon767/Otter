#define main main_ngircd
#include "ngircd-0.12.0-set.c"
#undef main



void* __PTR_SYMBOLIC(int n){
	char* p = malloc(n);
	for(int i=0;i<n;i++)
		p[i] = __SYMBOLIC();
	return p;
}

char* __SYMBOLIC_STR(){
	char* s = malloc(2);
	s[0] = __SYMBOLIC();
	__ASSUME(s[0]!='\0');
	__ASSUME(s[0]!='0');
	s[1] = '\0';
	return s;
}

//CLIENT* make_concrete_client(){
//	// defined in client.c
//	return Init_New_Client(
//		1,
//		0,
//		0,
//		16,
//		"id",
//		"user",
//		"hostname",
//		"info",
//		0,
//		0,
//		"",
//		1);
//}
CLIENT* make_symbolic_client(){
	// defined in client.c:
	return Init_New_Client(
		/* CONN_ID Idx */        __SYMBOLIC(sizeof(CONN_ID)),	
		/* CLIENT *Introducer */ 0,
		/* CLIENT *TopServer*/   0,
		/* int Type*/            16/*CLIENT_USER*/,
		/* char *ID*/            __SYMBOLIC_STR(),
		/* char *User*/          __SYMBOLIC_STR(),
		/* char *Hostname*/      __SYMBOLIC_STR(),
		/* char *Info*/          __SYMBOLIC_STR(),
		/* int Hops*/            __SYMBOLIC(sizeof(int)),
		/* int Token*/           __SYMBOLIC(sizeof(int)),
		/* char *Modes*/         __SYMBOLIC_STR(),
		/* bool Idented*/        1
		);
}

REQUEST* make_join_request(){
	REQUEST *request = malloc(sizeof(REQUEST));
	request->prefix = "";
	request->command = "JOIN";
	request->argv[0] = __SYMBOLIC_STR();
	request->argc = 1;
	return request;
}


int main(){

	/* initialization */
	//CLIENT *client = make_symbolic_client();
	
	// defined in client.c
	//This_Server = make_concrete_client();
	My_Clients = make_symbolic_client();
	CLIENT *client = My_Clients;

	REQUEST *request = make_join_request();

	IRC_JOIN(client,request);

	return 0;
}
