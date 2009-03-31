#include <symtest_mock.h>
#include <netdb.h>
#include <stdlib.h>
#include <string.h>

struct hostent gethostbyname_return;
#if __HAVE_gethostbyname__
struct hostent *gethostbyname(const char *name){
	if(strcmp(name,SERVER_IP)==0){
		gethostbyname_return.h_name = SERVER_NAME;
		//gethostbyname_return->h_addr_list = malloc(4);
		//gethostbyname_return->h_addr_list[0] = gethostbyname_return_h_addr;
		return &gethostbyname_return;
	}
	else{
		exit(1);
	}
}
#endif

#if __HAVE_gethostbyaddr__
struct hostent *gethostbyaddr(const void *addr,  socklen_t len, int type){
	return &gethostbyname_return;
}
#endif

