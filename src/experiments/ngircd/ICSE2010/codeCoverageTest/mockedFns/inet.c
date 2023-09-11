#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

char* inet_nextint(char* s,char delim,int* n){
	char* p = s;
	while( *p!=0 && *p!=delim ) p++;
	int flag = (*p==0);
	*p = 0;
	*n = atoi(s);
	if(flag) return 0;
	else return p+1;
}
//#if __HAVE_inet_aton__
int inet_aton(const char *cp, struct in_addr *pin){
	int r = 0,n;
	char cp2[1024];
	strcpy(cp2,cp);
	char* s = cp2;
	while( (s=inet_nextint(s,'.',&n))!=0){
		// SYMTEST: will there be overflow??
		r = r*256+n;
	}
	pin->s_addr = r;
	//SYMTEST
	//__EVAL(cp);
	//__EVAL(r);
	//exit(1);
	return 1;
}
//#endif

//#if __HAVE_inet_ntoa__
char *inet_ntoa(struct in_addr in){
	//long n = in.s_addr;
	//int ip[4];
	//for(int i=0;i<4;i++){
	//	ip[i] = n%256;
	//	n /=256;
	//}
	//sprxxxf(inet_ntoa_return,"%d.%d.%d.%d",ip[3],ip[2],ip[1],ip[0]);
	return strdup("44.33.22.11");
}
//#endif
//char *inet_ntoa(struct in_add in) {
//	return "1.2.3.4";
//}

const char *inet_ntop(int af, const void *restrict src,
											char *restrict dst, socklen_t size) {
	strcpy(dst, "11.22.33.44");
	return dst;
}
