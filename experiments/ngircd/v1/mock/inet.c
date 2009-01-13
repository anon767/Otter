#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdlib.h>

char* inet_nextint(char* s,char delim,int* n){
	char* p = s;
	while( *p!=0 && *p!=delim ) p++;
	int flag = (*p==0);
	*p = 0;
	*n = atoi(s);
	if(flag) return 0;
	else return p+1;
}
#if __HAVE_inet_aton__
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
#endif

#if __HAVE_inet_ntoa__
extern int __vsnprintf(char *buffer, size_t n, const char *format, va_list ap);
char inet_ntoa_return[20]; // static buffer overwritten with each call
char *inet_ntoa(struct in_addr in){
	long n = in.s_addr;
	int ip[4];
	for(int i=0;i<4;i++){
		ip[i] = n%256;
		n /=256;
	}
	sprintf(inet_ntoa_return,"%d.%d.%d.%d",ip[3],ip[2],ip[1],ip[0]);
	//SYMTEST
	//__EVAL(n);
	//__EVAL(inet_ntoa_return[0]);
	//__EVAL(inet_ntoa_return[1]);
	//exit(1);
	return inet_ntoa_return;
}
#endif
	  
