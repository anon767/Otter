#include<string.h>


void main(){

	char s[100];
	int n=0;
	__SYMBOLIC(&n);
	__ASSUME(n>=0,n<=100000);

	sprintf(s,"%d",n);
	__ASSERT(strlen(s)<=6);
}
