#include<string.h>
#include<stdlib.h>
void main(){

	char s[100];
	int n = __SYMBOLIC();
	__ASSUME(n==1);
	sprintf(s,"%d",n);
	__ASSERT(s[0]=='0');
	//__ASSERT(n==atoi(s));

}

