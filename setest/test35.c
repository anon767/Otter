#include<string.h>

int main(){

	int x;
	__ASSUME(x>=0,x<=99999);
	char s[26];
	sprintf(s,"%d",x);
	__ASSERT(strlen(s)<=5);

	return 0;
}
