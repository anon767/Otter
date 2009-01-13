#include<string.h>

int main(){


	char s[26];

	sprintf(s,"%d",365);
	__ASSERT(s[0]=='3',s[1]=='6',s[2]=='5');

	return 0;
}
