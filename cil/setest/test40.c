#define MAX	100
#include<string.h>

void stuff_symbolic(char* s1){
	for (int i=0;i<MAX-1;i++) s1[i]=__SYMBOLIC();
	s1[MAX-1]='\0';
}

int mystrcmp(char* const s1,char* const s2){
	int exp = __SYMBOLIC();
	for(int i=MAX-1;i>=0;i--){
		exp = __ITE(s1[i]!=s2[i],s1[i]-s2[i],__ITE(s1[i]==0,0,exp));
	}
	return exp;
}

void main(){

	char s1[MAX],s2[MAX];
	stuff_symbolic(s1);
	stuff_symbolic(s2);
	//__EVAL(strcmp(s1,s2));
	//__PATHCONDITION();
	__ASSUME(mystrcmp(s1,s2)>0);
	__ASSERT(mystrcmp(s2,s1)<0);
	
}
