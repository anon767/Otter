#define MAX	10
#include<string.h>

void stuff_symbolic(char* s1){
	for (int i=0;i<MAX-1;i++) s1[i]=__SYMBOLIC();
	s1[MAX-1]='\0';
}

int strcmp(const char *s1, const char *s2)
{
	const unsigned char *c1 = (const unsigned char *)s1;
	const unsigned char *c2 = (const unsigned char *)s2;
	unsigned char ch;
	int d = 0;

	while (1) {
		d = (int)(ch = *c1++) - (int)*c2++;
		if (d || !ch)
			break;
	}

	return d;
}

void main(){

	char s1[MAX],s2[MAX];
	stuff_symbolic(s1);
	stuff_symbolic(s2);
	//__EVAL(strcmp(s1,s2));
	//__PATHCONDITION();
	__ASSUME(strcmp(s1,s2)>0);
	__ASSERT(strcmp(s2,s1)<0);
	
}
