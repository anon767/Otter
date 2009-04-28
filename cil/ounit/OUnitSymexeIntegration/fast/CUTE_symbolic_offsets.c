// from EXE paper, related work about CUTE

#include<stdlib.h>
int main(){
	int a[100];
	int i,j; //symbolic
	__ASSUME(i>=0,i<100);
	__ASSUME(j>=0,j<100);
	
	memset(a,0xff,100*sizeof(int));

	a[i] = 0;
	a[j] = 1;

	__ASSERT(__TRUTH_VALUE(a[i]==0)==0);
	
	__ASSUME(i!=j);
	__ASSERT(__TRUTH_VALUE(a[i]==0)==1);

	return 0;
}
		
