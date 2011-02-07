// from EXE paper, related work about CUTE

#pragma no_other_abandoned

typedef unsigned long size_t;
void * memset(void *b, int c, size_t len);

int main(){
	int a[100];
	int i,j; //symbolic
	__SYMBOLIC(&i);
	__SYMBOLIC(&j);
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
		
