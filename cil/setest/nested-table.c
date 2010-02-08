// ./run setest/nested-table.c 

#define MAX 300
int f[MAX];

void main(){

	int k;
	int i = __SYMBOLIC();
	__ASSUME(i>=0,i<MAX);

	for(k=0;k<MAX;++k)
		f[k]=k;

	__ASSERT(f[(i+(-1))*4+0]==0);
	//__ASSERT(f[f[f[f[i]]]] >=0);
}
