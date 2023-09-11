#define MAX 100
#define I (__ITE(0<j&&j<i,s[j]!=0,1))   // wrong invariant
//#define I (__ITE(0<=j&&j<i,s[j]!=0,1))  // Right invariant

void main(){

	int i;
	char s[MAX];

	// init s
	__SYMBOLIC(&s);
	
	
	i=0;
	int j = __SYMBOLIC();

	// I = forall j, 0<=j<i -> s[j]!=0
	
	__ASSERT(I);
	
	__SYMBOLIC_STATE(); // i,j,s[] have fresh symbolic values

	__ASSUME(I);
	
	// The original loop:
	// while(s[i]!=0)
	// 	i++;
	
	if (s[i]!=0)
	{
		i++;
		__ASSERT(I);
	}
	else
	{
		// assert Q:
		__ASSERT(__ITE(0<=j&&j<i,s[j]!=0,1),s[i]==0);
	}
}
